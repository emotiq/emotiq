;; transport-blocking.lisp

(in-package :transport/blocking)

;;;
;;; TCP transport implementation based on thread-per-connection model.
;;;

(defclass tcp-threads-transport ()
  (;; Operational state.
   (listen-socket :initarg :listen-socket :initform nil :accessor listen-socket
                  :documentation "Listen socket that our peers can connect to.")
   (polling-process :initarg :polling-thread :initform nil :accessor polling-process
                    :documentation "Process dedicated to polling for network input.")
   (readers :initarg :readers :initform nil :accessor readers
            :documentation "List of reader processes.")
   (writers :initarg :writers :initform nil :accessor writers
            :documentation "List of writer processes.")
   (message-queues :initarg :message-queues :initform (make-hash-table :test 'equal)
                   :documentation "Hashtable of (ADDRESS PORT) to MAILBOX.")
   (sockets :initform nil :accessor sockets
            :documentation "List of all open sockets (inbound and outbound.)")
   (listen-process :initarg :listen-process :initform nil :accessor listen-process
                   :documentation "Process listening for inbound connections.")
   (lock :initform (mpcompat:make-lock) :accessor lock
         :documentation "Lock to be held when adding and removing connections.")
   ;; Configuration state.
   (config :initarg :config :accessor config)))

(defmethod start ((backend (eql :tcp)) config)
  (multiple-value-bind (address port) (my-endpoint config)
    (log-event config "Starting transport on" address :PORT port)
    (let* ((listen-socket (usocket:socket-listen address (or port 0)
                                                 :reuse-address t
                                                 :element-type '(unsigned-byte 8)))
           (transport (make-instance 'tcp-threads-transport
                                     :config config
                                     :listen-socket listen-socket)))
      (setf (listen-process transport)
            (mpcompat:process-run-function (format nil "gossip/transport accept on ~A" listen-socket)
                                           nil
                                           (lambda ()
                                             (log-event config "Started listen thread for" transport)
                                             (run-tcp-listen-thread transport))))
      (log-event config "Started transport" transport)
      transport)))

(defmethod stop ((transport tcp-threads-transport))
  ;; XXX detect when already stopped and... what?
  (log-event (config transport) "Stopping transport" transport)
  (with-slots (lock listen-socket listen-process readers writers sockets config) transport
    (mpcompat:with-lock (lock)
      ;; Stop worker processes.
      (mapc #'mpcompat:process-kill (list* listen-process (append readers writers)))
      ;; Stop accepting new connections.
      (ignore-errors (usocket:socket-close listen-socket))
      ;; Close existing connections.
      (dolist (socket sockets)
        (ignore-errors (usocket:socket-close socket)))))
  (log-event (config transport) "Stopped transport" transport))

;;; Listening

(defun run-tcp-listen-thread (transport)
  (with-slots (listen-socket readers lock peer-down-hook sockets config) transport
    (handler-case
        (loop
           (let ((socket (usocket:socket-accept listen-socket)))
             (mpcompat:with-lock (lock) (push socket sockets))
             (let ((process (mpcompat:process-run-function (format nil "gossip/transport reader on ~A" socket)
                                                           nil
                                                           (lambda ()
                                                             (log-event config "Started read thread for" transport)
                                                             (run-tcp-read-thread transport socket)))))
               (push process (readers transport)))))
      (error (err)
        (log-event config "Listen thread error" err)
        ;;; XXX close transport and make errors propagate
        ))))

;;; Reading

(defun run-tcp-read-thread (transport socket)
  "Transfer objects from SOCKET to MESSAGE-RECEIVED-HOOK."
  (let ((stream (usocket:socket-stream socket)))
    (loop for magic across +magic+
          do (assert (= magic (read-byte stream))))
    (handler-case
        (loop (let ((msg (read-message stream)))
                (run-message-received-hook (config transport) msg)
                (log-event (config transport) "Received message from" socket msg)))
      (error (err)
        (log-event (config transport) "Read thread error" err)
        (usocket:socket-close socket)))))

(defun read-message (stream)
  (let* ((length (loop for i from 0 to 4
                    for acc = 0 then (+ (ash acc 8) (read-byte stream))
                    finally (return acc)))
         (buffer (make-array (list length)
                             :element-type '(simple-array (unsigned-byte 8) (*)))))
    (read-sequence buffer stream)
    buffer))


;;; Writing

(defun run-tcp-write-thread (transport address port mailbox)
  "Transfer objects from MAILBOX to the remote endpoint ADDRESS:PORT."
  (with-slots (lock message-queues peer-down-hook sockets config) transport
    (handler-case
        ;; Establish new connection.
        (let* ((socket (connect-to transport address port))
               (stream (usocket:socket-stream socket)))
          (write-sequence +magic+ (usocket:socket-stream socket)) 
          (log-event config "Ready to write to" address :PORT port)
          (mpcompat:with-lock (lock) (push socket sockets))
          ;; Loop transferring messages from mailbox to socket.
          (unwind-protect
               (loop (let ((msg (mpcompat:mailbox-read mailbox)))
                       ;; Send length header.
                       (loop for i from 24 downto 0 by 8
                          do (write-byte (ldb (byte 8 i) (length msg)) stream))
                       ;; Send payload
                       (write-sequence msg stream))
                  (finish-output (usocket:socket-stream socket))
                  (log-event config "Sent message to" address :PORT port))
            (ignore-errors (usocket:socket-close socket))
            ;; Clear state related to this connection.
            (mpcompat:with-lock ((lock transport))
              (remhash (list address port) message-queues)
              (setf sockets (remove socket sockets)))))
      (error (err)
        ;; Notify that peer is down and then terminate.
        (log-event config "Write thread error" err)
        ;; Purge message queue to drop messages and forget connection.
        (mpcompat:with-lock (lock)
          (setf (gethash (list address port) message-queues) nil))
        (run-peer-down-hook (config transport) address port (princ-to-string err))))))

(defun connect-to (transport address port)
  "Return a new socket conneted to ADDRESS:PORT.
  Retries automatically during *CONNECT-TIMEOUT*."
  (log-event (config transport) "Connecting to" address :PORT port)
  (usocket:socket-connect address port
                          :protocol :stream
                          :nodelay :if-supported
                          :timeout 10
                          :element-type '(unsigned-byte 8)))

(defmethod transmit-message ((transport tcp-threads-transport) address port message)
  (mpcompat:mailbox-send (get-message-queue transport address port) message))

(defun get-message-queue (transport address port)
  (with-slots (lock message-queues writers) transport
    ;; Check if the writer for this endpoint was already created.
    (or (gethash (list address port) message-queues)
        (mpcompat:with-lock (lock)
          ;; Check if the writer was created before we acquired the lock.
          (or (gethash (list address port) message-queues)
              ;; Create the (one and only) writer to this endpoint.
              (let ((mailbox (mpcompat:make-mailbox)))
                (setf (gethash (list address port) message-queues) mailbox)
                (push (mpcompat:process-run-function (format nil "gossip/transport writer to ~A:~A" address port)
                                                     nil
                                                     (lambda ()
                                                       (log-event (config transport) "Started write thread for" transport :TO address :PORT port)
                                                       (run-tcp-write-thread transport address port mailbox)))
                      writers)
                mailbox))))))


