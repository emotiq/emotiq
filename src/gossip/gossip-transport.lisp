;;; gossip-transport.lisp

;;; The GOSSIP/TRANSPORT API delivers messages between Gossip nodes
;;; asynchronously over the network with best-effort ("send and pray")
;;; semantics.
;;;
;;; See also https://en.wikipedia.org/wiki/Best-effort_delivery.

(in-package :gossip/transport)

(defparameter *connect-timeout* 300 "Seconds before an unestablished outbound connection is abandoned.")
(defparameter *idle-timeout*    300 "Seconds before an idle outbound connection is closed.") ;; XXX unused

;;;
;;; High-level interface functions.
;;;

(defun start-transport (&rest backend-parameters
                        &key (backend :tcp)
                             address
                             port
                             message-received-hook
                             peer-up-hook
                             peer-down-hook
                        &allow-other-keys)
  "Start a new transport node that accepts connections on ADDRESS:PORT.
   The transport node executes asynchronously in the background and
   runs callback functions when important events occur. The callback
   functions are assumed to return promptly without blocking.

   The callback (MESSAGE-RECEIVED-HOOK MESSAGE) is called each time a
   message is received from a peer. The message is a byte array and
   the hook function is assumed to perform all necessary processing
   for the message.

   The callback (PEER-UP-HOOK ADDRESS PORT) is called each time a new
   connection with a peer is (re)established. The connection can be
   triggered by either an outbound or an inbound connection.

   The callback (PEER-DOWN-HOOK ADDRESS PORT REASON) is called each
   time an existing connection goes down (e.g. due to timeout or
   connectivity trouble) and also whenever a new outgoing connection
   cannot be established (e.g. peer address is not reachable within a
   reasonable timeframe.) 
   The reason is a human-readable string suitable for logging.

   BACKEND and BACKEND-PARAMETERS optionally specify additional
   low-level and implementation-specific parameters.

   Returns an object representing the transport node or raises an
   error if the node could not be started (e.g. because the
   address/port was not available.)"
  (start address port message-received-hook peer-up-hook peer-down-hook
         backend backend-parameters))

(defun stop-transport (node)
  "Stop NODE by closing its network connection and discarding state."
  (stop node))

(defun status (node)
  "Return status information about NODE as a property list."
  (status-of node))

(defun transmit (transport to-address to-port message)
  "Transmit a message to another node with best-effort delivery.

   The message is a byte array that will be delivered verbatum to
   the other node's MESSAGE-RECEIVED-HOOK without interpretation in
   this transport layer.

   The transport layer will take pains to deliver the message
   promptly, including automatically (re)establishing connectivity
   with the node and queueing the message during temporary
   connectivity problems, but no guarantee is made.

   The transport layer does not provide message delivery
   receipts (ack/nak). This means that the layer above is responsible
   for handling the possibility that any given message is not
   delivered (e.g. using timeouts and explicitly acknowledgement
   messages.)"
  (transmit-message transport to-address to-port message))

;;;
;;; Generic functions supporting multiple transport implementations.
;;;

(defgeneric start (address port msg peer-up peer-down backend backend-parameters))
(defgeneric stop (transport))
(defgeneric transmit-message (transport address port message))
(defgeneric status-of (transport))

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
   (message-received-hook :initarg :message-received-hook :initform nil :accessor message-received-hook)
   (peer-up-hook :initarg :peer-up-hook :initform nil :accessor peer-up-hook)
   (peer-down-hook :initarg :peer-down-hook :initform nil :accessor peer-down-hook)))

(defstruct peer
  ;; Remote address.
  ip port
  ;; Queue of messages waiting to be delivered.
  transmit-queue)

(defmethod start (address port msg peer-up peer-down
                          (backend (eql :tcp)) backend-parameters)
  (declare (ignore backend-parameters))
  (let* ((listen-socket (usocket:socket-listen address (or port 0)
                                               :reuse-address t
                                               :element-type '(unsigned-byte 8)))
         (transport (make-instance 'tcp-threads-transport
                                   :message-received-hook msg
                                   :peer-up-hook peer-up
                                   :peer-down-hook peer-down
                                   :listen-socket listen-socket)))
    (setf (listen-process transport)
          (mpcompat:process-run-function (format nil "gossip/transport accept on ~A" listen-socket)
                                         nil
                                         (lambda ()
                                           (run-tcp-listen-thread transport))))
    transport))

(defmethod stop ((transport tcp-threads-transport))
  ;; XXX detect when already stopped and... what?
  (with-slots (lock listen-socket listen-process readers writers sockets) transport
    (mpcompat:with-lock (lock)
      ;; Stop worker processes.
      (mapc #'mpcompat:process-kill (list* listen-process (append readers writers)))
      ;; Stop accepting new connections.
      (ignore-errors (usocket:socket-close listen-socket))
      ;; Close existing connections.
      (dolist (socket sockets)
        (ignore-errors (usocket:socket-close socket))))))

(defmethod status-of ((transport tcp-threads-transport))
  (list :listen-address (usocket:get-local-address (listen-socket transport))
        :listen-port (usocket:get-local-port (listen-socket transport))))

;;; Listening

(defun run-tcp-listen-thread (transport)
  (with-slots (listen-socket readers lock peer-down-hook sockets) transport
    (handler-case
        (loop
           (let ((socket (usocket:socket-accept listen-socket)))
             (mpcompat:with-lock (lock) (push socket sockets))
             (let ((process (mpcompat:process-run-function (format nil "gossip/transport reader on ~A" socket)
                                                           nil
                                                           (lambda ()
                                                             (run-tcp-read-thread transport socket)))))
               (push process (readers transport)))))
      (error (err)
        (when peer-down-hook
          (funcall peer-down-hook (princ-to-string err)))))))

;;; Reading

(defun run-tcp-read-thread (transport socket)
  "Transfer objects from SOCKET to MESSAGE-RECEIVED-HOOK."
  (handler-case
      (loop
         (funcall (message-received-hook transport)
                  (loenc:deserialize (usocket:socket-stream socket))))
    (error ())))

;;; Writing

(defun run-tcp-write-thread (transport address port mailbox)
  "Transfer objects from MAILBOX to the remote endpoint ADDRESS:PORT."
  (with-slots (lock message-queues peer-down-hook sockets) transport
    (handler-case
        ;; Establish new connection.
        (let ((socket (connect-to address port)))
          (mpcompat:with-lock (lock) (push socket sockets))
          ;; Loop transferring messages from mailbox to socket.
          (unwind-protect
               (loop (loenc:serialize (mpcompat:mailbox-read mailbox)
                                      (usocket:socket-stream socket)))
            (ignore-errors (usocket:socket-close socket))
            ;; Clear state related to this connection.
            (mpcompat:with-lock ((lock transport))
              (remhash (list address port) message-queues)
              (setf sockets (remove socket sockets)))))
      (error (err)
        ;; Notify that peer is down and then terminate.
        (when peer-down-hook
          (funcall peer-down-hook address port (princ-to-string err)))))))

(defun connect-to (address port)
  "Return a new socket conneted to ADDRESS:PORT.
  Retries automatically during *CONNECT-TIMEOUT*."
  (loop with deadline = (+ (usec:get-time-usec) (* *connect-timeout* 1000000))
        for retry-delay = 0.1 then (min 10 (* retry-delay 2))
        when (> (usec:get-time-usec) deadline) do (error "Failed to establish connection before timeout")
        do (handler-case (return (usocket:socket-connect address port
                                                         :protocol :stream
                                                         :nodelay :if-supported
                                                         :timeout 10
                                                         :element-type '(unsigned-byte 8)))
             (usocket:socket-error () (sleep retry-delay)))))

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
                                                                         (run-tcp-write-thread transport address port mailbox)))
                                        writers)
                                  mailbox))))))

(defun test ()
  (let ((transport (start-transport :address "localhost")))
    (sleep 0.1)
    (unwind-protect
        (transmit transport
                  "localhost"
                  (getf (status transport) :listen-port)
                  #(0 1 2 3 4)))
    (sleep 1)
    (stop-transport transport)))

(defun test-2 ()
  (let* ((n 0)
         (msg (lambda (&rest msg) (incf n)))
         (transports (loop for i from 1 to 10 collect (start-transport :address "localhost" :message-received-hook msg)))
         (ports (loop for tr in transports collect (getf (status tr) :listen-port))))
    (unwind-protect
        (progn
          (dotimes (i 10000)
            (dolist (tr transports)
              (dolist (port ports)
                (transmit tr "localhost" port #(1 2 3 4)))))
          (sleep 1))
      (mapc #'stop-transport transports))
    n))

(defun msg (format &rest args)
  (format t "~&~?~%" format args))

