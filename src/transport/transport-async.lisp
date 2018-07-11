;; transport-async.lisp

;;;
;;; Asychronous event loop TCP transport implementation based on cl-async.
;;;

(in-package :transport/async)

(defclass tcp-async-transport ()
  (;; Operational state
   (closed :initform nil :accessor closed)
   (shutdown-notifier :initarg :shutdown-notifier :accessor shutdown-notifier)
   (doorbell :accessor doorbell)
   (outbox :initform (mpcompat:make-mailbox) :accessor outbox)
   (outbound-connections :initform (make-hash-table :test 'equal) :accessor outbound-connections)
   ;; Configuration state.
   (config :initarg :config :accessor config)))

(defmethod start ((backend (eql :tcp-async)) config)
  (multiple-value-bind (address port) (my-endpoint config)
    (log-event config "Starting async transport on" address :port port)
    (let ((transport (make-instance 'tcp-async-transport :config config))
          ;; Mailbox for communicating success or failure of initialization.
          (start-mailbox (mpcompat:make-mailbox)))
      (flet ((start-event-loop ()
               ;; Initialization code to run inside the cl-async event loop in a separate thread.
               (log-event config "Started cl-async event loop" transport)
               (handler-case (cl-async:dns-lookup address
                                                  (lambda (host family)
                                                    (declare (ignore family))
                                                    (setf (doorbell transport)
                                                          (cl-async:make-notifier (lambda () (process-outbox transport))))
                                                    (setf (shutdown-notifier transport)
                                                          (cl-async:make-notifier #'as:exit-event-loop))
                                                    (cl-async:tcp-server host port 'inbound-data
                                                                         :event-cb (lambda (err) (inbound-error transport err))
                                                                         :connect-cb (lambda (socket)
                                                                                       (setf (cl-async:socket-data socket)
                                                                                             (list transport))))
                                                    ;; Report that event loop has started.
                                                    (mpcompat:mailbox-send start-mailbox :ok)))
                 (error (err)
                   ;; Report that event loop has failed to start.
                   (log-event config "Handling error in cl-async event loop" err)
                   (mpcompat:mailbox-send start-mailbox err)
                   (error err)))))
        ;; Start a separate thread to initialize and run the event loop.
        (mpcompat:process-run-function "gossip/transport async event loop"
                                       nil
                                       (lambda ()
                                         (cl-async:start-event-loop #'start-event-loop
                                                                    
                                                                    :catch-app-errors t)))
        ;; Wait for the initialization code to report success or failure to this thread.
        (let ((status (mpcompat:mailbox-read start-mailbox)))
          (log-event config "Received init status from async thread" status)
          (unless (eq status :ok)
            ;; Take the error from the event loop and signal it on the calling thread.
            (error status)))
        transport))))

(defmethod stop ((transport tcp-async-transport))
  (log-event (config transport) "Stopping transport" transport)
  (cl-async:trigger-notifier (shutdown-notifier transport))
  (mpcompat:process-wait-with-timeout "stopping event loop"
                                      1.0
                                      (lambda () (closed transport)))
  (log-event (config transport) "Stopped transport" transport))

;;; ------------------------------------------------------------
;;; Inbound messages.
;;; ------------------------------------------------------------

(defun inbound-data (socket data)
  ;; Unpack state from socket object.
  ;; &optional because this will be NIL for new sockets.
  (destructuring-bind (&optional
                       transport
                       ;; State machine state:
                       state            ; :READ-MAGIC | :READ-LENGTH | :READ-MESSAGE.
                       length           ; Length of next message.
                       position         ; Position of next input byte.
                       buffer)          ; Buffer where message is stored.
      (cl-async:socket-data socket)
    (log-event (config transport) "Received data on socket" socket :transport transport :data data)
    ;;
    ;; Helper functions for state transitions.
    ;;
    (flet ((transition-to-read-magic ()
             ;; Set state machine to :READ-MAGIC state.
             (setf state :read-magic
                   length 0
                   position 0
                   buffer nil))
           (transition-to-read-length ()
             ;; Set state machine to :READ-LENGTH state.
             (setf state :read-length
                   length 0
                   position 0
                   buffer nil))
           (transition-to-read-message (message-length)
             ;; Set state machine to :READ-MESSAGE state.
             (setf state :read-message
                   length message-length
                   position 0
                   buffer (make-array (list length) :element-type '(unsigned-byte 8)))))
      ;;
      ;; Initialize state machine if this is the first event.
      ;;
      (when (null state)
        (transition-to-read-magic))
      ;; Run the state machine on each byte of input.
      (loop for byte across data
         do (ecase state
              ;;
              ;; :READ-MAGIC state reads the 32-bit magic number to
              ;; check for a compatible transport protocol version.
              ;;
              (:read-magic
               (assert (= byte (aref +magic+ position)))
               (incf position)
               (when (= position 4)
                 (transition-to-read-length)))
              ;;
              ;; :READ-LENGTH state reads the 32-bit length header and
              ;; then transitions to :READ-MESSAGE.
              ;;
              (:read-length
               ;; Incrementally accumulate the message length bytes.
               (setf length (+ (ash length 8) byte))
               (when (= (incf position) 4)
                 (transition-to-read-message length)))
              ;;
              ;; :READ-MESSAGE state reads the variable size message and
              ;; then transitions back to :READ-LENGTH.
              ;;
              (:read-message
               ;; Store the next byte of the message
               (setf (aref buffer position) byte)
               (incf position)
               (when (= position length)
                 (deliver-input-message transport buffer)
                 (transition-to-read-length)))))
      ;; Pack state back into socket object.
      (setf (cl-async:socket-data socket)
            (list state length position buffer)))))

(defun inbound-error (transport err)
  (log-event (config transport) "Error on inbound connection" err))

(defun deliver-input-message (transport message)
  (run-message-received-hook (config transport) message))

;;; ------------------------------------------------------------
;;; Outbound messages.
;;; ------------------------------------------------------------

;;;
;;; Transmit a message by adding it to the "outbox" (message queue
;;; mailbox) and "ringing the doorbell" (notifying the event loop to
;;; asynchronously wake up and process the outbox.)
;;;

(defmethod transmit-message ((transport tcp-async-transport) address port message)
  "Transmit a message to a remote peer."
  (log-event (config transport) "Transmitting message" message :address address :port port)
  (with-slots (outbox doorbell) transport
    (mpcompat:mailbox-send outbox (list address port message))
    (handler-case (cl-async:trigger-notifier doorbell)
      (cl-async:event-error (err)
        (error "Transport backend failed: ~A" err)))))

;;;
;;; Code below runs inside the cl-async event loop and makes the
;;; connections.
;;;

(defun process-outbox (transport)
  "Send all messages queued in outbox to sockets."
  (log-event (config transport) "Processing outbox" transport)
  (with-slots (outbox) transport
    (loop until (mpcompat:mailbox-empty? outbox)
         do (apply #'send-outbound-message transport (mpcompat:mailbox-read outbox)))))

(defun send-outbound-message (transport address port message)
  "Send MESSAGE to the remote peer at ADDRESS:PORT."
  (log-event (config transport) "Transmitting message to " address :port port)
  (with-slots (outbound-connections) transport
    (let ((stream (get-outbound-stream transport address port)))
      ;; Send length header.
      (loop for i from 24 downto 0 by 8
         do (write-byte (ldb (byte 8 i) (length message)) stream))
      ;; Send message contents.
      (write-sequence (coerce message '(simple-array (unsigned-byte 8) (*))) stream)
      (finish-output stream))))

(defun get-outbound-stream (transport address port)
  "Return the output stream for peer ADDRESS:PORT.
  Initiate the connection if needed."
  (or (gethash (list address port) (outbound-connections transport))
      ;; Initiate outbound connection
      (let* ((socket (cl-async:tcp-connect address port
                                           (lambda (socket data)
                                             (outbound-socket-read transport address port socket data))
                                           :event-cb (lambda (err)
                                                       (outbound-socket-event transport address port err))
                                           :connect-cb (lambda (socket)
                                                         (declare (ignore socket))
                                                         (run-peer-up-hook (config transport) address port))
                                           :data +magic+)))
        (setf (gethash (list address port) (outbound-connections transport))
              (make-instance 'cl-async:async-output-stream :socket socket)))))

(defun outbound-socket-read (transport address port socket data)
  (log-event (config transport) "Closing outbound socket due to unexpected input" socket :data data)
  (cl-async:close-socket socket)
  (drop-outbound-connection transport address port))

(defun outbound-socket-event (transport address port err)
  (log-event (config transport) "Outbound socket closed" err)
  (drop-outbound-connection transport address port)
  (run-peer-down-hook (config transport) address port (prin1-to-string err)))

(defun drop-outbound-connection (transport address port)
  (remhash (list address port) (outbound-connections transport)))

