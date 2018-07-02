;;;
;;; Asychronous event loop TCP transport implementation based on cl-async.
;;;

(in-package :gossip/transport)

(defclass tcp-async-transport ()
  (;; Operational state
   (closed :initform nil :accessor closed)
   (shutdown-notifier :initarg :shutdown-notifier :accessor shutdown-notifier)
   (doorbell :accessor doorbell)
   (outbox :initform (mpcompat:make-mailbox) :accessor outbox)
   (outbound-connections :initform (make-hash-table :test 'equal) :accessor outbound-connections)
   ;; Configuration state.
   (message-received-hook :initarg :message-received-hook :initform nil :accessor message-received-hook)
   (peer-up-hook :initarg :peer-up-hook :initform nil :accessor peer-up-hook)
   (peer-down-hook :initarg :peer-down-hook :initform nil :accessor peer-down-hook)))

(defmethod start (address port msg peer-up peer-down
                  (backend (eql :tcp-async)) backend-parameters)
  (declare (ignore backend-parameters))
  (let ((transport (make-instance 'tcp-async-transport
                                  :message-received-hook msg
                                  :peer-up-hook peer-up
                                  :peer-down-hook peer-down))
        ;; Mailbox for communicating success or failure of initialization.
        (start-mailbox (mpcompat:make-mailbox)))
    (flet ((start-event-loop ()
             ;; Initialization code to run inside the cl-async event loop in a separate thread.
             (handler-case (cl-async:dns-lookup address
                                                (lambda (host family)
                                                  (declare (ignore family))
                                                  (setf (doorbell transport)
                                                        (cl-async:make-notifier (lambda () (process-outbox transport))))
                                                  (setf (shutdown-notifier transport)
                                                        (cl-async:make-notifier #'as:exit-event-loop))
                                                  (cl-async:tcp-server host port 'inbound-data
                                                                       :connect-cb (lambda (socket)
                                                                                     (setf (cl-async:socket-data socket)
                                                                                           (list transport))))
                                                  ;; Report that event loop has started.
                                                  (mpcompat:mailbox-send start-mailbox :ok)))
               (error (err)
                 ;; Report that event loop has failed to start.
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
        (unless (eq status :ok)
          ;; Take the error from the event loop and signal it on the calling thread.
          (error status)))
      transport)))

(defmethod stop ((transport tcp-async-transport))
  (cl-async:trigger-notifier (shutdown-notifier transport))
  (mpcompat:process-wait-with-timeout "stopping event loop"
                                      1.0
                                      (lambda () (closed transport))))

;;; ------------------------------------------------------------
;;; Inbound messages.
;;; ------------------------------------------------------------

(defparameter *magic* (coerce #(#x43 #xa9 #xe1 0) '(simple-array (unsigned-byte 8) (*)))
  "Protocol magic number.
  The last byte represents the protocol version number.")

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
               (assert (= byte (aref *magic* position)))
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

(defun deliver-input-message (transport message)
  (when (message-received-hook transport)
    (funcall (message-received-hook transport) message)))

;;; ------------------------------------------------------------
;;; Outbound messages.
;;; ------------------------------------------------------------

;;;
;;; Top half that runs outside the event loop.
;;;
;;; Transmit message algorithm:
;;;
;;; - Establish the outbound connection if one does not already exist.
;;; - Enqueue the message in the per-connection mailbox.
;;; - Trigger the per-connection notifier to wake up the event loop thread.
;;;
;;; The outbound connection is represented by a socket object. The
;;; mailbox and notifier objects are associated with the socket as
;;; a list value for CL-ASYNC:SOCKET-DATA.

(defmethod transmit-message ((transport tcp-async-transport) address port message)
  "Transmit a message to a remote peer."
  (with-slots (outbox doorbell) transport
    (mpcompat:mailbox-send outbox (list address port message))
    (cl-async:trigger-notifier doorbell)))

;;;
;;; Bottom half that runs inside the event loop.
;;;
;;; Wake up via the event loop notifier and send all queues messages
;;; to the socket. Output buffering is handled by cl-async.

(defun process-outbox (transport)
  "Send all messages queued in outbox to sockets."
  (with-slots (outbox) transport
    (loop until (mpcompat:mailbox-empty? outbox)
         do (apply #'send-outbound-message transport (mpcompat:mailbox-read outbox)))))

(defun send-outbound-message (transport address port message)
  "Send MESSAGE to the remote peer at ADDRESS:PORT."
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
                                           :data *magic*)))
        (setf (gethash (list address port) (outbound-connections transport))
              (make-instance 'cl-async:async-output-stream :socket socket)))))

(defun outbound-socket-read (transport address port socket data)
  (declare (ignore data))
  ;; LOG: Unexpected input on outbound socket.
  (cl-async:close-socket socket)
  (drop-outbound-connection transport address port))

(defun outbound-socket-event (transport address port err)
  (declare (ignore err))
  ;; LOG: outbound connection closed
  (drop-outbound-connection transport address port))

(defun drop-outbound-connection (transport address port)
  (remhash (list address port) (outbound-connections transport)))

