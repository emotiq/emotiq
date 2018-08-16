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

(defgeneric start (address port mrh peer-up peer-down backend backend-parameters))
(defgeneric stop (transport))
(defgeneric transmit-message (transport address port message))
(defgeneric status-of (transport))

;;;
;;; TCP transport implementation based on thread-per-connection model.
;;;

;;; To see open sockets:
;;; (gossip/transport::sockets gossip::*transport*)

(defun socket-info (socket)
  ; errors can happen if socket is closed
  (list (ignore-errors (usocket::get-local-address socket))
        (ignore-errors (usocket::get-local-port socket))
        (ignore-errors (usocket::get-peer-address socket))
        (ignore-errors (usocket::get-peer-port socket))))

(defun peerstring (socket)
  "Returns a string describing socket's peers"
  (format nil "~A/~A"
          (ignore-errors (usocket:get-peer-address socket))
          (ignore-errors (usocket:get-peer-port socket))))

(defun sockets-matching (pred)
  (when pred
    (loop for socket in (sockets gossip::*transport*)
      when (or (eq t pred)
               (ignore-errors (funcall pred socket)))
      collect socket)))

; (dolist (socket (gossip/transport::sockets-matching t)) (print (gossip/transport::socket-info socket)))

(defun sockets-to (peer-address)
  "Returns a list of sockets connected to given peer-address"
  (sockets-matching (lambda (socket)
                      (gossip::gossip-equalp peer-address (usocket::get-peer-address socket)))))

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

(defun canonicalize-address (address)
  "Always return a string representation of address."
  (usocket::host-to-hostname address))

(defun canonicalize-port (port)
  "Always return an integer for port"
  (if (integerp port)
      port
      ; Eventually we should be able to look ports up by name
      (error "Cannot deal with non-integer ports yet")))

(defmethod start (address port mrh peer-up peer-down
                          (backend (eql :tcp)) backend-parameters)
  (declare (ignore backend-parameters))
  (edebug 5 :transport "Starting transport on" address :PORT port)
  (let* ((listen-socket (usocket:socket-listen address (or port 0)
                                               :reuse-address t
                                               :element-type '(unsigned-byte 8)))
         (transport (make-instance 'tcp-threads-transport
                                   :message-received-hook mrh
                                   :peer-up-hook peer-up
                                   :peer-down-hook peer-down
                                   :listen-socket listen-socket)))
    (setf (listen-process transport)
          (mpcompat:process-run-function (format nil "gossip/transport accept on ~A/~A" address port)
                                         nil
                                         (lambda ()
                                           (run-tcp-listen-thread transport))))
    (edebug 5 :transport "Started transport" transport)
    transport))

(defmethod stop ((transport tcp-threads-transport))
  ;; XXX detect when already stopped and... what?
  (edebug 5 :transport "Stopping transport" transport)
  (with-slots (lock listen-socket listen-process readers writers sockets) transport
    (mpcompat:with-lock (lock)
      ;; Stop worker processes.
      (mapc #'mpcompat:process-kill (list* listen-process (append readers writers)))
      (setf readers nil
            writers nil)
      ;; Stop accepting new connections.
      (ignore-errors (usocket:socket-close listen-socket))
      (setf listen-socket nil)
      ;; Close existing connections.
      (dolist (socket sockets)
        (ignore-errors (usocket:socket-close socket)))
      (setf sockets nil)))
  (edebug 5 :transport "Stopped transport" transport))

(defmethod status-of ((transport tcp-threads-transport))
  (list :listen-address (usocket:get-local-address (listen-socket transport))
        :listen-port (usocket:get-local-port (listen-socket transport))))

;;; Listening

(defun run-tcp-listen-thread (transport)
  (edebug 5 :transport "Started listen thread for" transport)
  (with-slots (listen-socket readers lock peer-down-hook sockets) transport
    (gossip-handler-case
     (unwind-protect
         (loop
           (let* ((socket (ignore-errors (usocket:socket-accept listen-socket)))
                  (timed-out-if-null
                   (ccl::process-input-wait* (ccl::socket-device (usocket:socket socket)) 10000) ; it's milliseconds
                   ))
             
             (cond ((or (null timed-out-if-null)
                        (gossip::stream-eofp (usocket:socket-stream socket)))
                    (ignore-errors (usocket:socket-close socket)))
                   
                   (t (mpcompat:with-lock (lock) (push socket sockets))
                      (let ((process (mpcompat:process-run-function (format nil "gossip/transport reader on ~A" socket)
                                       nil
                                       (lambda ()
                                         (tcp-read-loop transport socket)))))
                        (mpcompat:with-lock (lock) (push process readers)))))))
       (ignore-errors (usocket:socket-close listen-socket))
       (when peer-down-hook
         (funcall peer-down-hook
                  (ignore-errors (usocket:get-local-address listen-socket))
                  (ignore-errors (usocket:get-local-port listen-socket))
                  "Listen thread error. Cannot recover.")))
     (error () ; should we automatically call #'start again with this transport if this happens??
            nil))))

;;; Reading

(defun tcp-read-loop (transport socket)
  "Transfer objects from SOCKET to MESSAGE-RECEIVED-HOOK."
  (edebug 5 :transport "Started read loop from" (peerstring socket))
  (with-slots (lock peer-down-hook readers sockets) transport
    (gossip-handler-case
     (unwind-protect
         (loop
           (let ((msg (loenc:deserialize (usocket:socket-stream socket))))
             (edebug 5 :transport "Received message apparently from" (peerstring socket) msg)
             (when (message-received-hook transport)
               (funcall (message-received-hook transport) msg))))
       ;; Clear state related to this connection.
       (ignore-errors (usocket:socket-close socket))
       (mpcompat:with-lock (lock)
         (setf sockets (remove socket sockets))
         ;; This process is about to die. Remove it from readers list.
         (setf readers (remove (mpcompat:current-process) readers)))
       (when peer-down-hook
         (funcall peer-down-hook
                  (ignore-errors (usocket:get-peer-address socket))
                  (ignore-errors (usocket:get-peer-port socket))
                  "Read loop error")))
     (error ()
            nil))))

;;; Writing

(defun tcp-write-loop (transport address port mailbox)
  "Transfer objects from MAILBOX to the remote endpoint ADDRESS:PORT."
  (setf address (canonicalize-address address)
        port    (canonicalize-port    port))
  (edebug 4 :transport "Started write loop" :TO address :PORT port)
  (with-slots (lock message-queues peer-down-hook writers sockets) transport
    (gossip-handler-case
     ;; Establish new connection.
     (let ((socket (ignore-errors (connect-to address port (eripa)))))
       (cond (socket ; connection might have been refused
              (edebug 4 :transport "Ready to write to" :ADDRESS address :PORT port :FROMPORT (ignore-errors (usocket:get-local-port socket)))
              (mpcompat:with-lock (lock) (push socket sockets))
              ;; Loop transferring messages from mailbox to socket.
              (unwind-protect
                  (loop
                    (unless (ignore-errors (usocket::get-peer-address socket)) ; force an error to be thrown if socket is not connected
                      (error "Disconnected socket")) ; this error should correct itself (because writer will try again)
                    ; Note 1: Ignore-errors above is because: While I know get-peer-address will throw an error on CCL,
                    ;   I don't know if it will do so on other implementations. Therefore we force the error.
                    ; Note 2: Sometimes finish-output will blindly send output to a disconnected socket
                    ;   without throwing an error (at least on CCL), so checking explicitly ensures this won't happen.
                    (loenc:serialize (mpcompat:mailbox-read mailbox)
                                     (usocket:socket-stream socket))
                    (finish-output (usocket:socket-stream socket))
                    (edebug 5 :transport "Sent message to" address :PORT port))
                ;; Clear state related to this connection.
                (ignore-errors (usocket:socket-close socket))
                (mpcompat:with-lock (lock)
                  (setf sockets (remove socket sockets))
                  ;; Purge message queue to drop messages and forget connection.
                  (remhash (list address port) message-queues)
                  ;; This process is about to die. Remove it from writers list.
                  (setf writers (remove (mpcompat:current-process) writers)))
                (when peer-down-hook
                  (funcall peer-down-hook address port "Write loop error"))))
             (t (mpcompat:with-lock ((lock transport)) ; if socket itself was not created, remove message queue.
                  (remhash (list address port) message-queues)))))
     (error ()
            nil))))

(defun connect-to (address port &optional local-host)
  "Return a new socket conneted to ADDRESS:PORT.
  Retries automatically during *CONNECT-TIMEOUT*."
  (edebug 5 :transport "Connecting to" :ADDRESS address :PORT port)
  (usocket:socket-connect address port
                          :protocol :stream
                          :nodelay :if-supported
                          :timeout 10
                          :element-type '(unsigned-byte 8)
                          :local-host local-host))

(defmethod transmit-message ((transport tcp-threads-transport) address port message)
  (setf address (canonicalize-address address)
        port    (canonicalize-port    port))
  (mpcompat:mailbox-send (get-message-queue transport address port) message))

(defun get-message-queue (transport address port)
  (setf address (canonicalize-address address)
        port    (canonicalize-port    port))
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
                          (tcp-write-loop transport address port mailbox)))
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
         (msg (lambda (&rest msg) (declare (ignore msg)) (incf n)))
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

