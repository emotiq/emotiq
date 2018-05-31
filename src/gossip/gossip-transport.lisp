;;; gossip-transport.lisp

;;; The GOSSIP/TRANSPORT API delivers messages between Gossip nodes
;;; asynchronously over the network with best-effort ("send and pray")
;;; semantics.
;;;
;;; See also https://en.wikipedia.org/wiki/Best-effort_delivery.

(in-package :gossip/transport)

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

   The callback (MESSAGE-RECEIVED-HOOK ADDRESS PORT MESSAGE) is called
   each time a message is received from a peer. The message is a byte
   array and the hook function is assumed to perform all necessary
   processing for the message.

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
   (peers :initarg :peers :initform nil :accessor peers
          :documentation "Key-value-store of socket peers.")
   ;; Configuration state.
   (peer-up-hook :initarg :peer-up-hook :initform nil :accessor peer-up-hook)
   (peer-down-hook :initarg :peer-down-hook :initform nil :accessor peer-down-hook)))

(defstruct peer
  ;; Remote address.
  ip port
  ;; Queue of messages waiting to be delivered.
  transmit-queue)

(defmethod start (address port msg peer-up peer-down
                          (backend (eql :tcp)) backend-parameters)
  (declare (ignore address port msg peer-up peer-down backend backend-parameters))
  (cerror "NYI" nil))



