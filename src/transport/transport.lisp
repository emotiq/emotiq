;;; transport.lisp

;;; The GOSSIP/TRANSPORT API delivers messages between Gossip nodes
;;; asynchronously over the network with best-effort ("send and pray")
;;; semantics.
;;;
;;; See also https://en.wikipedia.org/wiki/Best-effort_delivery.

(in-package :transport)

;;;
;;; High-level interface functions.
;;;

(defun start-transport (&key (backend :tcp)
                             address
                             port
                             message-received-hook
                             peer-up-hook
                             peer-down-hook
                             log-event-hook)
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

   The callback (LOG-EVENT-HOOK STRING &rest ARGS) is called when
   events occur that can be significant for troubleshooting. The
   string and arguments are fre-form with no specific format.

   BACKEND and BACKEND-PARAMETERS optionally specify additional
   low-level and implementation-specific parameters.

   Returns an object representing the transport node or raises an
   error if the node could not be started (e.g. because the
   address/port was not available.)"
  (start backend
         (make-transport-config :address address
                                :port port
                                :message-received-hook message-received-hook
                                :peer-up-hook peer-up-hook
                                :peer-down-hook peer-down-hook
                                :log-event-hook log-event-hook)))

(defun stop-transport (node)
  "Stop NODE by closing its network connection and discarding state."
  (stop node))

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
;;; Interface towards backend implementations.
;;;

(defconstant +magic+ (coerce #(#x43 #xa9 #xe1 0) '(simple-array (unsigned-byte 8) (*)))
  "Protocol magic number.
  The last byte represents the protocol version number.")

(defstruct transport-config
  "Common configuration parameters for transport backends."
  address port log-event-hook message-received-hook peer-up-hook peer-down-hook)

(defgeneric start (backend config))
(defgeneric stop (transport))
(defgeneric transmit-message (transport address port message))

(defun my-endpoint (config)
  (values (transport-config-address config) (transport-config-port config)))

(defun run-message-received-hook (config msg)
  (run-hook config #'transport-config-message-received-hook msg))

(defun run-peer-up-hook (config address port)
  (run-hook config #'transport-config-peer-up-hook address port))

(defun run-peer-down-hook (config address port reason)
  (run-hook config #'transport-config-peer-down-hook address port reason))

(defun log-event (config string &rest args)
  (apply #'run-hook config #'transport-config-log-event-hook string args))

(defun run-hook (config hook &rest args)
  (let ((fn (funcall hook config)))
    (when fn (apply fn args))))

