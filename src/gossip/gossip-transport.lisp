;;; socket-actors-transport.lisp
;;; Transport (TCP) layer of the socket-actors implementation.

;;; This is a network transport implementation for Gossip messages
;;; sent between actors on different nodes. The implementation is
;;; modelled closely on the Erlang distribution protocol.
;;;
;;; Key points:
;;;
;;; - Each node is identified by its ADDRESS:PORT endpoint.
;;;
;;; - Message can be sent asynchronously to the ADDRESS:PORT of any
;;;   node at any time with best-effort ("send and pray") delivery.
;;;
;;; - Connections to other nodes are automatically made when needed to
;;;   deliver a message.
;;;
;;; - Callbacks are invoked whenever connectivity with a peer is
;;;   established or broken.
;;;
;;; - Incoming messages are dispatched via a callback function.
;;;
;;; - One dedicated process performs all of the asynchronous
;;;   processing efficiently.

(in-package :gossip/transport)

;;;
;;; High-level interface functions.
;;;

(defun start-node (&key address
                        port
                        message-received-hook
                        peer-up-hook
                        peer-down-hook)
  "Start a new transport node that accepts connections on ADDRESS:PORT.
   The transport node runs asynchronously on a dedicated process and
   runs callback functions when relevant events occur.

   The function (MESSAGE-RECEIVED-HOOK ADDRESS PORT MESSAGE) is called
   each time a message is received from a peer.

   The function (PEER-UP-HOOK ADDRESS PORT) is called each time a new
   connection with a peer is established (whether inbound or
   outbound.)

   The function (PEER-DOWN-HOOK ADDRESS PORT) is called each time an
   existing connection goes down (e.g. due to timeout or connectivity
   trouble) and also whenever a new outgoing connection cannot be
   established (e.g. peer address is not reachable.)"
  (start-tcp-node address port message-received-hook peer-up-hook peer-down-hook))

(defun stop-node (node)
  "Stop NODE by closing its network connection and discarding state."
  (stop node))

(defun print-node-status (node &key (stream *standard-output*))
  "Print status information about NODE and its connections to peers."
  (print-status node stream))

(defun send (transport address port message)
  "Transmits MESSAGE to TRANSPORT's peer at ADDRESS:PORT.
   If no connection to the peer exists then one is created
   asynchronously and the message is queued for delivery once the
   connection is established.
   If the message cannot be delivered then it is silently dropped (but
   the connectivity error will be reported separately via
   PEER-DOWN-HOOK.)"
  (send-message transport address port message))

;;;
;;; Internal implementation functions.
;;;

(defclass tcp-transport ()
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

(defun start-tcp-node (address port message-received peer-up peer-down)
  (cerror "NYI" nil))

(defgeneric stop (node))
(defgeneric send-message (transport address port message))
(defgeneric print-status (node stream))


