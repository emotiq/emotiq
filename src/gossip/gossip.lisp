;;; gossip.lisp
;;; 8-Mar-2018 SVS

(defclass message-mixin ()
  ((uid :initarg :uid :initform nil :accessor uid
        :documentation "Unique ID")
   (timestamp :initarg :timestamp :initform nil :accessor timestamp
              :documentation "Timestamp of message origination")
   (kind :initarg :kind :initform nil :accessor kind
         :documentation "The verb of the message, indicating what action to take.")
   (args :initarg :args :initform nil :accessor args
            :documentation "Payload of the message. Arguments to kind.")))

(defclass solicitation (message-mixin)
  ((reply-requested? :initarg :reply-requested? :initform nil :accessor
                     reply-requested?
                     :documentation "True if a reply is requested to this solicitation.
            Kind will dictate nature of reply.")))

(defclass reply (message-mixin)
  ()
  (:documentation "Reply message. Used to reply to solicitations that need a reply."))

(defclass gossip-node ()
  ((message-cache :initarg :message-cache :initform nil :accessor message-cache
                  :documentation "Cache of seen messages")
   (kvs :initarg :kvs :initform nil :accessor kvs
        :documentation "Local key/value store for this node")
   (neighbors :initarg :neighbors :initform nil :accessor neighbors
              :documentation "Set of direct neighbors of this node")
   (logfn :initarg :logfn :initform nil :accessor logfn
          :documentation "If non-nil, assumed to be a function called with every
              message seen to log it.")))

(defmethod handle-message :after ((kind t) (node gossip-node) &rest args)
  (when (logfn node)
    (apply (logfn node) kind args)))

(defmethod handle-message ((kind (eql :assign)) (node gossip-node) &rest args)
  "Establishes a key/value pair to this node"
  (destructuring-bind (key value &rest other) args
    (setf (gethash key (kvs node) value))))

(defmethod handle-message ((kind (eql :inquire)) (node gossip-node) &rest args)
  "Inquire as to the value of a key on a node. If this node has no further
   neighbors, just return its value. Otherwise collect responses from subnodes."
  (gethash (car args) (kvs node) value))

(defmethod handle-message ((kind (eql :max)) (node gossip-node) &rest args)
  "Retrieve maximum value of a given key on all the nodes"
  )

(defmethod handle-message ((kind (eql :min)) (node gossip-node) &rest args)
  "Retrieve minimum value of a given key on all the nodes"
  )