;;; gossip.lisp
;;; 8-Mar-2018 SVS

(defparameter *max-message-age* 10 "Messages older than this number of seconds will be ignored")

(defclass uid-mixin ()
  ((uid :initarg :uid :initform nil :accessor uid
        :documentation "Unique ID")))

(defclass message-mixin (uid-mixin)
  ((timestamp :initarg :timestamp :initform nil :accessor timestamp
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

; Logcmds: Keywords that describe what a node has done with a given message UID

(defmethod maybe-log ((node gossip-node) logcmd uid &rest args)
  (when (logfn node)
    (apply (logfn node) logcmd uid args)))

;;; Mechanisms strictly for the simulation

(defparameter *end-simulation* nil "Set to true to end an ongoing simulation")

; Graham's Basic queue

(defun make-queue ()
  (cons nil nil))

(defun enq (obj q)
  (if (null (car q))
    (setf (cdr q) (setf (car q) (list obj)))
    (setf (cdr (cdr q)) (list obj)
          (cdr q) (cdr (cdr q))))
  (car q))

(defun deq (q)
  (pop (car q)))

(defvar *message-space* (make-queue))
(defvar *message-space-lock* (make-lock) "Just a lock to manage access to the message space")

(defun new-log ()
  (make-array 10 :adjustable t :fill-pointer 0))

(defvar *archived-logs* (make-array 10 :adjustable t :fill-pointer 0) "Previous historical logs")

(defvar *log* (new-log) "Log of simulated actions.")

(defun logging-function (logcmd uid &rest args)
  (vector-push-extend (list* logcmd uid args) *log*))

(defmethod send-msg ((msg solicitation) src-uid dest-uid)
  "Abstraction for asynchronous message sending. Post a message with src-id and dest-id into
  a global space that all nodes can read from."
  (with-lock-grabbed (*message-space-lock*)
    (enq (list msg src-uid dest-uid) *message-space*)
    ))

; TODO: Remove old entries in message-cache, eventually.
(defmethod receive-message ((sol solicitation) (destnode gossip-node) srcnode)
  "Deal with an incoming message. Srcnode could be nil in case of initiating messages."
  (cond ((< (get-universal-time) (+ *max-message-age* (timestamp sol))) ; ignore too-old messages
         (let ((already-seen? (gethash (uid sol) (message-cache destnode))))
           (cond (already-seen? ; ignore if already seen
                  (maybe-log destnode :ignore (uid sol) :already-seen))
                 (t
                  (setf (gethash (uid sol) (message-cache destnode)) t)
                  (maybe-log destnode :accepted (uid sol) (kind sol) (args sol))
                  (do-message (kind sol) destnode srcnode (args sol))))))
        (t (maybe-log destnode :ignore (uid sol) :too-old))))

(defmethod do-message ((kind (eql :assign)) destnode srcnode args)
  "Establishes a key/value pair on this node and forwards to other nodes, if any."
  (destructuring-bind (key value &rest other) args
    (setf (gethash key (kvs node) value))))

(defmethod do-message ((kind (eql :inquire)) (node gossip-node) args)
  "Inquire as to the value of a key on a node. If this node has no further
   neighbors, just return its value. Otherwise collect responses from subnodes."
  (gethash (car args) (kvs node) value))

(defmethod do-message ((kind (eql :max)) (node gossip-node) args)
  "Retrieve maximum value of a given key on all the nodes"
  )

(defmethod do-message ((kind (eql :min)) (node gossip-node) &rest args)
  "Retrieve minimum value of a given key on all the nodes"
  )

#+CCL
(defun my-prf (fn &rest keys)
  "So we can debug background processes in gui CCL. Also works in command-line CCL."
  (if (find-package :gui)
    (funcall (intern "BACKGROUND-PROCESS-RUN-FUNCTION" :gui) keys fn)
    (process-run-function keys fn)))

(defun dispatch-msg (msg src-uid dest-uid)
  (let ((srcnode (lookup-node src-uid))
        (destnode (lookup-node dest-uid)))
    (receive-message sol destnodesrcnode)))

(defun dispatcher-loop ()
  (let ((nextmsg nil))
    (loop until *end-simulation* do
      (with-lock-grabbed (*message-space-lock*)
        (setf nextmsg (deq *message-space*)))
      (if nextmsg
          (apply 'dispatch-msg nextmsg)
          (sleep .1)))))

(defun stop-gossip-sim ()
  (setf *end-simulation* t))

(defun run-gossip-sim ()
  (stop-gossip-sim) ; stop old simulation, if any
  ; Create gossip network
  ; Clear message space
  ; Archive the current log and clear it
  (vector-push-extend *log* *archived-logs*)
  (setf *log* (new-log))
  ; Start daemon that dispatches messages to nodes
  (setf *end-simulation* nil)
  (my-prf (lambda () (dispatcher-loop)) :name "Dispatcher Loop")
  )

