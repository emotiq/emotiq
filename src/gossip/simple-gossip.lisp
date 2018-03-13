;;; simple-gossip.lisp
;;; 8-Mar-2018 SVS

;;; Simple gossip protocol for experimentation.
;;;   No crypto. Just for gathering metrics.

(in-package :gossip)

(defparameter *max-message-age* 10 "Messages older than this number of seconds will be ignored")

(defvar *last-uid* 0 "Simple counter for making UIDs")

(defun new-uid ()
  (incf *last-uid*))

(defun make-uid-mapper ()
  "Returns a table that maps UIDs to objects"
  (make-hash-table :test 'equal))

(defclass uid-mixin ()
  ((uid :initarg :uid :initform (new-uid) :reader uid
        :documentation "Unique ID")))

(defclass message-mixin (uid-mixin)
  ((timestamp :initarg :timestamp :initform (get-universal-time) :accessor timestamp
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
  ((message-cache :initarg :message-cache :initform (make-uid-mapper) :accessor message-cache
                  :documentation "Cache of seen messages")
   (kvs :initarg :kvs :initform (make-hash-table) :accessor kvs
        :documentation "Local key/value store for this node")
   (neighbors :initarg :neighbors :initform nil :accessor neighbors
              :documentation "Set of UIDs of direct neighbors of this node")
   (logfn :initarg :logfn :initform 'default-logging-function :accessor logfn
          :documentation "If non-nil, assumed to be a function called with every
              message seen to log it.")))

(defun make-node (&rest args)
  "Makes a new node"
  (let ((node (apply 'make-instance 'gossip-node args)))
    (setf (gethash (uid node) *nodes*) node)
    node))

; Logcmd: Keyword that describes what a node has done with a given message UID
; Examples: :IGNORE, :ACCEPT, :FORWARD, etc.

(defmethod maybe-log ((node gossip-node) logcmd uid &rest args)
  (when (logfn node)
    (apply (logfn node) logcmd uid args)))

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
(defvar *message-space-lock* (ccl:make-lock) "Just a lock to manage access to the message space")
(defvar *nodes* (make-uid-mapper) "Table for mapping node UIDs to nodes")

(defun lookup-node (uid)
  (gethash uid *nodes*))

(defun new-log ()
  "Returns a new log space"
  (make-array 10 :adjustable t :fill-pointer 0))

(defvar *archived-logs* (make-array 10 :adjustable t :fill-pointer 0) "Previous historical logs")

(defvar *log* (new-log) "Log of simulated actions.")

(defun default-logging-function (logcmd uid &rest args)
  (vector-push-extend (list* logcmd uid args) *log*))

(defmethod send-msg ((msg solicitation) dest-uid src-uid)
  "Abstraction for asynchronous message sending. Post a message with src-id and dest-id onto
  a global space that all nodes can read from."
  (with-lock-grabbed (*message-space-lock*)
    (enq (list msg dest-uid src-uid) *message-space*)))

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
                  (let* ((kind (kind sol))
                         (kindsym nil))
                    (when kind (setf kindsym (intern (symbol-name kind) :gossip)))
                    (when (and kindsym
                               (fboundp kindsym))
                      (funcall kindsym sol destnode (uid srcnode))))))))
        (t (maybe-log destnode :ignore (uid sol) :too-old))))

(defun forward (msg srcuid destuids)
  "Sends msg from srcuid to multiple destuids"
  (mapc (lambda (destuid)
          (send-msg msg destuid srcuid))
        destuids))

(defmethod assign (msg destnode srcuid)
  "Establishes a key/value pair on this node and forwards to other nodes, if any."
  (let ((key (first (args msg)))
        (value (second (args msg))))
    (setf (gethash key (kvs destnode)) value)
    ; destnode becomes new source for forwarding purposes
    (forward msg destnode (remove srcuid (neighbors destnode)))))

(defmethod inquire (msg destnode srcuid)
  "Inquire as to the value of a key on a node. If this node has no further
   neighbors, just return its value. Otherwise collect responses from subnodes."
  (gethash (car (args msg)) (kvs destnode)))

(defmethod find-max (msg destnode srcuid)
  "Retrieve maximum value of a given key on all the nodes"
  )

(defmethod find-min (msg destnode srcuid)
  "Retrieve minimum value of a given key on all the nodes"
  )

#+CCL
(defun my-prf (fn &rest keys)
  "So we can debug background processes in gui CCL. Also works in command-line CCL."
  (if (find-package :gui)
    (funcall (intern "BACKGROUND-PROCESS-RUN-FUNCTION" :gui) keys fn)
    (ccl:process-run-function keys fn)))

(defun dispatch-msg (msg dest-uid src-uid)
  "Call receive-message on message for node with given dest-uid."
  (let ((srcnode (lookup-node src-uid))
        (destnode (lookup-node dest-uid)))
    (receive-message msg destnode srcnode)))

(defun dispatcher-loop ()
  (let ((nextmsg nil))
    (loop until *end-simulation* do
      (ccl:with-lock-grabbed (*message-space-lock*)
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

