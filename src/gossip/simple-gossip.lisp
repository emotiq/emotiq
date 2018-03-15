;;; simple-gossip.lisp
;;; 8-Mar-2018 SVS

;;; Simple gossip protocol for experimentation.
;;;   No crypto. Just for gathering metrics.

(in-package :gossip)

(defparameter *max-message-age* 10 "Messages older than this number of seconds will be ignored")

; TODO: Initiator node of message should wait longer than nodes farther away.
(defparameter *max-seconds-to-wait* 2 "Max seconds to wait for all replies to come in")

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
            Kind will dictate nature of reply. Not sure we need this, since whether
             a reply is needed is implicit in kind.")))

(defun make-solicitation (&rest args)
  (apply 'make-instance 'solicitation args))

; Note: If you change a message before forwarding it, you need to create a new
;   message with a new UID. (Replies can often be changed as they percolate backwards;
;   they need new UIDs so that a node that has seen one set of information won't
;   automatically ignore it.)
(defclass reply (message-mixin)
  ((solicitation-uid :initarg :solicitation-uid :initform nil :accessor solicitation-uid
                     :documentation "UID of solicitation message that elicited this reply."))
  (:documentation "Reply message. Used to reply to solicitations that need a reply.
     It is common and expected to receive multiple replies matching a given solicitation-uid.
     Most replies are 'inverse broadcasts' in that they have many sources that funnel
     back to one ultimate receiver -- the originator of the solicitation. However,
     a few replies (e.g. to sound-off) are full broadcasts unto themselves."))

(defun make-reply (&rest args)
  (apply 'make-instance 'reply args))

(defclass gossip-mixin (uid-mixin)
  ((address :initarg :address :initform nil :accessor address
            :documentation "Network address (e.g. IP) of node.")))
   
(defclass gossip-node (gossip-mixin)
  ((message-cache :initarg :message-cache :initform (make-uid-mapper) :accessor message-cache
                  :documentation "Cache of seen messages")
   (repliers-expected :initarg :repliers-expected :initform (make-hash-table :test 'equal)
                      :accessor repliers-expected
                      :documentation "Hash-table mapping a solicitation id to a list of node UIDs
                          that I expect to reply to that solicitation")
   (reply-data :initarg :reply-data :initform (make-hash-table :test 'equal)
               :accessor reply-data
               :documentation "Hash-table mapping a solicitation id to some data being accumulated
                          from replies for that solicitation")
   (kvs :initarg :kvs :initform (make-hash-table) :accessor kvs
        :documentation "Local key/value store for this node")
   (neighbors :initarg :neighbors :initform nil :accessor neighbors
              :documentation "List of UIDs of direct neighbors of this node")
   (logfn :initarg :logfn :initform 'default-logging-function :accessor logfn
          :documentation "If non-nil, assumed to be a function called with every
              message seen to log it.")))

; We'll use these for real (not simulated on one machine) protocol
(defclass remote-gossip-node (gossip-mixin)
  ()
  (:documentation "A local [to this process] standin for a gossip-node located elsewhere.
              All we know about it is its UID and address, which is enough to transmit a message to it."))

(defun make-node (&rest args)
  "Makes a new node"
  (let ((node (apply 'make-instance 'gossip-node args)))
    (setf (gethash (uid node) *nodes*) node)
    node))

(defun make-nodes (numnodes)
  (dotimes (i numnodes)
    (make-node)))

(defun listify-nodes (&optional (nodetable *nodes*))
  (loop for node being each hash-value of nodetable collect node))

(defmethod connect ((node1 gossip-node) (node2 gossip-node))
  (pushnew (uid node1) (neighbors node2))
  (pushnew (uid node2) (neighbors node1)))
  
(defmethod connected? ((node1 gossip-node) (node2 gossip-node))
  (or (member (uid node2) (neighbors node1) :test 'equal)
      ; redundant if we connected the graph correctly in the first place
      (member (uid node1) (neighbors node2) :test 'equal)))

(defun linear-path (nodelist)
  "Create a linear path through the nodes"
  (when (second nodelist)
    (connect (first nodelist) (second nodelist))
    (linear-path (cdr nodelist))))

(defun random-connection (nodelist)
  (let* ((len (length nodelist))
         (node1 (elt nodelist (random len)))
         (node2 (elt nodelist (random len))))
    (if (eq node1 node2)
        (random-connection nodelist)
        (values node1 node2))))

(defun random-new-connection (nodelist)
  (multiple-value-bind (node1 node2) (random-connection nodelist)
    (if (connected? node1 node2)
        (random-new-connection nodelist)
        (values node1 node2))))

(defun add-random-connections (nodelist n)
  "Adds n random edges between pairs in nodelist, where no connection currently exists."
  (dotimes (i n)
    (multiple-value-bind (node1 node2) (random-new-connection nodelist)
      (connect node1 node2))))

(defun make-graph (numnodes)
  (clrhash *nodes*)
  (make-nodes numnodes)
  (let ((nodelist (listify-nodes)))
    (linear-path nodelist)
    (add-random-connections nodelist (length nodelist))
    ))



; (make-graph 10)
; (graph-nodes (listify-nodes))


; Logcmd: Keyword that describes what a node has done with a given message UID
; Examples: :IGNORE, :ACCEPT, :FORWARD, etc.

(defmethod maybe-log ((node gossip-node) logcmd uid &rest args)
  (when (logfn node)
    (apply (logfn node) logcmd uid args)))

(defparameter *stop-dispatcher* nil "Set to true to end an ongoing simulation")

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

(defvar *message-space* (make-queue) "Queue of outgoing messages from local machine.")
(defvar *message-space-lock* (ccl:make-lock) "Just a lock to manage access to the message space")
(defvar *nodes* (make-uid-mapper) "Table for mapping node UIDs to nodes known by local machine")

(defun lookup-node (uid)
  (gethash uid *nodes*))

(defun new-log ()
  "Returns a new log space"
  (make-array 10 :adjustable t :fill-pointer 0))

(defvar *archived-logs* (make-array 10 :adjustable t :fill-pointer 0) "Previous historical logs")

(defvar *log* (new-log) "Log of simulated actions.")

(defun default-logging-function (logcmd uid &rest args)
  (vector-push-extend (list* logcmd uid args) *log*))

(defmethod send-msg ((msg solicitation) destuid srcuid)
  "Abstraction for asynchronous message sending. Post a message with src-id and dest-id onto
  a global space that all local and simulated nodes can read from."
  ; Could just call deliver-msg here, but I like the abstraction of
  ;   having a local message-space which is used for all messages in simulation mode,
  ;   and all outgoing messages in 'real' network mode.
  (ccl:with-lock-grabbed (*message-space-lock*)
    (enq (list msg destuid srcuid) *message-space*)))

; TODO: Remove old entries in message-cache, eventually.
(defmethod locally-receive-msg ((sol solicitation) (thisnode gossip-node) srcuid)
  "Deal with an incoming solicitation. Srcuid could be nil in case of initiating messages."
  (cond ((< (get-universal-time) (+ *max-message-age* (timestamp sol))) ; ignore too-old messages
         (let ((already-seen? (gethash (uid sol) (message-cache thisnode))))
           (cond (already-seen? ; ignore if already seen
                  (maybe-log thisnode :ignore (uid sol) :already-seen))
                 (t
                  ; Remember the srcuid that sent me this message, because that's where reply will be forwarded to
                  (setf (gethash (uid sol) (message-cache thisnode)) srcuid)
                  (maybe-log thisnode :accepted (uid sol) (kind sol) (args sol))
                  (let* ((kind (kind sol))
                         (kindsym nil))
                    (when kind (setf kindsym (intern (symbol-name kind) :gossip)))
                    (when (and kindsym
                               (fboundp kindsym))
                      (funcall kindsym sol thisnode srcuid)))))))
        (t (maybe-log thisnode :ignore (uid sol) :too-old))))

(defmethod locally-receive-msg ((sol solicitation) (thisnode remote-gossip-node) srcuid)
  (error "Bug: Cannot locally-receive to a remote node!"))

(defmethod locally-receive-msg ((rep reply) (thisnode gossip-node) srcuid)
  "Deal with an incoming reply. Srcuid could be nil in case of initiating messages."
  (cond ((< (get-universal-time) (+ *max-message-age* (timestamp rep))) ; ignore too-old messages
         (let ((already-seen? (gethash (uid rep) (message-cache thisnode))))
           (cond (already-seen? ; ignore if already seen
                  (maybe-log thisnode :ignore (uid rep) :already-seen))
                 (t
                  ; Remember the srcuid that sent me this message, not that we really need it for incoming replies
                  (setf (gethash (uid rep) (message-cache thisnode)) srcuid)
                  (maybe-log thisnode :reply-accepted (uid rep) (kind rep) (args rep))
                  (let* ((kind (kind rep))
                         (kindsym nil))
                    (when kind (setf kindsym (intern (symbol-name kind) :gossip)))
                    (when (and kindsym
                               (fboundp kindsym))
                      (funcall kindsym rep thisnode srcuid)))))))
        (t (maybe-log thisnode :ignore (uid rep) :too-old))))


(defun forward (msg srcuid destuids)
  "Sends msg from srcuid to multiple destuids"
  (mapc (lambda (destuid)
          (send-msg msg destuid srcuid))
        destuids))

(defmethod assign ((msg solicitation) thisnode srcuid)
  "Establishes a global key/value pair. Sets value on this node and then forwards 
   solicitation to other nodes, if any. This is a destructive operation --
   any node that currently has a value for the given key will have that value replaced.
   No reply expected."
  (let ((key (first (args msg)))
        (value (second (args msg))))
    (setf (gethash key (kvs thisnode)) value)
    ; thisnode becomes new source for forwarding purposes
    (forward msg thisnode (remove srcuid (neighbors thisnode)))))

(defmethod remove-key ((msg solicitation) thisnode srcuid)
  "Remove a global key/value pair. Removes key/value pair on this node and then forwards 
   solicitation to other nodes, if any. This is a destructive operation --
   any node that currently has the given key will have that key/value removed.
   No reply expected."
  (let ((key (first (args msg))))
    (remhash key (kvs thisnode))
    ; thisnode becomes new source for forwarding purposes
    (forward msg thisnode (remove srcuid (neighbors thisnode)))))

(defmethod inquire ((msg solicitation) thisnode srcuid)
  "Inquire as to the global value of a key. If this node has no further
   neighbors, just return its value. Otherwise collect responses from subnodes.
   Reply is of course expected.
   Reply here is somewhat complicated: Reply will be an alist of ((value1 . n1) (value2 .n2) ...) where
   value1 is value reported by n1 nodes downstream of thisnode,
   value2 is value reported by n2 nodes downstream of thisnode, etc."
  (let ((myvalue (gethash (car (args msg)) (kvs thisnode)))
        (srcid (gethash (uid sol) (message-cache thisnode))))
    ))

(defmethod find-max (msg thisnode srcuid)
  "Retrieve maximum value of a given key on all the nodes"
  )

(defmethod find-min (msg thisnode srcuid)
  "Retrieve minimum value of a given key on all the nodes"
  )

(defmethod sound-off (msg thisnode srcuid)
  "Broadcast a request that all nodes execute sound-off with some status information"
  )


(defgeneric sounding-off (msg thisnode srcuid)
  (:documentation "A broadcast message initiated by a particular origin node (UID contained in msg)
   containing specific requested or unprompted status information.
   Usually a response to a sound-off request, but can also be used
   by a node to announce important status information such as going offline,
   leaving the group, or joining the group."))

(defmethod sounding-off ((msg reply) thisnode srcuid)
  "A broadcast message initiated by a particular origin node (UID contained in msg)
   containing specific requested status information.
   Always a response to a sound-off request, which will be in the solicitation-uid field.
   Unlike most replies, this one is forwarded as a true broadcast rather than an inverse broadcast."
  )

(defmethod sounding-off ((msg solicitation) thisnode srcuid)
  "A broadcast message initiated by a particular origin node (UID contained in msg)
   containing specific unprompted status information.
   Can be used by a node to announce important status information such as going offline,
   leaving the group, or joining the group."
  )

(defmethod count-alive ((msg solicitation) thisnode srcuid)
  "Counts number of live nodes downstream of thisnode, plus thisnode itself.
  Reply with a scalar."
  (let ((soluid (uid msg)))
    ; prepare reply tables
    (setf (gethash soluid (reply-data thisnode)) 1)
    (setf (gethash soluid (repliers-expected thisnode)) (neighbors thisnode))
    (forward msg thisnode (remove srcuid (neighbors thisnode)))
    ; wait a finite time for all replies
    (ccl:process-wait-with-timeout "reply-wait"
                                   (* *max-seconds-to-wait* internal-time-units-per-second)
                                   (lambda ()
                                     (null (gethash soluid (repliers-expected thisnode)))))
    (let ((totalcount (gethash soluid (reply-data thisnode)))
          (where-to-forward-reply (gethash (uid msg) (message-cache thisnode))))
      ; clean up reply tables
      (remhash soluid (reply-data thisnode))
      (remhash soluid (repliers-expected thisnode))
      ; must create a new reply here; cannot reuse an old one because its content has changed
      (send-msg (make-reply :solicitation-id soluid
                            :kind :count-alive
                            :args (list totalcount))
                where-to-forward-reply
                (uid thisnode)))))

(defmethod count-alive ((rep reply) thisnode srcuid)
  (let ((soluid (solicitation-uid rep)))
    ; First record the data in the reply appropriately
    (let ((numalive (first (args rep))))
      (incf (gethash soluid (reply-data thisnode)) numalive))
    ; Now remove this srcuid from expected-replies list. (We know it's on
    ;  the list because this method would never have been called otherwise.)
    (let ((nodes-expected-to-reply (gethash soluid (repliers-expected thisnode))))
      (setf (gethash soluid (repliers-expected thisnode))
            (delete srcuid nodes-expected-to-reply)))))

(defmethod find-address-for-node ((msg solicitation) thisnode srcuid)
  "Find address for a node with a given uid. Equivalent to DNS lookup."
  ;(forward msg thisnode (remove srcuid (neighbors thisnode)))
  ; wait a finite time for all replies
  )

(defmethod find-node-with-address ((msg solicitation) thisnode srcuid)
  "Find address for a node with a given uid. Equivalent to reverse DNS lookup."
  ;(forward msg thisnode (remove srcuid (neighbors thisnode)))
  ; wait a finite time for all replies
  )

#+CCL
(defun my-prf (fn &rest keys)
  "So we can debug background processes in gui CCL. Also works in command-line CCL."
  (if (find-package :gui)
    (funcall (intern "BACKGROUND-PROCESS-RUN-FUNCTION" :gui) keys fn)
    (ccl:process-run-function keys fn)))

; NOT DONE YET
(defmethod transmit-msg (msg (node remote-gossip-node) srcuid)
  "Send message across network"
  )

; NOT DONE YET
(defun parse-raw-message (raw-message-string)
  "Deserialize a raw message string, srcuid, and destuid.
   Error on failure."
  ;(values msg srcuid destuid)
  )

; NOT DONE YET
(defun message-listener-daemon ()
  "Listen for messages received from network and dispatch them locally"
  (let ((rawmsg (listen-for-message)))
    (multiple-value-bind (msg srcuid destuid)
                         (parse-raw-message rawmsg)
      (let ((destnode (lookup-node destuid)))
        (locally-receive-msg msg destnode srcuid)))))

(defmethod deliver-msg (msg (node gossip-node) srcuid)
   (locally-receive-msg msg node srcuid))

(defmethod deliver-msg (msg (node remote-gossip-node) srcuid)
  "This node is a standin for a remote node. Transmit message across network."
   (transmit-msg msg node srcuid))

(defun dispatch-msg (msg destuid srcuid)
  "Call deliver-message on message for node with given destuid."
  (let ((destnode (lookup-node destuid)))
    (deliver-msg msg destnode srcuid)))

(defun dispatcher-loop ()
  "Process outgoing messages on this machine. Same code used in both simulation and 'real' modes."
  (setf *stop-dispatcher* nil)
  (let ((nextmsg nil))
    (loop until *stop-dispatcher* do
      (ccl:with-lock-grabbed (*message-space-lock*)
        (setf nextmsg (deq *message-space*)))
      (if nextmsg
          ; Process messages as fast as possible.
          (apply 'dispatch-msg nextmsg)
          ; But if no messages to process, wait a bit.
          (sleep .1)))))

(defun stop-gossip-sim ()
  (setf *stop-dispatcher* t))

(defun run-gossip-sim ()
  (stop-gossip-sim) ; stop old simulation, if any
  ; Create gossip network
  ; Clear message space
  ; Archive the current log and clear it
  (vector-push-extend *log* *archived-logs*)
  (setf *log* (new-log))
  ; Start daemon that dispatches messages to nodes
  (my-prf (lambda () (dispatcher-loop)) :name "Dispatcher Loop")
  )

