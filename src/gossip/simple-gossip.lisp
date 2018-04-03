;;; simple-gossip.lisp
;;; 8-Mar-2018 SVS

;;; Simple gossip protocol for experimentation.
;;;   No crypto. Just for gathering metrics.

(in-package :gossip)

(defparameter *max-message-age* 10 "Messages older than this number of seconds will be ignored")

(defparameter *max-seconds-to-wait* 10 "Max seconds to wait for all replies to come in")
(defparameter *seconds-to-wait* *max-seconds-to-wait* "Seconds to wait for a particular reply")
(defparameter *hop-factor* 0.9 "Decrease *seconds-to-wait* by this factor for every added hop. Must be less than 1.0.")

(defvar *nodes* (make-uid-mapper) "Table for mapping node UIDs to nodes known by local machine")

(defvar *last-uid* 0 "Simple counter for making UIDs")

(defun new-uid ()
  (incf *last-uid*))

(defun uid? (thing)
  (integerp thing))

(defun make-uid-mapper ()
  "Returns a table that maps UIDs to objects"
  (kvs:make-store ':hashtable :test 'equal))

(defclass uid-mixin ()
  ((uid :initarg :uid :initform (new-uid) :reader uid
        :documentation "Unique ID")))

(defmethod print-object ((thing uid-mixin) stream)
   (with-slots (uid) thing
       (print-unreadable-object (thing stream :type t :identity t)
          (when uid (princ uid stream)))))

;;; Should move these to mpcompat
(defun all-processes ()
  #+Allegro   mp:*all-processes*
  #+LispWorks (mp:list-all-processes)
  #+CCL       (ccl:all-processes)
  #-(or Allegro LispWorks CCL)
  (warn "No implementation of ALL-PROCESSES for this system.")
  )

(defun process-name (process)
  #+Allegro                (mp:process-name process)
  #+LispWorks              (mp:process-name process)
  #+CCL                    (ccl:process-name process)
  #-(or Allegro LispWorks CCL)
  (warn "No implementation of PROCESS-NAME for this system."))

(defun process-kill (process)
  #+Allegro                (mp:process-kill process)
  #+LispWorks              (mp:process-terminate process)
  #+CCL                    (ccl:process-kill process)
  #-(or Allegro LispWorks CCL)
  (warn "No implementation of PROCESS-KILL for this system."))

(defclass message-mixin (uid-mixin)
  ((timestamp :initarg :timestamp :initform (get-universal-time) :accessor timestamp
              :documentation "Timestamp of message origination")
   (hopcount :initarg :hopcount :initform 0 :accessor hopcount
             :documentation "Number of hops this message has traversed.")
   (kind :initarg :kind :initform nil :accessor kind
         :documentation "The verb of the message, indicating what action to take.")
   (args :initarg :args :initform nil :accessor args
         :documentation "Payload of the message. Arguments to kind.")))

(defgeneric copy-message (msg)
  (:documentation "Copies a message object verbatim. Mainly for simulation mode
          where messages are shared, in which case in order to increase hopcount,
          the message needs to be copied."))

(defmethod copy-message ((msg message-mixin))
  (let ((new-msg (make-instance (class-of msg)
                   :uid (uid msg)
                   :timestamp (timestamp msg)
                   :hopcount (hopcount msg)
                   :kind (kind msg)
                   :args (args msg))))
    new-msg))

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

(defmethod copy-message :around ((msg reply))
  (let ((new-msg (call-next-method)))
    (setf (solicitation-uid new-msg) (solicitation-uid msg))
    new-msg))

(defun make-reply (&rest args)
  (apply 'make-instance 'reply args))

(defclass gossip-mixin (uid-mixin)
  ((address :initarg :address :initform nil :accessor address
            :documentation "Network address (e.g. IP) of node.")))
   
(defclass gossip-node (gossip-mixin)
  ((message-cache :initarg :message-cache :initform (make-uid-mapper) :accessor message-cache
                  :documentation "Cache of seen messages. Table mapping UID of message to UID of sender.
                  Used to ensure identical messages are not acted on twice, and to determine where
                  replies to a message should be sent.")
   (reply-table-lock :initarg :reply-table-lock :initform (mpcompat:make-lock) :accessor reply-table-lock
                     :documentation "Lock for repliers-expected and reply-data. Changes to those must be
                     atomic to avoid a race condition in the simulator.")
   (repliers-expected :initarg :repliers-expected :initform (kvs:make-store ':hashtable :test 'equal)
                      :accessor repliers-expected
                      :documentation "Hash-table mapping a solicitation id to a list of node UIDs
                  that I expect to reply to that solicitation")
   (reply-data :initarg :reply-data :initform (kvs:make-store ':hashtable :test 'equal)
               :accessor reply-data
               :documentation "Hash-table mapping a solicitation id to some data being accumulated
                  from replies for that solicitation")
   (local-kvs :initarg :local-kvs :initform (kvs:make-store ':hashtable :test 'equal) :accessor local-kvs
        :documentation "Local key/value store for this node")
   (neighbors :initarg :neighbors :initform nil :accessor neighbors
              :documentation "List of UIDs of direct neighbors of this node")
   (logfn :initarg :logfn :initform 'default-logging-function :accessor logfn
          :documentation "If non-nil, assumed to be a function called with every
              message seen to log it.")))

(defmethod clear-caches ((node gossip-node))
  "Caches should be cleared in the normal course of events, but this can be used to make sure."
  (kvs:clear-store! (message-cache node))
  (kvs:clear-store! (repliers-expected node))
  (kvs:clear-store! (reply-data node))
  ; don't clear the local-kvs. That should be persistent.
  )

; We'll use these for real (not simulated on one machine) protocol
(defclass remote-gossip-node (gossip-mixin)
  ()
  (:documentation "A local [to this process] standin for a gossip-node located elsewhere.
              All we know about it is its UID and address, which is enough to transmit a message to it."))

(defun make-node (&rest args)
  "Makes a new node"
  (let ((node (apply 'make-instance 'gossip-node args)))
    (kvs:relate-unique! *nodes* (uid node) node)
    node))

(defun make-nodes (numnodes)
  (dotimes (i numnodes)
    (make-node)))

(defun listify-nodes (&optional (nodetable *nodes*))
  (loop for node being each hash-value of nodetable collect node))

(defmethod connect ((node1 gossip-node) (node2 gossip-node))
  "Establish an edge between two nodes. Because every node must know its nearest neighbors,
   we store the edge information twice: Once in each endpoint node."
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

(defun as-hash-table (test alist)
  "Builds a hash table from an alist"
  (let ((ht (make-hash-table :test test)))
    (dolist (pair alist)
      (setf (gethash (car pair) ht) (cdr pair)))
    ht))

(defmethod readable-value ((me t))
  (let ((*package* (find-package :gossip)))
    (format nil "~S" me)))

(defmethod readable-value ((me symbol))
  (let ((*package* (find-package :gossip)))
    (format nil "'~S" me)))

(defmethod alistify-hashtable ((table hash-table))
   (loop for key being each hash-key of table using (hash-value val) collect (cons key val)))

(defmethod readable-value ((me hash-table))
  (with-output-to-string (s)
    (format s "(as-hash-table~%")
    (format s "~T~T~A~%" (readable-value (hash-table-test me)))
    (format s "~T~T~A)~%" (readable-value (alistify-hashtable me)))))

(defmethod readable-value ((me list))
  (let ((*package* (find-package :gossip)))
    (format nil "'~S" me)))

(defmethod save-node ((node gossip-node) stream)
  "Write a form to stream that will reconstruct given node"
  (let ((*package* (find-package :gossip)))
    (flet ((write-slot (initarg value) ; assumes accessors and initargs are named the same
             (format stream "~%  ~S ~A" initarg (readable-value value))))
      (format stream "(make-node")
      (write-slot :uid (uid node))
      (write-slot :address (address node))
      (write-slot :neighbors (neighbors node))
      (write-slot :logfn (logfn node))
      (write-slot :kvs (local-kvs node))
      (format stream "~T)~%"))))

(defun save-graph (stream &optional (nodetable *nodes*))
  (format stream "(in-package :gossip)~%~%")
  (loop for node being each hash-value of nodetable do
    (save-node node stream)))

(defun save-graph-to-file (pathname &optional (nodetable *nodes*))
  "Save current graph to file. Restore by calling restore-graph-from-file.
   Makes experimentation easier."
  (with-open-file (stream pathname :direction :output)
    (format stream ";;; ~A~%" (file-namestring pathname))
    (format stream ";;; Saved graph file.~%")
    (format stream ";;; Call gossip::restore-graph-from-file on this file to restore graph from it.~%~%")
    (save-graph stream nodetable))
  pathname)

(defun restore-graph-from-file (pathname)
  "Restores a graph from a file saved by save-graph-to-file."
  (clrhash *nodes*)
  (load pathname))

(defun make-graph (numnodes &optional (fraction 0.5))
  "Build a graph with numnodes nodes. Strategy here is to first connect all the nodes in a single
   non-cyclic linear path, then add f random edges, where f is multiplied by the number of nodes."
  (clrhash *nodes*)
  (make-nodes numnodes)
  (let ((nodelist (listify-nodes)))
    ; following guarantees a single connected graph
    (linear-path nodelist)
    ; following --probably-- makes the graph an expander but we'll not try to guarantee that for now
    (add-random-connections nodelist (round (* fraction (length nodelist))))))

(defun solicit (node kind &rest args)
  "Send a solicitation to the network starting with given node. This is the primary interface to 
  the network to start an action from the outside. Nodes shouldn't use this function to initiate an
  action because they should set the srcuid parameter to be their own rather than nil."
  (unless node
    (error "No destination node supplied. You might need to run make-graph or restore-graph-from-file first."))
  (let ((uid (if (typep node 'gossip-node) ; yeah this is kludgy.
                 (uid node)
                 node))
        (node (if (typep node 'gossip-node)
                  node
                  (lookup-node node))))
    (setf (logfn node) 'interactive-logging-function)
    (send-msg (make-solicitation
               :kind kind
               :args args)
              uid        ; destination
              nil)))      ; srcuid

(defmethod briefname ((node gossip-node) &optional (prefix "node"))
 (format nil "~A~D" prefix (uid node)))

(defmethod briefname ((msg solicitation) &optional (prefix "sol"))
  (format nil "~A~D" prefix (uid msg)))

(defmethod briefname ((msg reply) &optional (prefix "rep"))
  (format nil "~A~D" prefix (uid msg)))

(defmethod briefname ((id integer) &optional (prefix ""))
  (format nil "~A~D" prefix id))

(defmethod briefname ((id null) &optional (prefix ""))
  (declare (ignore prefix))
  "nil")

; Logcmd: Keyword that describes what a node has done with a given message UID
; Examples: :IGNORE, :ACCEPT, :FORWARD, etc.
(defmethod maybe-log ((node gossip-node) logcmd msg &rest args)
  (when (logfn node)
    (apply (logfn node)
           logcmd
           (briefname node)
           (briefname msg)
           args)))

(defparameter *stop-dispatcher* nil "Set to true to end an ongoing simulation")

; Graham's Basic queue
(defun make-queue ()
  (cons nil nil))

(defvar *message-space* (make-queue) "Queue of outgoing messages from local machine.")
(defvar *message-space-lock* (mpcompat:make-lock) "Just a lock to manage access to the message space")

(defun enq (obj q)
  (if (null (car q))
    (setf (cdr q) (setf (car q) (list obj)))
    (setf (cdr (cdr q)) (list obj)
          (cdr q) (cdr (cdr q))))
  (car q))

(defun deq (q)
  (pop (car q)))

(defun lookup-node (uid)
  (kvs:lookup-key *nodes* uid))

(defun new-log ()
  "Returns a new log space"
  (make-array 10 :adjustable t :fill-pointer 0))

(defvar *archived-logs* (make-array 10 :adjustable t :fill-pointer 0) "Previous historical logs")

(defvar *log* (new-log) "Log of simulated actions.")

(defun default-logging-function (logcmd nodename msgname &rest args)
  (let ((logmsg (list* logcmd nodename msgname args)))
    (vector-push-extend logmsg *log*)
    logmsg))

(defun interactive-logging-function (logcmd nodename msgname &rest args)
  "Use this logging function for interactive debugging. You'll probably only want to use this
  in the mode you called #'solicit on."
  (let* ((logmsg (apply 'default-logging-function logcmd nodename msgname args))
         (logstring (format nil "~S~%" logmsg)))
    #+CCL
    (if (find-package :hi)
        (funcall (intern "WRITE-TO-TOP-LISTENER" :hi) logstring)
        (write-string logstring *standard-output*))
    #-CCL
    (write-string logstring *standard-output*)))
  
(defmethod send-msg ((msg message-mixin) destuid srcuid)
  "Abstraction for asynchronous message sending. Post a message with src-id and dest-id onto
  a global space that all local and simulated nodes can read from."
  ; Could just call deliver-msg here, but I like the abstraction of
  ;   having a local message-space which is used for all messages in simulation mode,
  ;   and all outgoing messages in 'real' network mode.
  (unless (or (null srcuid)
              (numberp srcuid))
    (break "In send-msg"))
  (mpcompat:with-lock (*message-space-lock*)
    (enq (list msg destuid srcuid) *message-space*)))

(defmethod locally-receive-msg :around (msg thisnode srcuid &optional (kindsym nil))
  (declare (ignore thisnode srcuid kindsym))
  (let ((*seconds-to-wait* (* *max-seconds-to-wait* (expt *hop-factor* (hopcount msg)))))
    (when (> *seconds-to-wait* *max-seconds-to-wait*)
      (error "*hop-factor* must be less than 1.0"))
    (call-next-method)))

(defmethod accept-msg? ((msg message-mixin) (thisnode gossip-node) srcuid)
  "Returns kindsym if this message should be accepted by this node, nil otherwise.
   Kindsym is the name of the gossip method that should be called to handle this message.
   Doesn't change anything in message or node."
  (cond ((> (get-universal-time) (+ *max-message-age* (timestamp msg))) ; ignore too-old messages
         (maybe-log thisnode :ignore msg :from (briefname srcuid "node") :too-old)
         nil)
        (t
         (let ((already-seen? (kvs:lookup-key (message-cache thisnode) (uid msg))))
           (cond (already-seen? ; ignore if already seen
                  (maybe-log thisnode :ignore msg :from (briefname srcuid "node") :already-seen)
                  nil)
                 (t ; it's a new message
                  (let* ((kind (kind msg))
                         (kindsym nil))
                    (cond (kind
                           (setf kindsym (intern (symbol-name kind) :gossip))
                           (if (and kindsym (fboundp kindsym))
                               kindsym ;; SUCCESS! We accept the message.
                               (progn
                                 (maybe-log thisnode :ignore msg :from (briefname srcuid "node") :unknown-kind kindsym)
                                 nil)))
                          (t
                           (maybe-log thisnode :ignore msg :from (briefname srcuid "node") :no-kind)
                           nil)))))))))

(defmethod accept-msg? ((msg reply) (thisnode gossip-node) srcuid)
  (let ((kindsym (call-next-method))) ; the one on message-mixin
    ; Also ensure this reply is actually expected
    (cond ((and kindsym
                (mpcompat:with-lock ((reply-table-lock thisnode))
                  (member srcuid (kvs:lookup-key  (repliers-expected thisnode) (solicitation-uid msg)))))
           kindsym)
          (t (maybe-log thisnode :ignore msg :from (briefname srcuid "node") :unexpected)
             nil))))

; TODO: Remove old entries in message-cache, eventually.
;       Might want to also check hopcount and reject message where it's too big.
(defmethod locally-receive-msg ((sol solicitation) (thisnode gossip-node) srcuid &optional (kindsym nil))
  "Deal with an incoming solicitation. Srcuid could be nil in case of initiating messages."
  (unless kindsym
    (setf kindsym (accept-msg? sol thisnode srcuid)))
  (when kindsym
    (maybe-log thisnode :accepted sol (kind sol) :from (briefname srcuid "node") (args sol))
    (funcall kindsym sol thisnode srcuid)))

(defmethod locally-receive-msg ((sol t) (thisnode remote-gossip-node) srcuid &optional (kindsym nil))
  (declare (ignore srcuid kindsym))
  (error "Bug: Cannot locally-receive to a remote node!"))

(defmethod locally-receive-msg ((msg reply) (thisnode gossip-node) srcuid &optional (kindsym nil))
  "Deal with an incoming reply. Srcuid could be nil in case of initiating messages."
  (unless kindsym
    (setf kindsym (accept-msg? msg thisnode srcuid)))
  (when kindsym
    (maybe-log thisnode :reply-accepted msg (kind msg) :from (briefname srcuid "node") (args msg))
    (funcall kindsym msg thisnode srcuid)))

(defun forward (msg srcuid destuids)
  "Sends msg from srcuid to multiple destuids"
  (unless (uid? srcuid) (setf srcuid (uid srcuid)))
  (mapc (lambda (destuid)
          (send-msg msg destuid srcuid))
        destuids))

;;;  GOSSIP METHODS. These handle specific gossip protocol solicitations and replies.

(defmethod announce ((msg solicitation) thisnode srcuid)
  "Announce a message to the collective. First arg of Msg is the announcement,
   which can be any Lisp object. Recipient nodes are not expected to reply.
   This is probably only useful for debugging gossip protocols, since the only
   record of the announcement will be in the log."
  (let ((content (first (args msg))))
    (declare (ignore content))
    ; thisnode becomes new source for forwarding purposes
    (forward msg thisnode (remove srcuid (neighbors thisnode)))))

(defmethod relate ((msg solicitation) thisnode srcuid)
  "Establishes a global non-unique key/value pair. If key currently has a value or set of values,
   new value will be added to the set; it won't replace them.
  Sets value on this node and then forwards 
  solicitation to other nodes, if any.
  No reply expected."
  (destructuring-bind (key value &rest other) (args msg)
    (declare (ignore other))
    (setf (local-kvs thisnode) (kvs:relate (local-kvs thisnode) key value))
    ; thisnode becomes new source for forwarding purposes
    (forward msg thisnode (remove srcuid (neighbors thisnode)))))

(defmethod relate-unique ((msg solicitation) thisnode srcuid)
  "Establishes a global unique key/value pair. [Unique means there will be only one value for this key.]
  Sets value on this node and then forwards 
  solicitation to other nodes, if any. This is a destructive operation --
  any node that currently has a value for the given key will have that value replaced.
  No reply expected."
  (destructuring-bind (key value &rest other) (args msg)
    (declare (ignore other))
    (setf (local-kvs thisnode) (kvs:relate-unique (local-kvs thisnode) key value))
    ; thisnode becomes new source for forwarding purposes
    (forward msg thisnode (remove srcuid (neighbors thisnode)))))

(defmethod remove-key ((msg solicitation) thisnode srcuid)
  "Remove a global key/value pair. Removes key/value pair on this node and then forwards 
   solicitation to other nodes, if any. This is a destructive operation --
   any node that currently has the given key will have that key/value removed.
   No reply expected."
  (let ((key (first (args msg))))
    (setf (local-kvs thisnode) (kvs:remove-key (local-kvs thisnode) key))
    ; thisnode becomes new source for forwarding purposes
    (forward msg thisnode (remove srcuid (neighbors thisnode)))))

(defun multiple-tally (store alist)
  "Given a key/value store and an alist like ((key1 . n1) (key2 . n2) ...)
   call kvs:tally for all pairs in alist."
  (dolist (pair alist)
    (setf store (kvs:tally store (car pair) (cdr pair))))
  store)

(defmethod lookup-key ((msg solicitation) thisnode srcuid)
  "Inquire as to the global value of a key. If this node has no further
  neighbors, just return its value. Otherwise collect responses from subnodes.
  Reply is of course expected.
  Reply here is somewhat complicated: Reply will be an alist of ((value1 . n1) (value2 .n2) ...) where
  value1 is value reported by n1 nodes downstream of thisnode,
  value2 is value reported by n2 nodes downstream of thisnode, etc."
  (let* ((soluid (uid msg))
         (repliers-expected (remove srcuid (neighbors thisnode)))
         (key (first (args msg)))
         (myvalue (kvs:lookup-key (local-kvs thisnode) key)))
    ; prepare reply tables
    (mpcompat:with-lock ((reply-table-lock thisnode))
      (kvs:relate-unique! (reply-data thisnode) soluid (list (cons myvalue 1)))
      (kvs:relate-unique! (repliers-expected thisnode) soluid repliers-expected))
    (forward msg thisnode repliers-expected)
    ; wait a finite time for all replies
    (maybe-log thisnode :WAITING msg repliers-expected)
    (let ((win
           (mpcompat:process-wait-with-timeout "reply-wait"
                                               *seconds-to-wait*
                                               (lambda ()
                                                 (null (kvs:lookup-key (repliers-expected thisnode) soluid))))))
      (if win
          (maybe-log thisnode :DONE-WAITING-WIN msg)
          (maybe-log thisnode :DONE-WAITING-TIMEOUT msg))
      (let ((local-values nil)
            (where-to-forward-reply srcuid))
        ; clean up reply tables
        (mpcompat:with-lock ((reply-table-lock thisnode))
          (setf local-values (kvs:lookup-key (reply-data thisnode) soluid))
          (remhash soluid (repliers-expected thisnode))
          (remhash soluid (reply-data thisnode)))
        ; must create a new reply here; cannot reuse an old one because its content has changed
        (if (uid? where-to-forward-reply) ; should be a uid or T. Might be nil if there's a bug.
            (let ((reply (make-reply :solicitation-uid soluid
                                     :kind :lookup-key
                                     :args (list local-values))))
              (maybe-log thisnode :SEND-REPLY reply :to (briefname where-to-forward-reply "node") local-values)
              (send-msg reply
                        where-to-forward-reply
                        (uid thisnode)))
            ; if no place left to reply to, just log the result.
            ;   This can mean that thisnode autonomously initiated the request, or
            ;   somebody running the sim told it to.
            (maybe-log thisnode :FINALREPLY msg local-values))))))

(defmethod lookup-key ((rep reply) thisnode srcuid)
  (let ((soluid (solicitation-uid rep)))
    ; First record the data in the reply appropriately
    (let ((values-in-reply (first (args rep))))
      (mpcompat:with-lock ((reply-table-lock thisnode))
        (let ((local-values (kvs:lookup-key (reply-data thisnode) soluid)))
          (kvs:relate-unique! (reply-data thisnode) soluid (multiple-tally local-values values-in-reply)))))
    ; Now remove this srcuid from expected-replies list. (We know it's on
    ;  the list because this method would never have been called otherwise.)
    (let ((nodes-expected-to-reply (kvs:lookup-key (repliers-expected thisnode) soluid)))
      (kvs:relate-unique! (repliers-expected thisnode) soluid (delete srcuid nodes-expected-to-reply)))))

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
  "Get a list of UIDs of live nodes downstream of thisnode, plus that of thisnode itself."
  (let ((soluid (uid msg))
        (repliers-expected (remove srcuid (neighbors thisnode))))
    ; prepare reply tables
    (mpcompat:with-lock ((reply-table-lock thisnode))
      (kvs:relate-unique! (reply-data thisnode) soluid (list (uid thisnode))) ; thisnode itself is 1 live node
      (kvs:relate-unique! (repliers-expected thisnode) soluid repliers-expected))
    (forward msg thisnode repliers-expected)
    ; wait a finite time for all replies
    (maybe-log thisnode :WAITING msg repliers-expected)
    (let ((win
           (mpcompat:process-wait-with-timeout "reply-wait"
                                          *seconds-to-wait*
                                          (lambda ()
                                            (null (kvs:lookup-key (repliers-expected thisnode) soluid))))))
      (if win
          (maybe-log thisnode :DONE-WAITING-WIN msg)
          (maybe-log thisnode :DONE-WAITING-TIMEOUT msg))
      (let ((local-alive nil)
            (where-to-forward-reply srcuid))
        ; clean up reply tables
        (mpcompat:with-lock ((reply-table-lock thisnode))
          (setf local-alive (kvs:lookup-key (reply-data thisnode) soluid))
          (remhash soluid (repliers-expected thisnode))
          (remhash soluid (reply-data thisnode)))
        ; must create a new reply here; cannot reuse an old one because its content has changed
        (if (uid? where-to-forward-reply) ; should be a uid or T. Might be nil if there's a bug.
            (let ((reply (make-reply :solicitation-uid soluid
                                     :kind :count-alive
                                     :args (list local-alive))))
              (maybe-log thisnode :SEND-REPLY reply :to (briefname where-to-forward-reply "node") local-alive)
              (send-msg reply
                        where-to-forward-reply
                        (uid thisnode)))
            ; if no place left to reply to, just log the result.
            ;   This can mean that thisnode autonomously initiated the request, or
            ;   somebody running the sim told it to.
            (maybe-log thisnode :FINALREPLY msg local-alive))))))

(defmethod count-alive ((rep reply) thisnode srcuid)
  (let ((soluid (solicitation-uid rep)))
    ; First record the data in the reply appropriately
    (let ((alive-in-reply (first (args rep))))
      (mpcompat:with-lock ((reply-table-lock thisnode))
        (let ((local-alive (kvs:lookup-key (reply-data thisnode) soluid)))
          (kvs:relate-unique! (reply-data thisnode) soluid (append alive-in-reply local-alive)))))
    ; Now remove this srcuid from expected-replies list. (We know it's on
    ;  the list because this method would never have been called otherwise.)
    (let ((nodes-expected-to-reply (kvs:lookup-key (repliers-expected thisnode) soluid)))
      (kvs:relate-unique! (repliers-expected thisnode) soluid (delete srcuid nodes-expected-to-reply)))))

; For debugging count-alive
(defun missing? (alive-list)
  "Returns a list of node UIDs that are missing from a list returned by count-alive"
  (let ((dead-list nil))
    (maphash (lambda (key value)
               (declare (ignore value))
               (unless (member key alive-list)
                 (push key dead-list)))
             *nodes*)
    (sort dead-list #'<)))

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

;;; END OF GOSSIP METHODS

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
      (incf (hopcount msg)) ; no need to copy message here since we just created it from scratch
      (let ((destnode (lookup-node destuid)))
        (locally-receive-msg msg destnode srcuid)))))

; just for simulator
(defmethod deliver-msg (msg (node gossip-node) srcuid)
  (setf msg (copy-message msg)) ; must copy before incrementing hopcount because we can't
  ;  modify the original without affecting other threads.
  (incf (hopcount msg))
  (let ((kindsym (accept-msg? msg node srcuid))) ; check this here only in simulator
    (when kindsym ; ignore the message here and now if it's ignorable, without creating a new thread
      (when (> (count-node-processes) (+ 100 (hash-table-count *nodes*))) ; just a WAG for debugging combinatorial explosions in simulator
        (pprint (all-processes) t)
        (break "In deliver-msg"))
      ; Remember the srcuid that sent me this message, because that's where reply will be forwarded to
      (kvs:relate-unique! (message-cache node) (uid msg) (or srcuid t)) ; solicitations from outside might not have a srcid
      (mpcompat:process-run-function (format nil "Node ~D" (uid node)) nil
        (lambda () (locally-receive-msg msg node srcuid kindsym))))))

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
      (mpcompat:with-lock (*message-space-lock*)
        (setf nextmsg (deq *message-space*)))
      (if nextmsg
          ; Process messages as fast as possible.
          (apply 'dispatch-msg nextmsg)
          ; But if no messages to process, wait a bit.
          (sleep .05) ; this dramatically decreases CPU time taken up by Lisp
          ))))

(defun stop-gossip-sim ()
  (setf *stop-dispatcher* t))

(defun run-gossip-sim ()
  (stop-gossip-sim) ; stop old simulation, if any
  (sleep .2) ; give dispatcher-loop time to stop
  ; Create gossip network
  ; Clear message space
  ; Archive the current log and clear it
  (kill-node-processes)
  (vector-push-extend *log* *archived-logs*)
  (setf *log* (new-log))
  (setf *message-space* (make-queue))
  (setf *message-space-lock* (mpcompat:make-lock))
  (mapc 'clear-caches (listify-nodes))
  ; Start daemon that dispatches messages to nodes
  (mpcompat:process-run-function "Dispatcher Loop" nil (lambda () (dispatcher-loop)))
  )


; (make-graph 10)
; (save-graph-to-file "~/gossip/10nodes.lisp")
; (restore-graph-from-file "~/gossip/10nodes.lisp")
; (make-graph 5)
; (save-graph-to-file "~/gossip/5nodes.lisp")
; (restore-graph-from-file "~/gossip/5nodes.lisp")

; (make-graph 100)
; (visualize-nodes *nodes*)

(defun count-node-processes ()
  "Returns a count of node processes."
  (let ((count 0))
    (mapc  #'(lambda (process)
               (when (ignore-errors (string= "Node " (subseq (process-name process) 0 5)))
                 (incf count)))
           (all-processes))
    count))

(defun node-processes ()
  "Returns a list of node processes."
  (let ((node-processes nil))
    (mapc  #'(lambda (process)
               (when (ignore-errors (string= "Node " (subseq (process-name process) 0 5)))
                 (push process node-processes)))
           (all-processes))
    node-processes))

; Mostly for debugging. Node processes should automaticallykill themselves.
(defun kill-node-processes ()
  (let ((node-processes (node-processes)))
    (mapc 'process-kill node-processes)))

; (run-gossip-sim)
; (solicit (first (listify-nodes)) :count-alive)
; (solicit (first (listify-nodes)) :announce :foo)
; (solicit 340 :count-alive)
; (inspect *log*) --> Should see :FINALREPLY with all nodes (or something slightly less, depending on *hop-factor*, network delays, etc.) 

; (solicit (first (listify-nodes)) :relate-unique :foo :bar)
; (solicit (first (listify-nodes)) :lookup-key :foo)
; (solicit (first (listify-nodes)) :relate :foo :baz)
; (solicit (first (listify-nodes)) :lookup-key :foo)
;; should produce something like (:FINALREPLY "node209" "sol255" (((:BAZ :BAR) . 4))) as last *log* entry

#+IGNORE
(setf node (make-node
  :UID 253
  :ADDRESS 'NIL
  :NEIGHBORS '(248 250 251)
  :LOGFN 'GOSSIP::DEFAULT-LOGGING-FUNCTION
  :KVS (as-hash-table 'eql '((key1 . 1) (key2 . 2) (key3 . 3)))))

#+IGNORE
(save-node node *standard-output*)

#+IGNORE
(setf msg (make-solicitation :kind :count-alive))
#+IGNORE
(copy-message msg)