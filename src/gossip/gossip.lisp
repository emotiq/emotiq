;;; gossip.lisp

;;; More realistic gossip protocol.
;;; Minimal changes to use actor concurrency model. As far as the actors are concerned,
;;;  we're just sending messages of type :gossip, regardless of whether they're solicitations,
;;;  replies, or timeouts.
;;; Gossip messages are simply sent as payload of a lower-level actor message.
;;; No more process-run-function because actors take care of that.
;;; No need for locks to access node data structures because they're always accessed from an actor's single thread.

;;; New mechanism: Interim replies instead of carefully-timed timeouts.

;;;; TODO:
;;; Change gossip-lookup-key to use the new mechanism now used by list-alive.
;;; Reorganize code after all the recent chaos.

;;;; NOTES: "Upstream" means "back to the solicitor: the node that sent me a solicitation in the first place"

(in-package :gossip)

(defparameter *max-message-age* 30 "Messages older than this number of seconds will be ignored")
(defparameter *max-seconds-to-wait* 10 "Max seconds to wait for all replies to come in")
(defparameter *use-all-neighbors* nil "True to broadcast to all neighbors; nil to randomly pick just one")
(defparameter *active-ignores* t "True to reply to sender when we're ignoring a message from that sender.")

; Typical cases:
; Case 1: *use-all-neighbors* = true and *active-ignores* = nil. The total-coverage case.
; Case 2: *use-all-neighbors* = false and *active-ignores* = true. The more common gossip case.
; Case 3: *use-all-neighbors* = true and *active-ignores* = true. More messages than necessary.
; Case 4: *use-all-neighbors* = false and *active-ignores* = false. Lots of timeouts. Poor results.

(defun set-protocol-style (kind)
  "Set style of protocol.
   :gossip style to pick one neighbor at random to send solicitations to.
      There's no guarantee this will reach all nodes. But it's quicker and more realistic.
   :full   style to send solicitations to all neighbors. More likely to reach all nodes but replies may be slower.
   Neither style should result in timeouts of a node waiting forever for a response from a neighbor."
  (case kind
    (:gossip (setf *use-all-neighbors* nil
                   *active-ignores* t))  ; must be true when *use-all-neighbors* is nil. Otherwise there will be timeouts.
    (:full   (setf *use-all-neighbors* t
                   *active-ignores* nil)))
  kind)
             
; (set-protocol-style :full)
; (set-protocol-style :gossip)

#|
Discussion: :gossip style is more realistic for "loose" networks where nodes don't know much about their neighbors.
:full style is better after :gossip style has been used to discover the graph topology and agreements about collaboration
are in place between nodes.
|#

(defparameter *gossip-absorb-errors* t "True for normal use; nil for debugging")
(defvar *last-uid* 0 "Simple counter for making UIDs")
(defparameter *log-filter* t "t to log all messages; nil to log none")
(defparameter *delay-interim-replies* t "True to delay interim replies.
  Should be true, especially on larger networks. Reduces unnecessary interim-replies while still ensuring
  some partial information is propagating in case of node failures.")

(defun make-uid-mapper ()
  "Returns a table that maps UIDs to objects"
  (kvs:make-store ':hashtable :test 'equal))

#+LISPWORKS
(hcl:defglobal-variable *nodes* (make-uid-mapper) "Table for mapping node UIDs to nodes known by local machine")
#+CCL
(ccl:defglobal *nodes* (make-uid-mapper) "Table for mapping node UIDs to nodes known by local machine")

(defun log-exclude (&rest strings)
  "Prevent log messages whose logcmd contains any of the given strings. Case-insensitive."
 (lambda (logcmd)
   (let ((logcmdname (symbol-name logcmd)))
     (notany (lambda (x) (search x logcmdname :test #'char-equal)) strings))))

(defun log-include (&rest strings)
  "Allow log messages whose logcmd contains any of the given strings. Case-insensitive."
 (lambda (logcmd)
   (let ((logcmdname (symbol-name logcmd)))
     (some (lambda (x) (search x logcmdname :test #'char-equal)) strings))))

;; Ex: Don't log any messages that contain "WAIT" or "ACCEPT"
;; (setf *log-filter* (log-exclude "WAIT" "ACCEPT" "IGNORE"))

;; Ex: Don't log any messages that contain "COALESCE"
;; (setf *log-filter* (log-exclude "COALESCE"))

;; Ex: Include only :FINALREPLY messages
;; (setf *log-filter* (log-include "FINALREPLY"))

(defun new-uid ()
  (incf *last-uid*))

(defun uid? (thing)
  (integerp thing))

(defclass uid-mixin ()
  ((uid :initarg :uid :initform (new-uid) :reader uid
        :documentation "Unique ID. These are assumed to become larger over time and not be random: They
         are used in discriminating later messages from earlier ones.")))

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

(defclass gossip-message-mixin (uid-mixin)
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

(defmethod copy-message ((msg gossip-message-mixin))
  (let ((new-msg (make-instance (class-of msg)
                   :uid (uid msg)
                   :timestamp (timestamp msg)
                   :hopcount (hopcount msg)
                   :kind (kind msg)
                   :args (args msg))))
    new-msg))

(defclass solicitation (gossip-message-mixin)
  ())

(defun make-solicitation (&rest args)
  (apply 'make-instance 'solicitation args))


(defclass solicitation-uid-mixin ()
  ((solicitation-uid :initarg :solicitation-uid :initform nil :accessor solicitation-uid
                     :documentation "UID of solicitation message that elicited this reply.")))

; Note: If you change a message before forwarding it, you need to create a new
;   message with a new UID. (Replies can often be changed as they percolate backwards;
;   they need new UIDs so that a node that has seen one set of information won't
;   automatically ignore it.)
(defclass reply (gossip-message-mixin solicitation-uid-mixin)
  ()
  (:documentation "Reply message. Used to reply to solicitations that need a reply.
     It is common and expected to receive multiple replies matching a given solicitation-uid.
     Most replies are 'inverse broadcasts' in that they have many sources that funnel
     back to one ultimate receiver -- the originator of the solicitation. However,
     a few replies (e.g. to sound-off) are full broadcasts unto themselves."))

(defclass interim-reply (reply)
  ()
  (:documentation "Interim replies engender a forwarded interim reply immediately to coalesce
    all available data. A later interim reply (or a final reply) from node X for solicitation Y will override
    an earlier interim reply. 'Later' here means the reply itself has a UID that's larger than the 'older'
    one."))

(defun make-interim-reply (&rest args)
  (apply 'make-instance 'interim-reply args))

(defclass final-reply (reply)
  ()
  (:documentation "Final replies engender a forwarded final reply from node X iff all other replies node X
    is expecting have also been received and are final replies. Furthermore, all reply-expectation structures
    must be cleaned up in this event.
    Otherwise a final reply to node X just engenders another interim reply upstream from node X."))

(defun make-final-reply (&rest args)
  (apply 'make-instance 'final-reply args))

(defclass timeout (gossip-message-mixin solicitation-uid-mixin)
  ()
  (:documentation "Used only for special timeout messages. In this case, solicitation-uid field
    is used to indicate which solicitation should be timed out at the receiver of this message."))

(defun make-timeout (&rest args)
  (let ((timeout (apply 'make-instance 'timeout args)))
    (if (null (solicitation-uid timeout)) (break))
    timeout))

(defmethod copy-message :around ((msg reply))
  (let ((new-msg (call-next-method)))
    (setf (solicitation-uid new-msg) (solicitation-uid msg))
    new-msg))

(defmethod copy-message :around ((msg timeout))
  (let ((new-msg (call-next-method)))
    (setf (solicitation-uid new-msg) (solicitation-uid msg))
    new-msg))

(defclass gossip-mixin (uid-mixin)
  ((address :initarg :address :initform nil :accessor address
            :documentation "Network address (e.g. IP) of node.")))
   
(defclass gossip-actor (ac:actor)
  ((node :initarg :node :initform nil :accessor node
         :documentation "The gossip-node this actor on behalf of which this actor works")))

(defclass gossip-node (gossip-mixin)
  ((message-cache :initarg :message-cache :initform (make-uid-mapper) :accessor message-cache
                  :documentation "Cache of seen messages. Table mapping UID of message to UID of upstream sender.
                  Used to ensure identical messages are not acted on twice, and to determine where
                  replies to a message should be sent.")
   (repliers-expected :initarg :repliers-expected :initform (kvs:make-store ':hashtable :test 'equal)
                      :accessor repliers-expected
                      :documentation "2-level Hash-table mapping a solicitation id to another hashtable of srcuids
                     that I expect to reply to that solicitation. Values in second hashtable are interim replies
                     that have been received from that srcuid for the given solicitation id.
                     Only accessed from this node's actor thread. No locking needed.")

   (reply-cache :initarg :reply-cache :initform (kvs:make-store ':hashtable :test 'equal)
               :accessor reply-cache
               :documentation "Hash-table mapping a solicitation id to some data applicable to THIS node
                  for that solicitation. Only accessed from this node's actor
                  thread. No locking needed. Only used for duration of a solicitation/reply cycle.")
   (local-kvs :initarg :local-kvs :initform (kvs:make-store ':hashtable :test 'equal) :accessor local-kvs
        :documentation "Local persistent key/value store for this node. Put long-lived global state data here.")
   (neighbors :initarg :neighbors :initform nil :accessor neighbors
              :documentation "List of UIDs of direct neighbors of this node")
   (actor :initarg :actor :initform nil :accessor actor
          :documentation "Actor for this node")
   (timers :initarg :timers :initform nil :accessor timers
            :documentation "Table mapping solicitation uids to a timer dealing with replies to that uid.")
   (timeout-handlers :initarg :timeout-handlers :initform nil :accessor timeout-handlers
                     :documentation "Table of functions of 1 arg: timed-out-p that should be
          called to clean up after waiting operations are done. Keyed on solicitation id.
          Timed-out-p is true if a timeout happened. If nil, it means operations completed without
          timing out.")
   (logfn :initarg :logfn :initform 'default-logging-function :accessor logfn
          :documentation "If non-nil, assumed to be a function called with every
              message seen to log it.")))

(defmethod clear-caches ((node gossip-node))
  "Caches should be cleared in the normal course of events, but this can be used to make sure."
  (kvs:clear-store! (message-cache node))
  (kvs:clear-store! (repliers-expected node))
  (kvs:clear-store! (reply-cache node))
  ; don't clear the local-kvs. That should be persistent.
  )

; We'll use these for real (not simulated on one machine) protocol
(defclass remote-gossip-node (gossip-mixin)
  ()
  (:documentation "A local [to this process] standin for a gossip-node located elsewhere.
              All we know about it is its UID and address, which is enough to transmit a message to it."))

(defun gossip-dispatcher (gossip-node &rest actor-msg)
  "Extracts gossip-msg from actor-msg and calls deliver-gossip-msg on it"
  (let ((gossip-cmd (first actor-msg)))
    (case gossip-cmd
      (:gossip
       (unless gossip-node (error "No node attached to this actor!"))
       (destructuring-bind (srcuid gossip-msg) (cdr actor-msg)
         (deliver-gossip-msg gossip-msg gossip-node srcuid)))
      (:relay
       ; we expect gossip-node to be nil in this case, but it's actually
       ;   irrelevant. We could just as well use an actor attached to a node
       ;   for relaying if we wanted to.
       (destructuring-bind (srcuid destuid gossip-msg) (cdr actor-msg)
         (send-msg gossip-msg destuid srcuid))))))

(defun make-gossip-actor (gossip-node)
  (make-instance 'gossip-actor
    :node gossip-node
    :fn 
    (lambda (&rest msg)
      (apply 'gossip-dispatcher gossip-node msg))))

(defparameter *relay-actor* nil "An actor whose sole purpose is to bounce messages to other nodes.")

(defmethod relay-msg ((msg gossip-message-mixin) destuid srcuid)
  "Cause a message to be relayed to another node -- or back to myself.
  Usually we use this in echo mode (where destuid = srcuid),
  so an actor can send a message to itself but forcing itself to first yield and
  handle other messages before handling the one herein."
  (unless *relay-actor*
    (setf *relay-actor* (make-gossip-actor nil)))
  (ac:send *relay-actor*
           :relay ; actor-verb
           srcuid  ; source node
           destuid ; must send this as a parameter for relays, so relayer will know who to send it to
           msg))

(defun make-node (&rest args)
  "Makes a new node"
  (let* ((node (apply 'make-instance 'gossip-node args))
         (actor (make-gossip-actor node)))
    (setf (actor node) actor)
    (kvs:relate-unique! *nodes* (uid node) node)
    node))

;;;; Graph making routines
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

;;;; End of Graph making routines

(defun make-graph (numnodes &optional (fraction 0.5))
  "Build a graph with numnodes nodes. Strategy here is to first connect all the nodes in a single
   non-cyclic linear path, then add f random edges, where f is multiplied by the number of nodes."
  (clrhash *nodes*)
  (make-nodes numnodes)
  (let ((nodelist (listify-nodes)))
    ; following guarantees a single connected graph
    (linear-path nodelist)
    ; following --probably-- makes the graph an expander but we'll not try to guarantee that for now
    (add-random-connections nodelist (round (* fraction (length nodelist)))))
  numnodes)

;;;; Graph saving/restoring routines

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
      (write-slot :local-kvs (local-kvs node))
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

;;;; End of Graph saving/restoring routines

(defun solicit (node kind &rest args)
  "Send a solicitation to the network starting with given node. This is the primary interface to 
  the network to start an action from the outside. Nodes shouldn't use this function to initiate an
  action because they should set the srcuid parameter to be their own rather than nil."
  (unless node
    (error "No destination node supplied. You might need to run make-graph or restore-graph-from-file first."))
  (let ((uid (if (typep node 'gossip-node) ; yeah this is kludgy.
                 (uid node)
                 node)))
    (let* ((solicitation (make-solicitation
                          :kind kind
                          :args args))
           (soluid (uid solicitation)))
      (send-msg solicitation
                uid      ; destination
                nil)     ; srcuid
      soluid)))

(defun solicit-wait (node kind &rest args)
  "Like solicit but waits for a reply.
  Don't use this on messages that don't expect a reply, because it'll wait forever."
  (unless node
    (error "No destination node supplied. You might need to run make-graph or restore-graph-from-file first."))
  (let ((uid (if (typep node 'gossip-node) ; yeah this is kludgy.
                 (uid node)
                 node))
        (response nil))
    (flet ((final-continuation (message)
             (setf response message)))
      (let* ((solicitation (make-solicitation
                            :kind kind
                            :args args))
             (soluid (uid solicitation)))
        (send-msg solicitation
                  uid      ; destination
                  #'final-continuation)     ; srcuid
        (let ((win (mpcompat:process-wait-with-timeout "Waiting for reply" *max-seconds-to-wait*
                                                       (lambda () response))))
          (values 
           (if win
               (first (args response))
               :TIMEOUT)
           soluid))))))

(defun solicit-progress (node kind &rest args)
  "Like solicit-wait but prints periodic progress log messages (if any)
  associated with this node to listener.
  Don't use this on messages that don't expect a reply, because it'll wait forever."
  (unless node
    (error "No destination node supplied. You might need to run make-graph or restore-graph-from-file first."))
  (let* ((uid (if (typep node 'gossip-node) ; yeah this is kludgy.
                  (uid node)
                  node))
         (node (if (typep node 'gossip-node)
                   node
                   (lookup-node node)))
         (response nil)
         (old-logger (logfn node)))
    (flet ((final-continuation (message)
             (setf response message)))
      (let* ((solicitation (make-solicitation
                            :kind kind
                            :args args))
             (soluid (uid solicitation)))
        (unwind-protect
            (progn
              (setf (logfn node) #'interactive-logging-function)
              (send-msg solicitation
                        uid      ; destination
                        #'final-continuation)     ; srcuid
              (let ((win (mpcompat:process-wait-with-timeout "Waiting for reply" *max-seconds-to-wait*
                                                             (lambda () response))))
                (values 
                 (if win
                     (first (args response))
                     :TIMEOUT)
                 soluid)))
          (setf (logfn node) old-logger))))))

(defun interactive-logging-function (logcmd nodename msgname &rest args)
  "Use this logging function for interactive debugging. You'll probably only want to use this
  in the node you called #'solicit on."
  (let* ((logmsg (apply 'default-logging-function logcmd nodename msgname args))
         (logstring (format nil "~S~%" logmsg)))
    #+CCL
    (if (find-package :hi)
        (funcall (intern "WRITE-TO-TOP-LISTENER" :hi) logstring)
        (write-string logstring *standard-output*))
    #-CCL
    (write-string logstring *standard-output*)))


(defmethod briefname ((node gossip-node) &optional (prefix "node"))
 (format nil "~A~D" prefix (uid node)))

(defmethod briefname ((msg solicitation) &optional (prefix "sol"))
  (format nil "~A~D" prefix (uid msg)))

(defmethod briefname ((msg reply) &optional (prefix "rep"))
  (format nil "~A~D" prefix (uid msg)))

(defmethod briefname ((msg timeout) &optional (prefix "timeout"))
  (format nil "~A~D" prefix (uid msg)))

(defmethod briefname ((id integer) &optional (prefix ""))
  (format nil "~A~D" prefix id))

(defmethod briefname ((id symbol) &optional (prefix ""))
  (declare (ignore prefix))
  (format nil "~A" id))

(defmethod briefname ((id string) &optional (prefix ""))
  (format nil "~A~A" prefix id))

#+IGNORE
(defmethod briefname ((id null) &optional (prefix ""))
  (declare (ignore prefix))
  "nil")

(defmethod briefname ((id t) &optional (prefix ""))
  (declare (ignore prefix))
  id)

; Logcmd: Keyword that describes what a node has done with a given message UID
; Examples: :IGNORE, :ACCEPT, :FORWARD, etc.
(defmethod maybe-log ((node gossip-node) logcmd msg &rest args)
  (when *log-filter*
    (when (or (eq t *log-filter*)
              (funcall *log-filter* logcmd))
      (when (logfn node)
        (apply (logfn node)
               logcmd
               (briefname node)
               (briefname msg)
               args)))))

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
  in the mode you called #'solicit on.
  Returns the form that default-logging-function returned."
  (let* ((logmsg (apply 'default-logging-function logcmd nodename msgname args))
         (logstring (format nil "~S~%" logmsg)))
    #+CCL
    (if (find-package :hi)
        (funcall (intern "WRITE-TO-TOP-LISTENER" :hi) logstring)
        (write-string logstring *standard-output*))
    #-CCL
    (write-string logstring *standard-output*)
    logmsg))
  
(defmethod send-msg ((msg gossip-message-mixin) destuid srcuid)
  (let* ((destnode (lookup-node destuid))
         (destactor (when destnode (actor destnode))))
    (ac:send destactor
           :gossip ; actor-verb
           srcuid  ; first arg of actor-msg
           msg)))  ; second arg of actor-msg

;  TODO: Might want to also check hopcount and reject message where hopcount is too large.
;        Might want to not accept maybe-sir messages at all if their soluid is expired.
(defmethod accept-msg? ((msg gossip-message-mixin) (thisnode gossip-node) srcuid)
  "Returns kindsym if this message should be accepted by this node, nil and a failure-reason otherwise.
  Kindsym is the name of the gossip method that should be called to handle this message.
  Doesn't change anything in message or node."
  (declare (ignore srcuid)) ; not using this for acceptance criteria in the general method
  (let ((soluid (uid msg)))
    (cond ((> (get-universal-time) (+ *max-message-age* (timestamp msg))) ; ignore too-old messages
           (values nil :too-old))
          (t
           (let ((already-seen? (kvs:lookup-key (message-cache thisnode) soluid)))
             (cond (already-seen? ; Ignore if already seen
                    (values nil :already-seen))
                   (t ; it's a new message
                    (let* ((kind (kind msg))
                           (kindsym nil))
                      (cond (kind
                             (setf kindsym (intern (symbol-name kind) :gossip))
                             (if (and kindsym (fboundp kindsym))
                                 kindsym ;; SUCCESS! We accept the message.
                                 (values nil (list :unknown-kind kindsym))))
                            (t
                             (values nil :no-kind)))))))))))

(defmethod accept-msg? ((msg reply) (thisnode gossip-node) srcuid)
  (if (eq :active-ignore (kind msg))
      (values nil :active-ignore)
      (multiple-value-bind (kindsym failure-reason) (call-next-method) ; the one on gossip-message-mixin
        ; Also ensure this reply is actually expected
        (cond (kindsym
               (let ((interim-table (kvs:lookup-key (repliers-expected thisnode) (solicitation-uid msg))))
                 (cond (interim-table
                        (multiple-value-bind (val present-p) (kvs:lookup-key interim-table srcuid)
                          (declare (ignore val))
                          (if present-p
                              kindsym
                              (values nil :unexpected-1))))
                       (t (values nil :unexpected-2)))))
              (t (values nil failure-reason))))))

(defmethod accept-msg? ((msg timeout) (thisnode gossip-node) srcuid)
  (declare (ignore srcuid))
  'timeout ; timeouts are always accepted
  )

; TODO: Remove old entries in message-cache, eventually.
(defmethod memoize-message ((node gossip-node) (msg gossip-message-mixin) srcuid)
  "Record the fact that this node has seen this particular message.
   In cases of solicitation messages, we also care about the upstream sender, so
   we just save that as the value in the key/value pair."
  (kvs:relate-unique! (message-cache node)
                      (uid msg)
                      (or srcuid t) ; because solicitations from outside might not have a srcid
                      ))

(defmethod get-upstream-source ((node gossip-node) soluid)
  "Retrieves the upstream source uid for a given soluid on this node"
  (kvs:lookup-key (message-cache node) soluid))

(defmethod get-downstream ((node gossip-node) srcuid)
  (let ((all-neighbors (remove srcuid (neighbors node))))
  (if (or *use-all-neighbors*
          (null all-neighbors))
      all-neighbors
      (list (nth (random (length all-neighbors)) all-neighbors)))))

(defun send-active-ignore (to from kind soluid failure-reason)
  "Actively send a reply message to srcuid telling it we're ignoring it."
  (let ((msg (make-interim-reply
              :solicitation-uid soluid
              :kind :active-ignore
              :args (list kind failure-reason))))
    (send-msg msg
              to
              from)))

(defmethod locally-receive-msg ((msg gossip-message-mixin) (node gossip-node) srcuid)
  "The main dispatch function for gossip messages. Runs entirely within an actor.
  First checks to see whether this message should be accepted by the node at all, and if so,
  it calls the function named in the kind field of the message to handle it."
  (let ((soluid (uid msg)))
    (multiple-value-bind (kindsym failure-reason) (accept-msg? msg node srcuid)
      (cond (kindsym ; message accepted
             (memoize-message node msg srcuid)
             (let ((logsym (typecase msg
                             (solicitation :accepted)
                             (interim-reply :interim-reply-accepted)
                             (final-reply :final-reply-accepted)
                             (t nil) ; don't log timeouts here. Too much noise.
                             )))
               (when logsym
                 (maybe-log node logsym msg (kind msg) :from (briefname srcuid "node") (args msg)))
               (if *gossip-absorb-errors*
                   (handler-case (funcall kindsym msg node srcuid)
                     (error (c) (maybe-log node :ERROR msg c)))
                   (funcall kindsym msg node srcuid))))
            (t ; not accepted
             (maybe-log node :ignore msg :from (briefname srcuid "node") failure-reason)
             (case failure-reason
               (:active-ignore ; RECEIVE an active-ignore. Whomever sent it is telling us they're ignoring us.
                ; Which means we need to ensure we're not waiting on them to reply.
                (if (typep msg 'interim-reply) ; should never be any other type
                    (destructuring-bind (kind failure-reason) (args msg)
                      (declare (ignore failure-reason)) ; should always be :already-seen, but we're not checking for now
                      (let ((was-present? (cancel-replier node kind (solicitation-uid msg) srcuid)))
                        (when was-present?
                          ; Don't log a :STOP-WAITING message if we were never waiting for a reply from srcuid in the first place
                          (maybe-log node :STOP-WAITING msg srcuid))))
                    ; weird. Shouldn't ever happen.
                    (maybe-log node :ERROR msg :from srcuid :ACTIVE-IGNORE-WRONG-TYPE)))
               (:already-seen ; potentially SEND an active ignore
                (when (typep msg 'solicitation) ; following is extremely important. See note A below.
                  ; If we're ignoring a message from node X, make sure that we are not in fact
                  ;   waiting on X either. This is essential in the case where
                  ;   *use-all-neighbors* = true and
                  ;   *active-ignores* = false
                  ;   but it doesn't hurt anything in other cases.
                  (let ((was-present? (cancel-replier node (kind msg) soluid srcuid)))
                    (when was-present?
                      ; Don't log a :STOP-WAITING message if we were never waiting for a reply from srcuid in the first place
                      (maybe-log node :STOP-WAITING msg srcuid))
                    (when *active-ignores* ; now actively tell X we're ignoring it
                      (send-active-ignore srcuid (uid node) (kind msg) soluid failure-reason)))))
               (t nil)))))))

; NOTE A:
; If node X is ignoring a solicitation* from node Y because it already
; saw that solicitation, then it must also not expect a reply from node Y
; for that same solicitation.
; Here's why [In this scenario, imagine #'locally-receive-msg is acting on behalf of node X]:
; If node X ignores a solicition from node Y -- because it's already seen that solicitation --
;   then X knows the following:
;   1. Node Y did not receive the solicition from X. It must have received it from somewhere else,
;      because nodes *never* forward messages to their upstream.
;   2. Therefore, if X forwards (or forwarded) the solicitation to Y, Y
;      is definitely going to ignore it. Because Y has already seen it.
;   3. Therefore (to recap) if X just ignored a solicitation from Y, then X
;      knows Y is going to ignore that same solicitation from X.
;   4. THEREFORE: X must not expect Y to respond, and that's why
;      we call cancel-replier here. If we don't do this, Y will ignore
;      and X will eventually time out, which it doesn't need to do.
;   5. FURTHERMORE: Y knows X knows this. So it knows X will stop waiting for it.
;
; * we take no special action for non-solicitation messages because they can't
;   ever be replied to anyway.

(defmethod locally-receive-msg ((msg t) (thisnode remote-gossip-node) srcuid)
  (declare (ignore srcuid))
  (error "Bug: Cannot locally-receive to a remote node!"))

(defun forward (msg srcuid destuids)
  "Sends msg from srcuid to multiple destuids"
  (unless (uid? srcuid) (setf srcuid (uid srcuid)))
  (mapc (lambda (destuid)
          (send-msg msg destuid srcuid))
        destuids))

(defun send-gossip-timeout-message (actor soluid)
  "Send a gossip-timeout message to an actor. (We're not using the actors' native timeout mechanisms at this time.)"
  (ac:send actor
           :gossip
           'ac::*master-timer* ; source of timeout messages is always *master-timer* thread
           (make-timeout :solicitation-uid soluid
                         :kind :timeout)))

(defun schedule-gossip-timeout (delta actor soluid)
  "Call this to schedule a timeout message to be sent to an actor after delta seconds from now.
   Keep the returned value in case you need to call ac::unschedule-timer before it officially times out."
  (when delta
    (let ((timer (ac::make-timer
                  'send-gossip-timeout-message actor soluid)))
      (ac::schedule-timer-relative timer (ceiling delta)) ; delta MUST be an integer number of seconds here
      timer)))

(defmethod make-timeout-handler ((node gossip-node) (msg solicitation) #-LISPWORKS (kind keyword) #+LISPWORKS (kind symbol))
  (let ((soluid (uid msg)))
    (lambda (timed-out-p)
      "Cleanup operations if timeout happens, or all expected replies come in."
      (let ((timer (kvs:lookup-key (timers node) soluid)))
        (cond (timed-out-p
               ; since timeout happened, actor infrastructure will take care of unscheduling the timeout
               (maybe-log node :DONE-WAITING-TIMEOUT msg))
              (t ; done, but didn't time out. Everything's good. So unschedule the timeout message.
               (cond (timer
                      (ac::unschedule-timer timer) ; cancel a timer prematurely, if any.
                      ; if no timer, then this is a leaf node
                      (maybe-log node :DONE-WAITING-WIN msg))
                     (t ; note: Following log message doesn't necessarily mean anything is wrong.
                      ; If node is singly-connected to the graph, it's to be expected
                      (maybe-log node :NO-TIMER-FOUND msg)))))
        (cleanup&finalreply node kind soluid)))))

(defmethod prepare-repliers ((thisnode gossip-node) soluid downstream)
  "Prepare reply tables for given node, solicitation uid, and set of downstream repliers."
  (let ((interim-table (kvs:make-store ':hashtable :test 'equal)))
    (dolist (replier-uid downstream)
      (kvs:relate-unique! interim-table replier-uid nil))
    (kvs:relate-unique! (repliers-expected thisnode) soluid interim-table)
    interim-table))

;;;  GOSSIP METHODS. These handle specific gossip protocol solicitations and replies.

;; NO-REPLY-NECESSARY methods. One method per message.

;;;; Timeout. Generic message handler for all methods that scheduled a timeout.
;;;; These never expect a reply but they can happen for methods that did expect one.
(defmethod timeout ((msg timeout) thisnode srcuid)
  "Timeouts are a special kind of message in the gossip protocol,
  and they're typically sent by a special timer thread."
  (cond ((eq srcuid 'ac::*master-timer*)
         ;;(maybe-log thisnode :timing-out msg :from (briefname srcuid "node") (solicitation-uid msg))
         (let* ((soluid (solicitation-uid msg))
                (timeout-handler (kvs:lookup-key (timeout-handlers thisnode) soluid)))
           (when timeout-handler
             (funcall timeout-handler t))))
        (t ; log an error and do nothing
         (maybe-log thisnode :ERROR msg :from srcuid :INVALID-TIMEOUT-SOURCE))))

(defmethod more-replies-expected? ((node gossip-node) soluid)
  (let ((interim-table (kvs:lookup-key (repliers-expected node) soluid)))
    (when interim-table
      (not (zerop (hash-table-count interim-table))))))

(defmethod maybe-sir ((msg solicitation) thisnode srcuid)
  "Maybe-send-interim-reply. This is strictly a message handler; it's not a function
  a node actor should call. The sender of this message will usually be thisnode itself, via
  the *relay-actor*."
  (declare (ignore srcuid))
  ; srcuid will usually (always) be that of thisnode, but we're not checking for that
  ;   because it doesn't matter if it's not true. For now.
  (let* ((soluid (first (args msg)))
         (reply-kind (second (args msg)))
         (where-to-send-reply (get-upstream-source thisnode soluid)))
    (if (more-replies-expected? thisnode soluid)
      (send-interim-reply thisnode reply-kind soluid where-to-send-reply)
      ; else we'd have already sent a final reply and no further action is needed.
      )))

(defmethod announce ((msg solicitation) thisnode srcuid)
  "Announce a message to the collective. First arg of Msg is the announcement,
   which can be any Lisp object. Recipient nodes are not expected to reply.
   This is probably only useful for debugging gossip protocols, since the only
   record of the announcement will be in the log."
  (let ((content (first (args msg))))
    (declare (ignore content))
    ; thisnode becomes new source for forwarding purposes
    (forward msg thisnode (get-downstream thisnode srcuid))))

(defmethod gossip-relate ((msg solicitation) thisnode srcuid)
  "Establishes a global non-unique key/value pair. If key currently has a value or set of values,
   new value will be added to the set; it won't replace them.
  Sets value on this node and then forwards 
  solicitation to other nodes, if any.
  No reply expected."
  (destructuring-bind (key value &rest other) (args msg)
    (declare (ignore other))
    (setf (local-kvs thisnode) (kvs:relate (local-kvs thisnode) key value))
    ; thisnode becomes new source for forwarding purposes
    (forward msg thisnode (get-downstream thisnode srcuid))))

(defmethod gossip-relate-unique ((msg solicitation) thisnode srcuid)
  "Establishes a global unique key/value pair. [Unique means there will be only one value for this key.]
  Sets value on this node and then forwards 
  solicitation to other nodes, if any. This is a destructive operation --
  any node that currently has a value for the given key will have that value replaced.
  No reply expected."
  (destructuring-bind (key value &rest other) (args msg)
    (declare (ignore other))
    (setf (local-kvs thisnode) (kvs:relate-unique (local-kvs thisnode) key value))
    ; thisnode becomes new source for forwarding purposes
    (forward msg thisnode (get-downstream thisnode srcuid))))

(defmethod gossip-remove-key ((msg solicitation) thisnode srcuid)
  "Remove a global key/value pair. Removes key/value pair on this node and then forwards 
   solicitation to other nodes, if any. This is a destructive operation --
   any node that currently has the given key will have that key/value removed.
   There's no harm in calling this more than once with the same key; if key
   wasn't present in the first place, this is a no-op.
   No reply expected."
  (let ((key (first (args msg))))
    (kvs:remove-key! (local-kvs thisnode) key)
    ; thisnode becomes new source for forwarding purposes
    (forward msg thisnode (get-downstream thisnode srcuid))))

(defmethod gossip-tally ((msg solicitation) thisnode srcuid)
  "Increment the value of a given key by an increment amount.
   If no value for that key exists currently, set it to 1.
   No reply expected."
  (let ((key (first (args msg)))
        (increment (second (args msg))))
    (setf (local-kvs thisnode) (kvs:tally (local-kvs thisnode) key increment))
    ; thisnode becomes new source for forwarding purposes
    (forward msg thisnode (get-downstream thisnode srcuid))))

;;; TODO: add this to key-value-store
(defmethod tally-nondestructive ((store list) key amount &key (test #'equal))
  "Purely functional version of kvs:tally. Only works on alists.
  Increments the key . value pair in alist indicated by key, by the indicated amount.
  If such a pair doesn't exist, create it."
  (let* ((found nil)
         (result (mapcar (lambda (pair)
                           (if (funcall test key (car pair))
                               (progn
                                 (setf found t)
                                 (cons key (+ (cdr pair) amount)))
                               pair))
                         store)))
    (if found
        result
        (acons key amount result))))

; just an auxiliary function. Not a gossip method
(defun multiple-tally (alist1 alist2)
  "Given two alists like ((key1 . n1) (key2 . n2) ...)
  call kvs:tally for all pairs in alist."
  (let ((output nil))
    (dolist (pair alist2)
      (setf output (tally-nondestructive alist1 (car pair) (cdr pair))))
    output))

;; REPLY-NECESSARY methods. Three methods per message

;;;; List-Alive
(defmethod list-alive ((msg solicitation) (thisnode gossip-node) srcuid)
  "Get a list of UIDs of live nodes downstream of thisnode, plus that of thisnode itself."
  (let* ((soluid (uid msg))
         (downstream (get-downstream thisnode srcuid)) ; don't forward to the source of this solicitation
         (timer nil)
         (cleanup (make-timeout-handler thisnode msg :list-alive)))
    (kvs:relate-unique! (reply-cache thisnode) soluid (list (uid thisnode))) ; thisnode itself is 1 live node
    (cond (downstream
           (prepare-repliers thisnode soluid downstream)
           (forward msg thisnode downstream)
           ; wait a finite time for all replies
           (setf timer (schedule-gossip-timeout (ceiling *max-seconds-to-wait*) (actor thisnode) soluid))
           (kvs:relate-unique! (timers thisnode) soluid timer)
           (kvs:relate-unique! (timeout-handlers thisnode) soluid cleanup) ; bind timeout handler
           (maybe-log thisnode :WAITING msg (ceiling *max-seconds-to-wait*) downstream))
          (t ; this is a leaf node. Just reply upstream.
           (funcall cleanup nil)))))

(defmethod list-alive ((rep interim-reply) (thisnode gossip-node) srcuid)
  "Handler for interim replies of type :list-alive. These will come from children of a given node.
  Incoming interim-replies never change the local reply-cache. Rather, they
  coalesce the local reply-cache with all latest interim-replies and forward a new interim reply upstream."
  (let* ((soluid (solicitation-uid rep)))
    ; First record the reply including its data appropriately
    (when (record-interim-reply rep thisnode soluid srcuid) ; true if this reply is later than previous
      ; coalesce all known data and send it upstream as another interim reply.
      ; (if this reply is not later, drop it on the floor)
      (if *delay-interim-replies*
          (send-delayed-interim-reply thisnode :list-alive soluid)
          (let ((upstream-source (get-upstream-source thisnode (solicitation-uid rep))))
            (send-interim-reply thisnode :list-alive soluid upstream-source))))))

(defmethod list-alive ((rep final-reply) (thisnode gossip-node) srcuid)
  "Handler for final replies of type :list-alive. These will come from children of a given node.
  Incoming final-replies ALWAYS change the local reply-cache, to coalesce it with their data.
  Furthermore, they remove srcuid from downstream for this soluid.
  Furthermore, they always cause either an interim reply or a final reply to be sent upstream,
  depending on whether no further replies are expected or not."
  (let* ((soluid (solicitation-uid rep))
         (local-data (kvs:lookup-key (reply-cache thisnode) soluid))
         (coalescer (coalescer :LIST-ALIVE)))
    ; Any time we get a final reply, we destructively coalesce its data into local reply-cache
    (kvs:relate-unique! (reply-cache thisnode)
                        soluid
                        (funcall coalescer local-data (first (args rep))))
    (cancel-replier thisnode :list-alive soluid srcuid)))

;;;; Count-Alive
(defmethod count-alive ((msg solicitation) (thisnode gossip-node) srcuid)
  "Get a list of UIDs of live nodes downstream of thisnode, plus that of thisnode itself."
  (let* ((soluid (uid msg))
         (downstream (get-downstream thisnode srcuid)) ; don't forward to the source of this solicitation
         (timer nil)
         (cleanup (make-timeout-handler thisnode msg :count-alive)))
    (kvs:relate-unique! (reply-cache thisnode) soluid 1) ; thisnode itself is 1 live node
    (cond (downstream
           (prepare-repliers thisnode soluid downstream)
           (forward msg thisnode downstream)
           ; wait a finite time for all replies
           (setf timer (schedule-gossip-timeout (ceiling *max-seconds-to-wait*) (actor thisnode) soluid))
           (kvs:relate-unique! (timers thisnode) soluid timer)
           (kvs:relate-unique! (timeout-handlers thisnode) soluid cleanup) ; bind timeout handler
           (maybe-log thisnode :WAITING msg (ceiling *max-seconds-to-wait*) downstream))
          (t ; this is a leaf node. Just reply upstream.
           (funcall cleanup nil)))))

(defmethod count-alive ((rep interim-reply) (thisnode gossip-node) srcuid)
  "Handler for interim replies of type :count-alive. These will come from children of a given node.
  Incoming interim-replies never change the local reply-cache. Rather, they
  coalesce the local reply-cache with all latest interim-replies and forward a new interim reply upstream."
  (let* ((soluid (solicitation-uid rep)))
    ; First record the reply including its data appropriately
    (when (record-interim-reply rep thisnode soluid srcuid) ; true if this reply is later than previous
      ; coalesce all known data and send it upstream as another interim reply.
      ; (if this reply is not later, drop it on the floor)
      (if *delay-interim-replies*
          (send-delayed-interim-reply thisnode :count-alive soluid)
          (let ((upstream-source (get-upstream-source thisnode (solicitation-uid rep))))
            (send-interim-reply thisnode :count-alive soluid upstream-source))))))

(defmethod count-alive ((rep final-reply) (thisnode gossip-node) srcuid)
  "Handler for final replies of type :count-alive. These will come from children of a given node.
  Incoming final-replies ALWAYS change the local reply-cache, to coalesce it with their data.
  Furthermore, they remove srcuid from downstream for this soluid.
  Furthermore, they always cause either an interim reply or a final reply to be sent upstream,
  depending on whether no further replies are expected or not."
  (let* ((soluid (solicitation-uid rep))
         (local-data (kvs:lookup-key (reply-cache thisnode) soluid))
         (coalescer (coalescer :count-ALIVE)))
    ; Any time we get a final reply, we destructively coalesce its data into local reply-cache
    (kvs:relate-unique! (reply-cache thisnode)
                        soluid
                        (funcall coalescer local-data (first (args rep))))
    (cancel-replier thisnode :count-alive soluid srcuid)))

;;;; Gossip-lookup-key
(defmethod gossip-lookup-key ((msg solicitation) (thisnode gossip-node) srcuid)
  "Inquire as to the global value of a key. If this node has no further
  neighbors, just return its value. Otherwise collect responses from subnodes.
  Reply is of course expected.
  Reply here is somewhat complicated: Reply will be an alist of ((value1 . n1) (value2 .n2) ...) where
  value1 is value reported by n1 nodes downstream of thisnode,
  value2 is value reported by n2 nodes downstream of thisnode, etc."
  (let* ((soluid (uid msg))
         (downstream (get-downstream thisnode srcuid)) ; don't forward to the source of this solicitation
         (timer nil)
         (cleanup (make-timeout-handler thisnode msg :gossip-lookup-key))
         (key (first (args msg)))
         (myvalue (kvs:lookup-key (local-kvs thisnode) key)))
    (kvs:relate-unique! (reply-cache thisnode) soluid (list (cons myvalue 1)))
   (cond (downstream
           (prepare-repliers thisnode soluid downstream)
           (forward msg thisnode downstream)
           ; wait a finite time for all replies
           (setf timer (schedule-gossip-timeout (ceiling *max-seconds-to-wait*) (actor thisnode) soluid))
           (kvs:relate-unique! (timers thisnode) soluid timer)
           (kvs:relate-unique! (timeout-handlers thisnode) soluid cleanup) ; bind timeout handler
           (maybe-log thisnode :WAITING msg (ceiling *max-seconds-to-wait*) downstream))
          (t ; this is a leaf node. Just reply upstream.
           (funcall cleanup nil)))))

(defmethod gossip-lookup-key ((rep interim-reply) (thisnode gossip-node) srcuid)
  "Handler for replies of type :gossip-lookup-key. These will come from children of a given node.
  Incoming interim-replies never change the local reply-cache. Rather, they
  coalesce the local reply-cache with all latest interim-replies and forward a new interim reply upstream."
  (let ((soluid (solicitation-uid rep)))
    ; First record the data in the reply appropriately
    (when (record-interim-reply rep thisnode soluid srcuid) ; true if this reply is later than previous
      ; coalesce all known data and send it upstream as another interim reply.
      ; (if this reply is not later, drop it on the floor)
      (if *delay-interim-replies*
          (send-delayed-interim-reply thisnode :gossip-lookup-key soluid)
          (let ((upstream-source (get-upstream-source thisnode (solicitation-uid rep))))
            (send-interim-reply thisnode :gossip-lookup-key soluid upstream-source))))))

(defmethod gossip-lookup-key ((rep final-reply) (thisnode gossip-node) srcuid)
  "Handler for final replies of type :gossip-lookup-key. These will come from children of a given node.
  Incoming final-replies ALWAYS change the local reply-cache, to coalesce it with their data.
  Furthermore, they remove srcuid from downstream for this soluid.
  Furthermore, they always cause either an interim reply or a final reply to be sent upstream,
  depending on whether no further replies are expected or not."
  (let* ((soluid (solicitation-uid rep))
         (local-data (kvs:lookup-key (reply-cache thisnode) soluid))
         (coalescer (coalescer :GOSSIP-LOOKUP-KEY)))
    ; Any time we get a final reply, we destructively coalesce its data into local reply-cache
    (kvs:relate-unique! (reply-cache thisnode)
                        soluid
                        (funcall coalescer local-data (first (args rep))))
    (cancel-replier thisnode :gossip-lookup-key soluid srcuid)))

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

(defmethod find-address-for-node ((msg solicitation) thisnode srcuid)
  "Find address for a node with a given uid. Equivalent to DNS lookup."
  ;(forward msg thisnode (get-downstream thisnode srcuid))
  ; wait a finite time for all replies
  )

(defmethod find-node-with-address ((msg solicitation) thisnode srcuid)
  "Find address for a node with a given uid. Equivalent to reverse DNS lookup."
  ;(forward msg thisnode (get-downstream thisnode srcuid))
  ; wait a finite time for all replies
  )

;;; END OF GOSSIP METHODS

;;; REPLY SUPPORT ROUTINES

(defun cleanup&finalreply (thisnode reply-kind soluid)
  "Generic cleanup function needed for any message after all final replies have come in or
   timeout has happened.
   Clean up reply tables and reply upstream with final reply with coalesced data."
  (let ((coalesced-data ;(kvs:lookup-key (reply-cache thisnode) soluid)) ; will already have been coalesced here
         (coalesce thisnode reply-kind soluid)) ; won't have already been coalesced if we timed out!
        (where-to-send-reply (get-upstream-source thisnode soluid)))
    ; clean up reply tables.
    (kvs:remove-key (repliers-expected thisnode) soluid) ; might not have been done if we timed out
    (kvs:remove-key (reply-cache thisnode) soluid)
    (kvs:remove-key (timers thisnode) soluid)
    (kvs:remove-key (timeout-handlers thisnode) soluid)
    ; must create a new reply here; cannot reuse an old one because its content has changed
    (let ((reply (make-final-reply :solicitation-uid soluid
                                   :kind reply-kind
                                   :args (list coalesced-data))))
      (cond ((uid? where-to-send-reply) ; should be a uid or T. Might be nil if there's a bug.
             (maybe-log thisnode :SEND-FINAL-REPLY reply :to (briefname where-to-send-reply "node") coalesced-data)
             (send-msg reply
                       where-to-send-reply
                       (uid thisnode)))
            ; if no place left to reply to, just log the result.
            ;   This can mean that thisnode autonomously initiated the request, or
            ;   somebody running the sim told it to.
            (t
             (maybe-log thisnode :FINALREPLY (briefname soluid "sol") coalesced-data)
             (when (functionp where-to-send-reply)
               (funcall where-to-send-reply reply)))))
    coalesced-data ; mostly for debugging
    ))

#|
DISCUSSION:
send-interim-reply needs to be delayed, and I don't know how to do this with actors.
interim-replies don't resolve anything in the gossip model; they just provide (sometimes valuable)
partial information to the ultimate solicitor. However, if things are moving along properly,
a huge flurry of almost content-free interim-replies doesn't accomplish anything except to
clog up communication.

I don't want to get rid of interim-replies altogether because they'll be needed in cases of any
malfunctioning nodes. But I'd like to delay them a bit -- put them on the actor's back-burner
as it were.
Specifically what I need is a way to tell an actor
1. Put the send-interim-reply on the back burner and if nothing usurps it, execute it after a 1 second
delay.
2. If a send-final-reply happens, remove any send-interim-reply requests from the back-burner.
3. If a second send-interim-reply request comes in when one is already on the back-burner, do nothing,
because the original will take care of it. However, do reset the clock to make it execute 1 second
from now, rather than 1 second from whatever time the original request set up.

There are a couple of tricky issues here I don't know how to solve:
-- How to create a local "back-burner" queue on an actor. Conceivably this could just be another slot
on an actor, and #'next-message could be made to look at it last. Or possibly it could be put on the
actor-message-mbox at lowest priority?
-- How to ensure the actor gets put back on the *actor-ready-queue* 1 second from now.

Another solution might be to just give actors a "yield" capability such that they put themselves back
onto the far end of the *actor-ready-queue* and give other actors (or technically, other messages to
themselves) a chance to run first.

Can an actor be on the *actor-ready-queue* more than once? I would hope so. But no. The queue doesn't even
really contain actors per se; it just contains lexical closures, all of which are associated with some
actor. However, the way #'send works is that it never re-adds an actor's run closure to the queue if the
actor is already busy.

I suppose the simplest solution is to just make a timer for this, but the resolution of the timers
is 1 second and that's almost too coarse-grained for the job.

IDEA: An actor cannot re-insert itself onto the ready-queue -- because even if an actor added
some code to the ready-queue for itself to run later, the actor-busy flag would still be a problem.
When the current function ended, the actor would negate the actor-busy flag, but the other body
of code would already be in the queue. Without its actor-busy flag (the flag is attached to the actor,
not to the lambda body in the ready-queue) the other lambda body could cause problems. We could
change the actor-busy flag to a semaphore so it could take on a integral value of how many times the
actor occurred in the queue, but I think a better solution is:

Just spin up another actor. We'll call it the echo-actor. And it's sole job will be to "reflect" a message
back to us. This way, an actor can cause a message to itself to get reinserted into the head of the queue
normally and that code will be executed later, if any other lambda bodies are in the queue ahead of it.
Because an actor cannot properly send a message to itself, but it can send a message to another actor that
gets sent back, and everything will be copacetic.
|#

(defun send-interim-reply (thisnode reply-kind soluid where-to-send-reply)
  "Send an interim reply, right now."
  (let ((coalesced-data (coalesce thisnode reply-kind soluid)))
    (if (uid? where-to-send-reply) ; should be a uid or T. Might be nil if there's a bug.
        (let ((reply (make-interim-reply :solicitation-uid soluid
                                         :kind reply-kind
                                         :args (list coalesced-data))))
          (maybe-log thisnode :SEND-INTERIM-REPLY reply :to (briefname where-to-send-reply "node") coalesced-data)
          (send-msg reply
                    where-to-send-reply
                    (uid thisnode)))
        ; if no place left to reply to, just log the result.
        ;   This can mean that thisnode autonomously initiated the request, or
        ;   somebody running the sim told it to.
        (maybe-log thisnode :INTERIMREPLY (briefname soluid "sol") coalesced-data))))

(defun send-delayed-interim-reply (thisnode reply-kind soluid)
  "Called by a node actor to tell itself (or another actor, but we never do that now)
  to maybe send an interim reply after it processes any possible interim messages, which
  could themselves obviate the need for an interim reply."
  ; Should we make these things be yet another class with solicitation-uid-mixin?
  ; Or just make a general class of administrative messages with timeout being one?
  (let ((msg (make-solicitation
              :kind :maybe-sir
              :args (list soluid reply-kind))))
    (maybe-log thisnode :ECHO-MAYBE-SIR nil)
    (relay-msg msg (uid thisnode) (uid thisnode))))

(defun later-reply? (new old)
  "Returns true if old is nil or new has a higher uid"
  (or (null old)
      (> (uid new) (uid old))))

;;; Memorizing previous replies
(defmethod set-previous-reply ((node gossip-node) soluid srcuid reply)
  "Remember interim reply indexed by soluid and srcuid"
  (let ((interim-table (kvs:lookup-key (repliers-expected node) soluid)))
    (unless interim-table
      (setf interim-table (kvs:make-store ':hashtable :test 'equal))
      (kvs:relate-unique! (repliers-expected node) soluid interim-table))
    (kvs:relate-unique! interim-table srcuid reply)))

(defmethod get-previous-reply ((node gossip-node) soluid srcuid)
  "Retrieve interim reply indexed by soluid and srcuid"
  (let ((interim-table (kvs:lookup-key (repliers-expected node) soluid)))
    (when interim-table
      (kvs:lookup-key interim-table srcuid))))

(defmethod remove-previous-reply ((node gossip-node) soluid srcuid)
  "Remove interim reply (if any) indexed by soluid and srcuid from repliers-expected table.
  Return true if table is now empty; false if any other expected repliers remain.
  Second value is true if given soluid and srcuid was indeed present in repliers-expected."
  (let ((interim-table (kvs:lookup-key (repliers-expected node) soluid)))
    (unless interim-table
      (return-from remove-previous-reply (values t nil))) ; no table found. Happens for soluids where no replies were ever expected.
    (multiple-value-bind (store was-present?)
                         (kvs:remove-key! interim-table srcuid) ; whatever was there before should have been nil or an interim-reply, but we're not checking for that
      (declare (ignore store))
      (cond ((zerop (hash-table-count interim-table))
             ; if this is the last reply, kill the whole table for this soluid and return true
             ;; (maybe-log node :KILL-TABLE-1 nil)
             (kvs:remove-key! (repliers-expected node) soluid)
             (values t was-present?))
            (t (values nil was-present?))))))

(defun record-interim-reply (new-reply node soluid srcuid)
  "Record new-reply for given soluid and srcuid on given node.
  If new-reply is later or first one, replace old and return true.
  Otherwise return false."
  (let* ((previous-interim (get-previous-reply node soluid srcuid)))
    (when (or (null previous-interim)
              (later-reply? new-reply previous-interim))
      (set-previous-reply node soluid srcuid new-reply)
      t)))

(defmethod coalescer ((kind (eql :LIST-ALIVE)))
  "Proper coalescer for :list-alive responses. Might
   want to add a call to remove-duplicates here if we start
   doing a less deterministic gossip protocol (one not guaranteed to fully cover the graph,
   and also not guaranteed not to contain redundancies)."
  'append)

(defmethod coalescer ((kind (eql :COUNT-ALIVE)))
  "Proper coalescer for :count-alive responses. Might
   want to add a call to remove-duplicates here if we start
   doing a less deterministic gossip protocol (one not guaranteed to fully cover the graph,
   and also not guaranteed not to contain redundancies)."
  '+)

(defmethod coalescer ((kind (eql :GOSSIP-LOOKUP-KEY)))
  "Proper coalescer for :GOSSIP-LOOKUP-KEY responses."
  'multiple-tally)

(defmethod coalesce ((node gossip-node) kind soluid)
  "Grab all the data that interim repliers have sent me so far, combine it with my
  local reply-cache in a way that's specific to kind, and return result.
  This purely functional and does not change reply-cache."
  (let* ((local-data    (kvs:lookup-key (reply-cache node) soluid))
         (interim-table (kvs:lookup-key (repliers-expected node) soluid))
         (interim-data (when interim-table (loop for reply being each hash-value of interim-table
                         while reply collect (first (args reply)))))
         (coalescer (coalescer kind)))
    ;(maybe-log node :COALESCE interim-data local-data)
    (let ((coalesced-output
           (reduce coalescer
            interim-data
            :initial-value local-data)))
      ;(maybe-log node :COALESCED-OUTPUT coalesced-output)
      coalesced-output)))

(defun cancel-replier (thisnode reply-kind soluid srcuid)
  "Actions to take when a replier should be canceled.
  If no more repliers, must reply upstream with final reply.
  If more repliers, reply upstream either immediately or after a yield period with an interim-reply."
  (multiple-value-bind (no-more-repliers was-present?) (remove-previous-reply thisnode soluid srcuid)
    (cond (no-more-repliers
           (let ((timeout-handler (kvs:lookup-key (timeout-handlers thisnode) soluid)))
             (when timeout-handler ; will be nil for messages that don't expect a reply
               (funcall timeout-handler nil))))
          (t ; more repliers are expected
           ; functionally coalesce all known data and send it upstream as another interim reply
           (if *delay-interim-replies*
               (send-delayed-interim-reply thisnode reply-kind soluid)
               (let ((upstream-source (get-upstream-source thisnode soluid)))
                 (send-interim-reply thisnode reply-kind soluid upstream-source)))))
    was-present?
    ))

;;;; END OF REPLY SUPPORT ROUTINES

; For debugging list-alive
(defun missing? (alive-list)
  "Returns a list of node UIDs that are missing from a list returned by list-alive"
  (let ((dead-list nil))
    (maphash (lambda (key value)
               (declare (ignore value))
               (unless (member key alive-list)
                 (push key dead-list)))
             *nodes*)
    (sort dead-list #'<)))

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

(defmethod deliver-gossip-msg (gossip-msg (node gossip-node) srcuid)
  "Actor version. Does not launch a new thread because actor infrastructure will already have done that."
  (setf gossip-msg (copy-message gossip-msg)) ; must copy before incrementing hopcount because we can't
  ;  modify the original without affecting other threads.
  (incf (hopcount gossip-msg))
  ; Remember the srcuid that sent me this message, because that's where reply will be forwarded to
  (locally-receive-msg gossip-msg node srcuid))

(defmethod deliver-gossip-msg (msg (node remote-gossip-node) srcuid)
  "This node is a standin for a remote node. Transmit message across network."
   (transmit-msg msg node srcuid))

(defun run-gossip-sim ()
  "Archive the current log and clear it.
   Prepare all nodes for new simulation.
   Only necessary to call this once, or
   again if you change the graph or want to start with a clean log."
  (vector-push-extend *log* *archived-logs*)
  (setf *log* (new-log))
  (maphash (lambda (uid node)
             (declare (ignore uid))
             (setf (car (ac::actor-busy (actor node))) nil)
             (clear-caches node))
           *nodes*)
  (hash-table-count *nodes*))

; (make-graph 10)
; (save-graph-to-file "~/gossip/10nodes.lisp")
; (restore-graph-from-file "~/gossip/10nodes.lisp")
; (restore-graph-from-file "~/gossip/10nodes2.lisp") ;;; best test network
; (make-graph 5)
; (save-graph-to-file "~/gossip/5nodes.lisp")
; (restore-graph-from-file "~/gossip/5nodes.lisp")

; (make-graph 100)
; (make-graph 1000)
; (make-graph 10000)
; (visualize-nodes *nodes*)  ; probably not a great idea for >100 nodes

; (run-gossip-sim)
; (solicit-wait (first (listify-nodes)) :list-alive)
; (solicit-progress (first (listify-nodes)) :list-alive)
; (solicit-wait (first (listify-nodes)) :count-alive)
; (solicit (first (listify-nodes)) :announce :foo)
; (solicit-wait 340 :list-alive) ; if there happens to be a node with UID 340
; (inspect *log*) --> Should see :FINALREPLY with all nodes (or something slightly less, depending on network delays, etc.) 

; (solicit (first (listify-nodes)) :gossip-relate-unique :foo :bar)
; (solicit-wait (first (listify-nodes)) :gossip-lookup-key :foo)
; (solicit (first (listify-nodes)) :gossip-relate :foo :baz)
; (solicit-wait (first (listify-nodes)) :gossip-lookup-key :foo)

;; should produce something like (:FINALREPLY "node209" "sol255" (((:BAZ :BAR) . 4))) as last *log* entry

(defun get-kvs (key)
  "Shows value of key for all nodes. Just for debugging. :gossip-lookup-key for real way to do this."
  (let ((nodes (listify-nodes)))
    (mapcar (lambda (node)
              (cons node (kvs:lookup-key (local-kvs node) key)))
            nodes)))

; TEST NO-REPLY MESSAGES
; (make-graph 10)
; (run-gossip-sim)
; (solicit (first (listify-nodes)) :gossip-relate-unique :foo :bar)
; (get-kvs :foo) ; should return a list of (node . BAR)
; (solicit (first (listify-nodes)) :gossip-remove-key :foo)
; (get-kvs :foo) ; should just return a list of (node . nil)
; (solicit (first (listify-nodes)) :gossip-tally :foo 1)
; (get-kvs :foo) ; should return a list of (node . 1)
; (solicit (first (listify-nodes)) :gossip-tally :foo 1)
; (get-kvs :foo) ; should return a list of (node . 2)

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
(setf msg (make-solicitation :kind :list-alive))
#+IGNORE
(copy-message msg)