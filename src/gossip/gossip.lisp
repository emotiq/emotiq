;;; gossip.lisp

;;; More realistic gossip protocol.
;;; Minimal changes to use actor concurrency model. As far as the actors are concerned,
;;;  we're just sending messages of type :gossip, regardless of whether they're solicitations,
;;;  replies, or timeouts.
;;; Gossip messages are simply sent as payload of a lower-level actor message.
;;; No more process-run-function because actors take care of that.
;;; No need for locks to access node data structures because they're always accessed from an actor's single thread.

;;;; TODO:
;;;; Change things such that when forward-to is an integer, if a node gets an active ignore back from
;;;;  neighbor X, it should pick another neighbor if it has more available.

;;;; NOTES: "Upstream" means "back to the solicitor: the node that sent me a solicitation in the first place"

(in-package :gossip)

(defparameter *default-uid-style* :short ":tiny, :short, or :long. :short is shorter; :long is more comparable with other emotiq code.
       :tiny should only be used for testing, documentation, and graph visualization of nodes on a single machine since it creates extremely short UIDs")
(defparameter *max-message-age* 30 "Messages older than this number of seconds will be ignored")
(defparameter *max-seconds-to-wait* 10 "Max seconds to wait for all replies to come in")
(defparameter *direct-reply-max-seconds-to-wait* *max-seconds-to-wait* "Max second to wait for direct replies")

;;;; DEPRECATE
(defparameter *use-all-neighbors* 2 "True to broadcast to all neighbors; nil for none (no forwarding).
                                       Can be an integer to pick up to n neighbors.")
;;;; DEPRECATE
(defparameter *active-ignores* t "True to reply to sender when we're ignoring a message from that sender.")
(defparameter *nominal-gossip-port* 65002 "Nominal gossip network port to be used. We may increment this *max-server-tries* times
   in an effort to find an unused local port")
(defparameter *max-server-tries* 4 "How many times we can try finding an unused local server port before we give up. 1 to only try once.")
(defvar *actual-udp-gossip-port* nil "Local gossip UDP network port actually in use")
(defvar *actual-tcp-gossip-port* nil "Local gossip TCP network port actually in use")
(defvar *udp-gossip-socket* nil "Local passive UDP gossip socket, if any")
(defvar *tcp-gossip-socket* nil "Local passive TCP gossip socket, if any")
(defparameter *max-server-tries* 4 "How many times we can try finding an unused local server port before we give up. 1 to only try once.")
(defparameter *shutting-down*     nil "True to shut down gossip-server")
(defparameter *shutdown-msg* (make-array 8 :element-type '(UNSIGNED-BYTE 8) :initial-contents #(115 104 117 116 100 111 119 110)) "shutdown")
(defun shutdown-msg-len () (length *shutdown-msg*))
(defvar *udp-proxy-table* (make-hash-table) "Hash table of all UDP proxies on this machine")
(defvar *tcp-proxy-table* (make-hash-table) "Hash table of all TCP proxies on this machine")

(defvar *tcp-gossip-server-name* "TCP Gossip Server")
(defvar *udp-gossip-server-name* "UDP Gossip Server")

(defvar *debug* 4 "True to log debugging information while handling gossip requests. Larger values log more messages.")

(defun debug-level (&optional level)
  (when *debug*
    (if (numberp *debug*)
      (if (numberp level)
        (>= *debug* level)
        t)
      t)))

; Typical cases:
; Case 1: *use-all-neighbors* = true and *active-ignores* = nil. The total-coverage (neighborcast) case.
; Case 2: *use-all-neighbors* = 1 and *active-ignores* = true. The more common gossip case.
; Case 3: *use-all-neighbors* = true and *active-ignores* = true. More messages than necessary. NOW A FORBIDDEN CASE.
; Case 4: *use-all-neighbors* = 1 and *active-ignores* = false. Lots of timeouts. Poor results. NOW A FORBIDDEN CASE.

;;; DEPRECATE. Use forward-to slot on messages for this now.
(defun set-protocol-style (kind &optional (n 1))
  "Set style of protocol.
   :gossip style to pick one neighbor at random to send solicitations to.
      There's no guarantee this will reach all nodes. But it's quicker and more realistic.
   :neighborcast   style to send solicitations to all neighbors. More likely to reach all nodes but replies may be slower.
   Neither style should result in timeouts of a node waiting forever for a response from a neighbor."
  (case kind
    (:gossip (setf *use-all-neighbors* n
                   *active-ignores* t))  ; must be true when *use-all-neighbors* is nil. Otherwise there will be timeouts.
    (:neighborcast  (setf *use-all-neighbors* t
                   *active-ignores* nil)))
  kind)

(defun get-protocol-style ()
  "Get style of protocol. See set-protocol-style."
  (cond ((and (null *use-all-neighbors*)
              *active-ignores*)
         :noforward) ; use 0 neighbors means "no forward" which is generally counterproductive except during bootstrapping
        ((and (integerp *use-all-neighbors*)
              *active-ignores*)
         (if (zerop *use-all-neighbors*)
             (values :noforward)
             (values :gossip *use-all-neighbors*)))
        ((and (eql t *use-all-neighbors*)
              (null *active-ignores*))
         :neighborcast)
        (t :unknown)))
             
; (set-protocol-style :neighborcast) ; you should expect 100% correct responses most of the time in this mode
; (set-protocol-style :gossip)       ; you should expect 100% correct responses very rarely in this mode

#|
Discussion: :gossip style is more realistic for "loose" networks where nodes don't know much about their neighbors.
:neighborcast style is better after :gossip style has been used to discover the graph topology and agreements about collaboration
are in place between nodes.
|#

(defparameter *gossip-absorb-errors* t "True for normal use; nil for debugging")
(defvar *last-uid* 0 "Simple counter for making UIDs. Only used for :short style UIDs.")
(defvar *uid-lock* (mpcompat:make-lock) "Lock for *last-uid*")

(defvar *last-tiny-uid* 0 "Simple counter for making UIDs")
(defvar *tiny-uid-lock* (mpcompat:make-lock) "Lock for *last-tiny-uid*")

(defparameter *log-filter* t "t to log all messages; nil to log none")
(defparameter *delay-interim-replies* t "True to delay interim replies.
  Should be true, especially on larger networks. Reduces unnecessary interim-replies while still ensuring
  some partial information is propagating in case of node failures.")

(defun make-uid-mapper ()
  "Returns a table that maps UIDs to objects"
  (kvs:make-store ':hashtable :test 'equal))

;;; defglobal here makes running tests easier
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

;; Ex: Don't include SIR messages
;; (setf *log-filter* (log-exclude "SIR"))

(defun aws-p ()
  "Returns true if we're running on an AWS EC2 instance.
   Always runs quickly."
  ; https://serverfault.com/questions/462903/how-to-know-if-a-machine-is-an-ec2-instance
  (and (probe-file "/sys/hypervisor/uuid")
       (with-open-file (s "/sys/hypervisor/uuid")
         (let ((line (read-line s nil nil nil)))
           (and line
                (> (length line) 3)
                (equalp "ec2" (subseq line 0 3)))))))

(defun eripa-via-network-lookup ()
  "Returns ERIPA for current process via http lookup at external site"
  (let ((dq (with-output-to-string (s)
              (cond ((aws-p)
                     (http-fetch "http://169.254.169.254/latest/meta-data/public-ipv4" s))
                    (t (http-fetch "http://api.ipify.org/" s))))))
    (when (and (stringp dq)
               (> (length dq) 0))
      (values (usocket::host-to-hbo dq)
              dq))))

(defun externally-routable-ip-address ()
  "Returns ERIPA for current process"
  ; add mechanisms to get this from command-line args or environment variables which should take precedence
  ;   over network lookup
  (eripa-via-network-lookup))

(defparameter *eripa* nil "Cached EXTERNALLY-ROUTABLE-IP-ADDRESS")

(defun eripa ()
  "Return externally-routable ip address of this machine. This is the primary user function.
   Caches result for quick repeated use."
  (or *eripa*
      (setf *eripa* (externally-routable-ip-address))))

(defun encode-uid (machine-id process-id numeric-id)
  (logior (ash machine-id 48)
          (ash process-id 32)
          numeric-id))

; Should do this with an actor gatekeeper, but I don't know how to wrap an actor around a read operation
(defun generate-new-monotonic-id ()
    (mpcompat:with-lock (*uid-lock*)
      (incf *last-uid*)))

#+LISPWORKS
(progn
  (fli:define-c-typedef pid_t :int)
  (fli:define-foreign-function getpid ()
    :result-type pid_t))

; Somewhat shorter than the longer uuid version, (80 bits vs. 128) and more useful for simulating with
;   two processes on the same machine because it uses PID as part of the ID. Should still work
;   fine even in real (not simulated) systems.
(defun short-uid ()
  (encode-uid (logand #xFFFFFFFF (eripa)) ; 32 bits
              (logand #xFFFF #+OPENMCL (ccl::getpid) #+LISPWORKS (getpid)) ; 16 bits
              (logand #xFFFFFFFF (generate-new-monotonic-id)))) ; 32 bits

(defun long-uid ()
  (uuid::uuid-to-integer (uuid::make-v1-uuid)))

; Should do this with an actor gatekeeper, but I don't know how to wrap an actor around a read operation
(defun new-tiny-uid ()
  (mpcompat:with-lock (*tiny-uid-lock*)
    (incf *last-tiny-uid*)))

(defun new-uid (&optional (style *default-uid-style*))
  "Generate a new UID of the given style"
  (case style
    (:long  (long-uid))
    (:short (short-uid))
    (:tiny  (new-tiny-uid))))
                   
(defun uid? (thing)
  (integerp thing))

(defun uid= (thing1 thing2)
  (eql thing1 thing2))

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

(defun find-process (name)
  "Returns a process with given name, if any."
  (find-if (lambda (process) (ignore-errors (string-equal name (subseq (process-name process) 0 (length name))))) (all-processes)))

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
  ((forward-to :initarg :forward :initform *use-all-neighbors* :accessor forward-to
             :documentation "Normally true, which means this solicitation should be forwarded to neighbors
             of the one that originally received it. If nil, it means never forward. In that case, if a reply is expected,
             just reply immediately.
             It can be an integer n, which means to forward to up to n random neighbors. (This is traditional
             gossip protocol.)
             It can also simply be T, which means to forward to all neighbors. (This is neighborcast protocol.)")
   (reply-to :initarg :reply-to :initform nil :accessor reply-to
             :documentation "Nil for no reply expected. :UPSTREAM or :GOSSIP or :NEIGHBORCAST indicate
             replies should happen by one of those mechanisms, or a UID to request a direct reply to
             a specific node.")))

(defun make-solicitation (&rest args)
  (apply 'make-instance 'solicitation args))

(defmethod neighborcast? ((msg solicitation))
  "True if message is requesting neighborcast protocol"
  (eql t (forward-to msg)))

(defmethod copy-message :around ((msg solicitation))
  (let ((new-msg (call-next-method)))
    (setf (forward-to new-msg) (forward-to msg)
          (reply-to new-msg) (reply-to msg))
    new-msg))

(defclass solicitation-uid-mixin ()
  ((solicitation-uid :initarg :solicitation-uid :initform nil :accessor solicitation-uid
                     :documentation "UID of solicitation message that elicited this reply.")))

; Note: If you change a message before forwarding it, you need to create a new
;   message with a new UID. (Replies can often be changed as they percolate backwards;
;   they need new UIDs so that a node that has seen one set of information won't
;   automatically ignore it.)
(defclass reply (gossip-message-mixin solicitation-uid-mixin)
  ()
  (:documentation "Reply message. Used to reply to solicitations that need an :UPSTREAM or direct reply.
     This class is not used for :GOSSIP or :NEIGHBORCAST replies; those replies are just new solicitations.
     Why? Because only :UPSTREAM replies require the coalescence (i.e. reduce) mechanism and
     for those, an expectation needs to be set up in the sending node that a reply will be coming back,
     and a teardown mechanism happens when all expected replies have come back or a timeout occurs.
     No coalescence, expectation, or teardown needs to happen for :GOSSIP or :NEIGHBORCAST replies.

     Direct replies do establish expectations, so they use the reply class. Direct replies also use coalescence
     but only at the ultimate receiver node since there are no other nodes where coalescence could occur.

     It is common and expected to receive multiple :UPSTREAM or direct replies matching a given solicitation-uid.
     :UPSTREAM replies are 'inverse broadcasts' in that they have many sources that funnel
     back to one ultimate receiver -- the originator of the solicitation.
     Direct replies can be similar except the message is sent directly from the ultimate recipient back
     to the ultimate sender without being forwarded through the network."))

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
    (when (null (solicitation-uid timeout)) (error "No solicitation-uid on timeout"))
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
  ((address :initarg :address :initform (eripa) :accessor address
            :documentation "Network address (e.g. IP) of node.")
   (logfn :initarg :logfn :initform 'default-logging-function :accessor logfn
          :documentation "If non-nil, assumed to be a function called with every
              message seen to log it.")))
   
(defclass gossip-actor (ac:actor)
  ((node :initarg :node :initform nil :accessor node
         :documentation "The gossip-node on behalf of which this actor works")))

#+OBSOLETE
(defclass gossip-network-actor (ac:actor)
  ((node :initarg :node :initform nil :accessor node
         :documentation "The gossip-node on behalf of which this actor works.
         Should only be attached to proxy-gossip-nodes.")))

(defclass actor-mixin ()
  ((actor :initarg :actor :initform nil :accessor actor
          :documentation "Actor for this node")))

(defclass gossip-node (gossip-mixin actor-mixin)
  ((message-cache :initarg :message-cache :initform (make-uid-mapper) :accessor message-cache
                  :documentation "Cache of seen messages. Table mapping UID of message to UID of upstream sender.
                  Used to ensure identical messages are not acted on twice, and to determine where
                  replies to a message should be sent in case of :UPSTREAM messages.")
   (repliers-expected :initarg :repliers-expected :initform (kvs:make-store ':hashtable :test 'equal)
                      :accessor repliers-expected
                      :documentation "2-level Hash-table mapping a solicitation id to another hashtable of srcuids
                     that I expect to reply to that solicitation. Values in second hashtable are either interim replies
                     that have been received from that srcuid for the given solicitation id.
                     Only accessed from this node's actor thread. No locking needed.
                     See Note B below for more info.")
   (reply-cache :initarg :reply-cache :initform (kvs:make-store ':hashtable :test 'equal)
               :accessor reply-cache
               :documentation "Hash-table mapping a solicitation id to some data applicable to THIS node
                  for that solicitation. Only accessed from this node's actor
                  thread. No locking needed. Only used for duration of a solicitation/reply cycle.")
   (local-kvs :initarg :local-kvs :initform (kvs:make-store ':hashtable :test 'equal) :accessor local-kvs
        :documentation "Local persistent key/value store for this node. Put long-lived global state data here.")
   (neighbors :initarg :neighbors :initform nil :accessor neighbors
              :documentation "List of UIDs of direct neighbors of this node. This is the mechanism that establishes
              the connectivity of the node graph.")
   (timers :initarg :timers :initform nil :accessor timers
            :documentation "Table mapping solicitation uids to a timer dealing with replies to that uid.")
   (timeout-handlers :initarg :timeout-handlers :initform nil :accessor timeout-handlers
                     :documentation "Table of functions of 1 arg: timed-out-p that should be
          called to clean up after waiting operations are done. Keyed on solicitation id.
          Timed-out-p is true if a timeout happened. If nil, it means operations completed without
          timing out.")))

(defmethod clear-caches ((node gossip-node))
  "Caches should be cleared in the normal course of events, but this can be used to make sure."
  (kvs:clear-store! (message-cache node))
  (kvs:clear-store! (repliers-expected node))
  (kvs:clear-store! (reply-cache node))
  ; don't clear the local-kvs. That should be persistent.
  )

; We'll use these for real (not simulated on one machine) protocol over the network
(defclass proxy-gossip-node (gossip-mixin actor-mixin)
  ((real-address :initarg :real-address :initform nil :accessor real-address
            :documentation "Real IP or DNS address of remote node")
   (real-port :initarg :real-port :initform *nominal-gossip-port* :accessor real-port
              :documentation "Real port of remote node")
   (real-uid :initarg :real-uid :initform nil :accessor real-uid
                  :documentation "UID of the (real) gossip node on the other end.
               This proxy node will also have its own UID just for local lookup purposes,
               but the real-uid is used to route it to the proper node on the remote
               machine."))
  (:documentation "A local [to this process] standin for a real gossip-node located elsewhere.
              All we know about it is its UID and address, which is enough to transmit a message to it."))

(defmethod clear-caches ((node proxy-gossip-node))
  "Caches should be cleared in the normal course of events, but this can be used to make sure."
  )

(defclass tcp-gossip-node (proxy-gossip-node)
  ())

(defclass udp-gossip-node (proxy-gossip-node)
  ())

(defmethod gossip-dispatcher (node &rest actor-msg)
  "Extracts gossip-msg from actor-msg and calls deliver-gossip-msg on it"
  (let ((gossip-cmd (first actor-msg)))
    (case gossip-cmd
      (:gossip
       (unless node (error "No node attached to this actor!"))
       (destructuring-bind (srcuid gossip-msg) (cdr actor-msg)
         (deliver-gossip-msg gossip-msg node srcuid))))))

(defmethod make-gossip-actor ((node t))
  (make-instance 'gossip-actor
    :node node
    :fn 
    (lambda (&rest msg)
      (apply 'gossip-dispatcher node msg))))

#+OBSOLETE
(defmethod make-gossip-actor ((node null))
  (make-instance 'gossip-actor
    :node node
    :fn 
    (lambda (&rest msg)
      (apply 'gossip-dispatcher node msg))))

#+OBSOLETE
(defmethod make-gossip-actor ((node proxy-gossip-node))
  (make-instance 'gossip-network-actor
    :node node
    :fn 
    (lambda (&rest msg)
      (apply 'gossip-dispatcher node msg))))

(defun make-node (&rest args)
  "Makes a new node"
  (let* ((node (apply 'make-instance 'gossip-node args))
         (actor (make-gossip-actor node)))
    (setf (actor node) actor)
    (kvs:relate-unique! *nodes* (uid node) node)
    node))

(defun make-proxy-node (mode &rest args &key proxy-subtable &allow-other-keys)
  "Makes a new proxy node of given mode: :UDP or :TCP"
  (remf args :proxy-subtable)
  (let* ((node (case mode
                 (:tcp (apply 'make-instance 'tcp-gossip-node args))
                 (:udp (apply 'make-instance 'udp-gossip-node args))))
         (actor (make-gossip-actor node)))
    (setf (actor node) actor)
    (kvs:relate-unique! *nodes* (uid node) node)
    (memoize-proxy node proxy-subtable)))

;;;; Graph making routines
(defun make-nodes (numnodes)
  (dotimes (i numnodes)
    (make-node)))

(defun listify-nodes (&optional (nodetable *nodes*))
  (loop for node being each hash-value of nodetable collect node))

(defun random-node (&optional (nodetable *nodes*))
  (let ((len (hash-table-count nodetable)))
    (when (> len 0)
      (let ((num (random len))
            (i 0))
        (maphash (lambda (key value)
                   (declare (ignore key))
                   (if (>= i num)
                       (return-from random-node value)
                       (incf i)))
                 nodetable)))))

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

(defun make-graph (numnodes &optional (fraction 0.5))
  "Build a graph with numnodes nodes. Strategy here is to first connect all the nodes in a single
   non-cyclic linear path, then add f*n random edges, where n is the number of nodes."
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

#|
To send a message that doesn't require a reply:
Use #'solicit.
To send a message that does require a reply:
Use #'solicit-wait.

To send a message that you want a direct response to:
Use #'solicit-direct and make your message manually with the :reply-to slot being the UID of the node that should expect all replies.

To send a message from a NODE that the node wants a direct response to:
The node should make the message manually with the :reply-to slot being the UID of the node itself.
The node should call (send-msg msg uid #'final-continuation), where uid is the UID of the node itself [yes, you send
the message to yourself] and final-continuation is a single-argument function of a reply, where that reply is the coalesced reply
the node itself would send upstream if it were an :UPSTREAM message. If final-continuation is nil, that reply just gets
dropped on the floor.
|#

(defun solicit-direct (node kind &rest args)
  "Like solicit-wait but asks for all replies to be sent back to node directly, rather than percolated upstream.
  Don't use this on messages that don't expect a reply, because it'll wait forever."
  (unless node
    (error "No destination node supplied. You might need to run make-graph or restore-graph-from-file first."))
  (let ((uid (if (typep node 'gossip-mixin) ; yeah this is kludgy.
                 (uid node)
                 node))
        (response nil))
    (flet ((final-continuation (reply)
             (setf response reply)))
      (let* ((solicitation (make-solicitation
                            :reply-to (uid node)
                            :kind kind
                            :args args))
             (soluid (uid solicitation)))
        (send-msg solicitation
                  uid                   ; destination
                  #'final-continuation) ; srcuid. Note that nodes that are instructed to reply-to themselves will instead send the reply upstream
        (let ((win (mpcompat:process-wait-with-timeout "Waiting for reply" *max-seconds-to-wait*
                                                       (lambda () response))))
          (values 
           (if win
               (first (args response))
               :TIMEOUT)
           soluid))))))

(defun solicit (node kind &rest args)
  "Send a solicitation to the network starting with given node. This is the primary interface to 
  the network to start an action from the outside. Nodes shouldn't use this function to initiate an
  action because they should set the srcuid parameter to be their own rather than nil."
  (unless node
    (error "No destination node supplied. You might need to run make-graph or restore-graph-from-file first."))
  (let ((uid (if (typep node 'gossip-mixin) ; yeah this is kludgy.
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

#+LATER-ACTORS ; not working ATM. Figure it out later.
(defun solicit-wait (mbox node kind &rest args)
  "Like solicit but waits for a reply. Only works for :UPSTREAM replies.
  Don't use this on messages that don't expect a reply, because it'll wait forever."
  (unless node
    (error "No destination node supplied. You might need to run make-graph or restore-graph-from-file first."))
  (let* ((uid (if (typep node 'gossip-mixin) ; yeah this is kludgy.
                  (uid node)
                node))
         (solicitation (make-solicitation
                        :reply-to :UPSTREAM
                        :kind kind
                        :args args))
         (soluid (uid solicitation))
         (me (ac:current-actor)))
    (send-msg solicitation
              uid      ; destination
              me)
    (ac:recv
      ((list reply)
       (ac:send mbox (list (first (args reply)) soluid)))

      :ON-TIMEOUT (ac:send mbox (list :TIMEOUT soluid))
      :TIMEOUT *max-seconds-to-wait*)))

#+LATER-ACTORS ; not working ATM. Figure it out later.
(defun test-solicit-wait (node kind &rest args)
  (let ((mbox   (mpcompat:make-mailbox)))
    (apply 'ac:spawn 'solicit-wait mbox node kind args)
    (mpcompat::mailbox-read mbox)))
; (test-solicit-wait (first (listify-nodes)) :list-alive)

(defun solicit-wait (node kind &rest args)
  "Like solicit but waits for a reply. Only works for :UPSTREAM replies.
  Don't use this on messages that don't expect a reply, because it'll wait forever."
  (unless node
    (error "No destination node supplied. You might need to run make-graph or restore-graph-from-file first."))
  (let ((uid (if (typep node 'gossip-mixin) ; yeah this is kludgy.
                 (uid node)
                 node))
        (response nil))
    (flet ((final-continuation (reply)
             (setf response reply)))
      (let* ((solicitation (make-solicitation
                            :reply-to :UPSTREAM
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
  (let* ((uid (if (typep node 'gossip-mixin) ; yeah this is kludgy.
                  (uid node)
                  node))
         (node (if (typep node 'gossip-mixin)
                   node
                   (lookup-node node)))
         (response nil)
         (old-logger (logfn node)))
    (flet ((final-continuation (message)
             (setf response message)))
      (let* ((solicitation (make-solicitation
                            :reply-to :UPSTREAM
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

(defmethod briefname ((id null) &optional (prefix ""))
  (declare (ignore prefix))
  nil)

(defmethod briefname ((id t) &optional (prefix ""))
  (declare (ignore prefix))
  id)

; Logcmd: Keyword that describes what a node has done with a given message UID
; Examples: :IGNORE, :ACCEPT, :FORWARD, etc.
(defmethod maybe-log ((node gossip-mixin) logcmd msg &rest args)
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

(defvar *log* (new-log) "Log of gossip actions.")
(defvar *log-actor* (ac:make-actor (lambda (&rest logmsg) (vector-push-extend logmsg *log*)))
  "Actor which serves as gatekeeper to *log* to ensure absolute serialization of log messages and no resource contention for *log*.")

(defun default-logging-function (logcmd nodename msgname &rest args)
  (let* ((usec (usec::get-universal-time-usec)) ; important to do this before calling ac:send so it's most accurate
         (logmsg (if msgname
                    (list* usec logcmd nodename msgname args)
                    (list usec logcmd nodename))))
    (apply 'ac:send *log-actor* logmsg)
    logmsg))

(defun debug-log (&rest args)
  "Freeform version of default-logging-function"
  (apply 'ac:send *log-actor* (cons (usec::get-universal-time-usec) args)))

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
  
(defmethod send-msg ((msg solicitation) (destuid (eql 0)) srcuid)
  "Sending a message to destuid=0 broadcasts it to all local (non-proxy) nodes in *nodes* database.
   This is intended to be used by incoming-message-handler-xxx methods for bootstrapping messages
   before #'neighbors connectivity has been established."
  (let ((no-forward-msg (copy-message msg))
        (nodes (listify-nodes)))
    (setf (forward-to no-forward-msg) nil) ; don't let any node forward. Just reply immediately.
    (setf nodes (remove-if (lambda (node) (typep node 'proxy-gossip-node)) nodes))
    (setf nodes (mapcar 'uid nodes))
    (forward msg srcuid nodes)))

(defmethod send-msg ((msg gossip-message-mixin) destuid srcuid)
  (let* ((destnode (lookup-node destuid))
         (destactor (when destnode (actor destnode))))
    (when (null destactor)
      (if (null destnode)
          (error "Cannot find node for ~D" destuid)
          (error "Cannot find actor for node ~S" destnode)))
    (ac:send destactor
           :gossip ; actor-verb
           srcuid  ; first arg of actor-msg
           msg)))  ; second arg of actor-msg

(defun current-node ()
  (uiop:if-let (actor (ac:current-actor))
    (values (node actor) actor)))

(defmethod send-self ((msg gossip-message-mixin))
  "Send a gossip message to the current node, if any.
  Should only be called from within a gossip-actor's code; otherwise nothing happens."
  (multiple-value-bind (node actor) (current-node)
    (when node ; actor will always be true too
      (ac:send actor :gossip (uid node) msg))))

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
                        (if (eql :ANONYMOUS interim-table) ; accept replies from any srcuid
                            kindsym
                            (multiple-value-bind (val present-p) (kvs:lookup-key interim-table srcuid)
                              (declare (ignore val))
                              (if present-p
                                  kindsym
                                  (values nil :unexpected-1)))))
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

(defun remove-nth (n list)
  "Returns nth item and returns item and new list."
  (let ((newlist (loop for i in list
                   for idx from 0
                   unless (= idx n)
                   collect i)))
    (values (nth n list) newlist)))
  
(defun make-random-generator (list)
  "Returns a thunk that returns another random member of list
  every time it's called. Never generates same member twice
  (unless that member appears more than once in list in the first place).
  Generates nil when no members are left."
  (lambda ()
    (when list
      (let ((n (random (length list)))
            (item nil))
        (multiple-value-setq (item list) (remove-nth n list))
        item))))

(defun use-some-neighbors (neighbors howmany)
  "Pick a few random neighbors based on value of *use-all-neighbors*"
  (when neighbors
    (let ((len (length neighbors)))
      (cond ((integerp howmany)
             (if (< howmany 1)
                 nil ; if zero or less, return nil. Degenerate case.
                 (if (>= howmany len)
                     neighbors ; just use them all
                     (let ((fn (make-random-generator neighbors)))
                       (loop for i below howmany collect
                         (funcall fn))))))
            (howmany ; if true but not integer, just use them all
             neighbors)
            (t
             nil)))))

(defmethod get-downstream ((node gossip-node) srcuid howmany)
  (let ((all-neighbors (remove srcuid (neighbors node))))
    (use-some-neighbors all-neighbors howmany)))

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
                  ;   *active-ignores* = false [:neighborcast protocol]
                  ;   but it doesn't hurt anything in other cases.
                  (let ((was-present? (cancel-replier node (kind msg) soluid srcuid)))
                    (when was-present?
                      ; Don't log a :STOP-WAITING message if we were never waiting for a reply from srcuid in the first place
                      (maybe-log node :STOP-WAITING msg srcuid))
                    (unless (neighborcast? msg) ; not neighborcast means use active ignores. Neighborcast doesn't need them.
                      (send-active-ignore srcuid (uid node) (kind msg) soluid failure-reason)))))
               (t nil)))))))

(defmethod locally-receive-msg ((msg t) (thisnode proxy-gossip-node) srcuid)
  (declare (ignore srcuid))
  (error "Bug: Cannot locally-receive to a proxy node!"))

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
      "Cleanup operations if timeout happens, or all expected replies come in. This won't hurt anything
      if no cleanup was necessary, but it's a waste of time."
      (let ((timer (kvs:lookup-key (timers node) soluid)))
        (cond (timed-out-p
               ; since timeout happened, actor infrastructure will take care of unscheduling the timeout
               (maybe-log node :DONE-WAITING-TIMEOUT msg (more-replies-expected? node soluid t)))
              (t ; done, but didn't time out. Everything's good. So unschedule the timeout message.
               (cond (timer
                      (ac::unschedule-timer timer) ; cancel a timer prematurely, if any.
                      ; if no timer, then this is a leaf node
                      (maybe-log node :DONE-WAITING-WIN msg))
                     (t ; note: Following log message doesn't necessarily mean anything is wrong.
                      ; If node is singly-connected to the graph, it's to be expected
                      (maybe-log node :NO-TIMER-FOUND msg)))))
        (coalesce&reply (reply-to msg) node kind soluid)))))

(defmethod prepare-repliers ((thisnode gossip-node) soluid downstream)
  "Prepare reply tables for given node, solicitation uid, and set of downstream repliers."
  (let ((interim-table (kvs:make-store ':hashtable :test 'equal)))
    (dolist (replier-uid downstream)
      (kvs:relate-unique! interim-table replier-uid nil))
    (kvs:relate-unique! (repliers-expected thisnode) soluid interim-table)
    interim-table))

;;;; GOSSIP METHODS. These handle specific gossip protocol solicitations and replies.
;;;; Except for those marked "utility function", these are strictly message handlers; they're not functions a programmer should call.

;;; Timeout. Generic message handler for all methods that scheduled a timeout.
;;; These never expect a reply but they can happen for methods that did expect one.
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

(defmethod more-replies-expected? ((node gossip-node) soluid &optional (whichones nil))
  "Utility function. Can be called by message handlers.
  If whichones is true, returns not just true but an actual list of expected repliers [or nil if none]."
  (let ((interim-table (kvs:lookup-key (repliers-expected node) soluid)))
    (when interim-table
      (if (eql :ANONYMOUS interim-table)
          nil
          (if whichones
              (loop for key being each hash-key of interim-table collect key)
              (not (zerop (hash-table-count interim-table))))))))

(defmethod maybe-sir ((msg solicitation) thisnode srcuid)
  "Maybe-send-interim-reply. The sender of this message will be thisnode itself"
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

;;; NO-REPLY-NECESSARY methods. One method per message.

;; Because these messages never involve a reply, even if the reply-to slot is non-nil in these messages,
;;   it will be ignored.

(defmethod announce ((msg solicitation) thisnode srcuid)
  "Announce a message to the collective. First arg of Msg is the announcement,
   which can be any Lisp object. Recipient nodes are not expected to reply.
   This is probably only useful for debugging gossip protocols, since the only
   record of the announcement will be in the log."
  (let ((content (first (args msg))))
    (declare (ignore content))
    ; thisnode becomes new source for forwarding purposes
    (forward msg thisnode (get-downstream thisnode srcuid (forward-to msg)))))

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
    (forward msg thisnode (get-downstream thisnode srcuid (forward-to msg)))))

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
    (forward msg thisnode (get-downstream thisnode srcuid (forward-to msg)))))

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
    (forward msg thisnode (get-downstream thisnode srcuid (forward-to msg)))))

(defmethod gossip-tally ((msg solicitation) thisnode srcuid)
  "Increment the value of a given key by an increment amount.
   If no value for that key exists currently, set it to 1.
   No reply expected."
  (let ((key (first (args msg)))
        (increment (second (args msg))))
    (setf (local-kvs thisnode) (kvs:tally (local-kvs thisnode) key increment))
    ; thisnode becomes new source for forwarding purposes
    (forward msg thisnode (get-downstream thisnode srcuid (forward-to msg)))))

;;; TODO: add this to key-value-store
(defmethod tally-nondestructive ((store list) key amount &key (test #'equal))
  "Utility function. Purely functional version of kvs:tally. Only works on alists.
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

(defun multiple-tally (alist1 alist2)
  "Utility function. Given two alists like ((key1 . n1) (key2 . n2) ...)
  call kvs:tally for all pairs in alist."
  (let ((output nil))
    (dolist (pair alist2)
      (setf output (tally-nondestructive alist1 (car pair) (cdr pair))))
    output))

;; REPLY-NECESSARY methods. Three methods per message. The generic ones should handle most if not all cases.


;; We assume that we must coalesce results any time we expect replies from downstream nodes, whether the message
;;   says reply-to == :UPSTREAM or reply-to == (uid thisnode).
;; But of course if there are NO downstream nodes, we cannot expect any replies by definition, so coalescence is not necessary.
(defmethod generic-srr-handler ((msg solicitation) (thisnode gossip-node) srcuid)
  "Generic handler for solicitations requiring replies (SRRs)"
  (let* ((kind (kind msg))
         (soluid (uid msg))
         (downstream (get-downstream thisnode srcuid (forward-to msg))) ; don't forward to the source of this solicitation
         (timer nil)
         (seconds-to-wait *max-seconds-to-wait*)
         (cleanup (make-timeout-handler thisnode msg kind)))
    (flet ((initialize-reply-cache ()
             "Set up node's reply-cache with initial-reply-value for this message"
             (kvs:relate-unique! (reply-cache thisnode) soluid (initial-reply-value kind thisnode (args msg))))
           
           (must-coalesce? ()
             "Returns true if this node must coalesce any incoming replies.
             If nil, there is no possibility of repliers, so no coalescence is needed."
             (or (eql :UPSTREAM (reply-to msg))
                 (and (uid? (reply-to msg)) ; check for potential direct replies back to this node
                      (eql (reply-to msg) (uid thisnode))))))
      ; Always initialize the reply cache--even if there are no downstream nodes--because we assume that if this
      ;   node could potentially be replied to, it itself is always considered a participant node of the message.
      (when (must-coalesce?) (initialize-reply-cache))
      ; (if (not (eql 495831281100387564650501 (uid thisnode))) (break))
      
      ;;; It's possible for there to be a downstream but have no potential repliers. In that case
      ;;; we need to go ahead and reply.
      
      ;;; It's never necessary to coalesce when there's no downstream, because there won't be anything TO coalesce in that case.
      ;;; It MAY not be necessary to coalesce even when there IS a downstream (that's the case for direct replies where this node
      ;;;   is not the node to reply to).
      
      (cond (downstream
             (cond ((must-coalesce?)
                    (if (and (uid? (reply-to msg)) ; check for potential direct replies back to this node
                             (eql (reply-to msg) (uid thisnode)))
                        (progn
                          (setf seconds-to-wait *direct-reply-max-seconds-to-wait*)
                          (kvs:relate-unique! (repliers-expected thisnode) soluid :ANONYMOUS))
                        (prepare-repliers thisnode soluid downstream))
                    (forward msg thisnode downstream)
                    ; wait a finite time for all replies
                    (setf timer (schedule-gossip-timeout (ceiling seconds-to-wait) (actor thisnode) soluid))
                    (kvs:relate-unique! (timers thisnode) soluid timer)
                    (kvs:relate-unique! (timeout-handlers thisnode) soluid cleanup) ; bind timeout handler
                    ;(maybe-log thisnode :WAITING msg (ceiling *max-seconds-to-wait*) downstream)
                    (maybe-log thisnode :WAITING msg downstream))
                   (t ; just forward the message if there won't be an :UPSTREAM reply
                    (forward msg thisnode downstream)
                    ; No cleanup necessary because even though we had a downstream, we never expected any replies.
                    (send-final-reply thisnode (reply-to msg) soluid kind (initial-reply-value kind thisnode (args msg))))))
            (t ; No downstream. This is a leaf node. No coalescence is possible, which implies no cleanup is necessary.
             (send-final-reply thisnode (reply-to msg) soluid kind (initial-reply-value kind thisnode (args msg))))))))

; No node should ever receive an interim or final reply unless it was expecting one by virtue
;  of the original solicitation having a reply-to slot of :UPSTREAM or the UID of the node itself.
;  So there's no special handling in the methods for interim-reply or final-reply of other reply modes.
;  Besides, :GOSSIP and :NEIGHBORCAST reply modes will never cause the creation of a true reply message anyway.
(defmethod generic-srr-handler ((msg interim-reply) (thisnode gossip-node) srcuid)
  (let ((kind (kind msg))
        (soluid (solicitation-uid msg)))
    ; First record the data in the reply appropriately
    (unless (eql :ANONYMOUS (kvs:lookup-key (repliers-expected thisnode) soluid)) ; ignore interim-replies to anonymous expectations
      (when (record-interim-reply msg thisnode soluid srcuid) ; true if this reply is later than previous
        ; coalesce all known data and send it upstream as another interim reply.
        ; (if this reply is not later, drop it on the floor)
        (if *delay-interim-replies*
            (send-delayed-interim-reply thisnode kind soluid)
            (let ((upstream-source (get-upstream-source thisnode (solicitation-uid msg))))
              (send-interim-reply thisnode kind soluid upstream-source)))))))

(defmethod generic-srr-handler ((msg final-reply) (thisnode gossip-node) srcuid)
  (let* ((kind (kind msg))
         (soluid (solicitation-uid msg))
         (local-data (kvs:lookup-key (reply-cache thisnode) soluid))
         (coalescer (coalescer kind)))
    ; Any time we get a final reply, we destructively coalesce its data into local reply-cache
    (kvs:relate-unique! (reply-cache thisnode)
                        soluid
                        (funcall coalescer local-data (first (args msg))))
    (cancel-replier thisnode kind soluid srcuid)))

(defgeneric initial-reply-value (kind thisnode msgargs)
  (:documentation "Initial the reply-value for the given kind of message"))

(defmethod initial-reply-value ((kind (eql :gossip-lookup-key)) (thisnode gossip-node) msgargs)
  (let* ((key (first msgargs))
         (myvalue (kvs:lookup-key (local-kvs thisnode) key)))
    (list (cons myvalue 1))))

(defmethod initial-reply-value ((kind (eql :count-alive)) (thisnode gossip-node) msgargs)
  (declare (ignore msgargs))
  1)

(defmethod initial-reply-value ((kind (eql :list-alive)) (thisnode gossip-node) msgargs)
  (declare (ignore msgargs))
  (list (uid thisnode)))

(defmethod initial-reply-value ((kind (eql :list-addresses)) (thisnode gossip-node) msgargs)
  (declare (ignore msgargs))
  (list (address thisnode)))

(defmethod initial-reply-value ((kind (eql :find-max)) (thisnode gossip-node)  msgargs)
  (let ((key (first msgargs)))
    (kvs:lookup-key (local-kvs thisnode) key)))

(defmethod initial-reply-value ((kind (eql :find-min)) (thisnode gossip-node) msgargs)
  (let ((key (first msgargs)))
    (kvs:lookup-key (local-kvs thisnode) key)))

(defmethod count-alive ((msg gossip-message-mixin) (thisnode gossip-node) srcuid)
  "Return total count of nodes that received the message."
  (generic-srr-handler msg thisnode srcuid))

(defmethod list-alive ((msg gossip-message-mixin) (thisnode gossip-node) srcuid)
  "Like count-alive but this returns an actual list of node UIDs that received the message."
  (generic-srr-handler msg thisnode srcuid))

(defmethod list-addresses ((msg gossip-message-mixin) (thisnode gossip-node) srcuid)
  "Every node reports its address. Useful if every node is on a different machine."
  (generic-srr-handler msg thisnode srcuid))

(defmethod gossip-lookup-key ((msg gossip-message-mixin) (thisnode gossip-node) srcuid)
  "Lookup value of given key. Returns an alist of ((value1 . count1) (value2 . count2) ...)
   where valuex is a value found associated with the key and countx is the number of nodes with
   that value."
  (generic-srr-handler msg thisnode srcuid))

(defmethod find-max ((msg gossip-message-mixin) thisnode srcuid)
  "Retrieve maximum value of a given key on all the nodes. Non-numeric values are ignored."
  (generic-srr-handler msg thisnode srcuid))

(defmethod find-min ((msg gossip-message-mixin) thisnode srcuid)
  "Retrieve minimum value of a given key on all nodes"
  (generic-srr-handler msg thisnode srcuid))

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
  ;(forward msg thisnode (get-downstream thisnode srcuid (forward-to msg)))
  ; wait a finite time for all replies
  )

(defmethod find-node-with-address ((msg solicitation) thisnode srcuid)
  "Find address for a node with a given uid. Equivalent to reverse DNS lookup."
  ;(forward msg thisnode (get-downstream thisnode srcuid (forward-to msg)))
  ; wait a finite time for all replies
  )

;;; END OF GOSSIP METHODS

;;; REPLY SUPPORT ROUTINES

(defun send-final-reply (srcnode destination soluid kind data)
  (let ((reply (make-final-reply :solicitation-uid soluid
                                 :kind kind
                                 :args (list data)))
        (where-to-send-reply (cond ((uid? destination)
                                    destination)
                                   ((eq :UPSTREAM destination)
                                    (get-upstream-source srcnode soluid))
                                   (t destination))))
    (cond ((uid? where-to-send-reply) ; should be a uid or T. Might be nil if there's a bug.
           (maybe-log srcnode :SEND-FINAL-REPLY reply :to (briefname where-to-send-reply "node") data)
           (send-msg reply
                     where-to-send-reply
                     (uid srcnode)))
          ; if no place left to reply to, just log the result.
          ;   This can mean that srcnode autonomously initiated the request, or
          ;   somebody running the sim told it to.
          #+LATER-ACTORS ; not working ATM. Figure it out later.
          ((typep where-to-send-reply 'ac::actor) ; can happen if actor version of solicit-wait was used
           (ac:send where-to-send-reply reply (uid srcnode)))
          ((functionp where-to-send-reply)
           (maybe-log srcnode :FINALREPLY (briefname soluid "sol") data)
           (funcall where-to-send-reply reply))
          ((null where-to-send-reply)
           (maybe-log srcnode :NO-REPLY-DESTINATION! (briefname soluid "sol") data))
          (t (error "Invalid destination")))))

(defun coalesce&reply (reply-to thisnode reply-kind soluid)
  "Generic cleanup function needed for any message after all final replies have come in or
  timeout has happened. Only for upstream [reduce-style] replies; don't use otherwise.
  Cleans up reply tables and reply upstream with final reply containing coalesced data.
  Assumes any timers have already been cleaned up."
  (let ((coalesced-data ;(kvs:lookup-key (reply-cache thisnode) soluid)) ; will already have been coalesced here
         (coalesce thisnode reply-kind soluid))) ; won't have already been coalesced if we timed out!
    ; clean up reply tables.
    (kvs:remove-key (repliers-expected thisnode) soluid) ; might not have been done if we timed out
    (kvs:remove-key (reply-cache thisnode) soluid)
    (kvs:remove-key (timers thisnode) soluid)
    (kvs:remove-key (timeout-handlers thisnode) soluid)
    ; must create a new reply here; cannot reuse an old one because its content has changed
    (send-final-reply
     thisnode
     (if (uid? reply-to)
         (if (uid= reply-to (uid thisnode)) ; can't send a reply to myself, so punt and send it upstream
             :UPSTREAM
             reply-to)
         reply-to)
     soluid reply-kind coalesced-data)
    coalesced-data ; mostly for debugging
    ))

#|
DISCUSSION:
send-interim-reply needs to be delayed.
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

REMAINDER OF DISCUSSION IS OBSOLETE. WE SOLVE THE PROBLEM WITH SEND-SELF NOW.

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

FUNDAMENTAL MISUNDERSTANDING ABOVE: An actor must *never* be on the ready queue more than once because then
it could be executed by two processes concurrently, and that's *never* allowed. All atomicity guarantees of
the actor model would dissolve under such a scenario.

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
  ; Don't ever call this for replies to :ANONYMOUS expectations. See Note C.
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
    (send-self msg)))

(defun later-reply? (new old)
  "Returns true if old is nil or new has a higher uid"
  (or (null old)
      (> (uid new) (uid old))))

;;; Memorizing previous replies
(defmethod set-previous-reply ((node gossip-node) soluid srcuid reply)
  "Remember interim reply indexed by soluid and srcuid"
  ; Don't ever call this for replies to :ANONYMOUS expectations. See Note C.
  (let ((interim-table (kvs:lookup-key (repliers-expected node) soluid)))
    (unless interim-table
      (setf interim-table (kvs:make-store ':hashtable :test 'equal))
      (kvs:relate-unique! (repliers-expected node) soluid interim-table))
    (kvs:relate-unique! interim-table srcuid reply)))

(defmethod get-previous-reply ((node gossip-node) soluid srcuid)
  "Retrieve interim reply indexed by soluid and srcuid"
  ; Don't ever call this for replies to :ANONYMOUS expectations. See Note C.
  (let ((interim-table (kvs:lookup-key (repliers-expected node) soluid)))
    (when interim-table
      (kvs:lookup-key interim-table srcuid))))

(defmethod remove-previous-reply ((node gossip-node) soluid srcuid)
  "Remove interim reply (if any) indexed by soluid and srcuid from repliers-expected table.
  Return true if table is now empty; false if any other expected repliers remain.
  Second value is true if given soluid and srcuid was indeed present in repliers-expected."
  ; Don't ever call this for replies to :ANONYMOUS expectations. See Note C.
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
  ; Don't ever call this for replies to :ANONYMOUS expectations. See Note C.
  (let* ((previous-interim (get-previous-reply node soluid srcuid)))
    (when (or (null previous-interim)
              (later-reply? new-reply previous-interim))
      (set-previous-reply node soluid srcuid new-reply)
      t)))

(defgeneric coalescer (kind)
  (:documentation "Reduction function for a particular message kind."))

(defmethod coalescer ((kind (eql :COUNT-ALIVE)))
  "Proper coalescer for :count-alive responses."
  '+)

(defmethod coalescer ((kind (eql :LIST-ALIVE)))
  "Proper coalescer for :list-alive responses. Might
   want to add a call to remove-duplicates here if we start
   using a gossip protocol not guaranteed to ignore redundancies)."
  'append)

(defmethod coalescer ((kind (eql :LIST-ADDRESSES)))
  'append)

(defmethod coalescer ((kind (eql :GOSSIP-LOOKUP-KEY)))
  "Proper coalescer for :GOSSIP-LOOKUP-KEY responses."
  'multiple-tally)

(defmethod coalescer ((kind (eql :FIND-MAX)))
  (lambda (x y) (cond ((and (numberp x)
                            (numberp y))
                       (max x y))
                      ((numberp x)
                       x)
                      ((numberp y)
                       y)
                      (t nil))))

(defmethod coalescer ((kind (eql :FIND-MIN)))
  (lambda (x y) (cond ((and (numberp x)
                            (numberp y))
                       (min x y))
                      ((numberp x)
                       x)
                      ((numberp y)
                       y)
                      (t nil))))

(defmethod coalesce ((node gossip-node) kind soluid)
  "Grab all the data that interim repliers have sent me so far, combine it with my
  local reply-cache in a way that's specific to kind, and return result.
  This purely functional and does not change reply-cache.
  This is essentially a reduce operation but of course we can't use that name."
  (let* ((local-data    (kvs:lookup-key (reply-cache node) soluid))
         (interim-table (kvs:lookup-key (repliers-expected node) soluid))
         (interim-data (when (and interim-table
                                  (hash-table-p interim-table))
                         (loop for reply being each hash-value of interim-table
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
  If no more repliers, reply upstream with final reply.
  If more repliers, reply upstream either immediately or after a yield period with an interim-reply.
  Returns true if a [now-cancelled] reply had been expected from soluid/srcuid."
  (let ((interim-table (kvs:lookup-key (repliers-expected thisnode) soluid)))
    (cond ((eql :ANONYMOUS interim-table)
           nil) ; do nothing and return nil if we're accepting :ANONYMOUS replies
          (t (multiple-value-bind (no-more-repliers was-present?) (remove-previous-reply thisnode soluid srcuid)
               (cond (no-more-repliers
                      (let ((timeout-handler (kvs:lookup-key (timeout-handlers thisnode) soluid)))
                        (if timeout-handler ; will be nil for messages that don't expect a reply
                            (funcall timeout-handler nil)
                            (maybe-log thisnode :NO-TIMEOUT-HANDLER! soluid))))
                     (t ; more repliers are expected
                      ; functionally coalesce all known data and send it upstream as another interim reply
                      (if *delay-interim-replies*
                          (send-delayed-interim-reply thisnode reply-kind soluid)
                          (let ((upstream-source (get-upstream-source thisnode soluid)))
                            (send-interim-reply thisnode reply-kind soluid upstream-source)))))
               was-present?)))))

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

;;;; Network communications

; network messages are 3 pieces in a list:
;   destuid of ultimate receiving node
;   srcuid of sending node
;   gossip-msg

(defun make-ip-key (address port)
  (if (and (integerp address)
           (null port))
      address ; means port is already folded into real-address
      (+ (ash (usocket::host-to-hbo address) 16) port)))

(defmethod memoize-proxy ((proxy proxy-gossip-node) &optional subtable)
  "Memoize given proxy for quick lookup. Returns proxy itself."
  (with-slots (real-address real-port real-uid) proxy
    (let ((table (typecase proxy
                   (udp-gossip-node *udp-proxy-table*)
                   (tcp-gossip-node *tcp-proxy-table*))))
      (unless subtable
        (let ((key (make-ip-key real-address real-port)))
          (setf subtable (gethash key table))
          (unless subtable (setf subtable (setf (gethash key table) (make-hash-table))))))
      (setf (gethash real-uid subtable) proxy))))

(defun ensure-proxy-node (mode real-address real-port real-uid)
  "Real-address, real-port, and real-uid refer to the node on an OTHER machine that this node points to.
   Returns newly-created or old proxy-gossip-node.
   Found or newly-created proxy will be added to *nodes* database for this process."
  (check-type real-port integer) ; might want to support lookup of port-name to port-number eventually
  (let* ((key (make-ip-key real-address real-port))
         (table (case mode
                   (:udp *udp-proxy-table*)
                   (:tcp *tcp-proxy-table*)))
         (proxy-subtable (gethash key table))
         (proxy nil))
    (when proxy-subtable
           (setf proxy (gethash real-uid proxy-subtable)))
    (when proxy (kvs:relate-unique! *nodes* (uid proxy) proxy)) ; because we might have cleared *nodes* during testing
    (unless proxy
      ; make one and memoize it
      (setf proxy (make-proxy-node mode
                                   :real-address real-address
                                   :real-port real-port
                                   :real-uid real-uid
                                   :proxy-subtable proxy-subtable)))
    proxy))

(defmethod incoming-message-handler ((msg solicitation) srcuid destuid)
  "Locally dispatch messages received from network"
  ;; srcuid will be that of a proxy-gossip-node on receiver (this) machine
  ;; destuid will be that of a true gossip-node on receiver (this) machine
  (when (uid? (reply-to msg)) ; deal with direct replies properly
          (setf (reply-to msg) srcuid))
  (incf (hopcount msg)) ; no need to copy message here since we just created it from scratch
  (send-msg msg destuid srcuid))

(defmethod incoming-message-handler ((msg reply) srcuid destuid)
  "Locally dispatch messages received from network"
  ;; proxy will be a proxy-gossip-node on receiver (this) machine
  ;; destuid will be that of a true gossip-node on receiver (this) machine
  (incf (hopcount msg)) ; no need to copy message here since we just created it from scratch
  (send-msg msg destuid srcuid))

;; ------------------------------------------------------------------------------
;; Generic handling for expected authenticated messages. Check for
;; valid deserialization, check deserialization is a
;; pbc:signed-message, check for valid signature, then call user's
;; handler with embedded authenticated message. If any failure along
;; the way, just drop the message on the floor.

(defun do-process-authenticated-packet (deserialize-fn body-fn)
  "Handle decoding and authentication. If fails in either case just do nothing."
  (let ((decoded (ignore-errors
                   ;; might not be a valid serialization
                   (funcall deserialize-fn))))
    (when (and decoded
               (ignore-errors
                 ;; might not be a pbc:signed-message
                 (pbc:check-message decoded)))
      (funcall body-fn (pbc:signed-message-msg decoded)))))

(defmacro with-authenticated-packet ((packet-arg) deserializer &body body)
  "Macro to simplify decoding and authentication"
  `(do-process-authenticated-packet (lambda ()
                                      ,deserializer)
                                    (lambda (,packet-arg)
                                      ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-authenticated-packet" 2)

;; ------------------------------------------------------------------------------

(defun incoming-message-handler-udp (raw-message-buffer rem-address)
  "Deserialize a raw message string, srcuid, and destuid.
  srcuid and destuid will be that of a proxy-gossip-node and gossip-node on THIS machine,
  respectively."
  ;                                                v--- Port at which passive listener is running
  ;network message: (list (real-uid node) srcuid rem-port msg)
  ;                     uid  --^            ^-- source uid
  ;             on local machine            on destination machine
  (with-authenticated-packet (packet)
      (loenc:decode raw-message-buffer)
    (destructuring-bind (destuid srcuid rem-port msg) packet ;; note: if not what we were expecting, then this will signal error
      (when (debug-level 1)
        (debug-log :INCOMING-UDP msg :FROM rem-address rem-port :TO destuid))
      (let ((proxy (ensure-proxy-node :UDP rem-address rem-port srcuid)))
          ;ensure a local node of type proxy-gossip-node exists on this machine with
          ;  given rem-address, rem-port, and srcuid (the last of which will be the proxy node's real-uid that it points to).
        (incoming-message-handler msg (uid proxy) destuid) ; use uid of proxy here because destuid needs to see a source that's meaningful
          ;   on THIS machine.
        ))))

; TCP not done yet
(defun incoming-message-handler-tcp (stream)
  "Deserialize a raw message string, srcuid, and destuid.
  srcuid and destuid will be that of a proxy-gossip-node and gossip-node on THIS machine,
  respectively."
  ;                                                v--- Port at which passive listener is running
  ;network message: (list (real-uid node) srcuid rem-port msg)
  ;                    uid  --^            ^-- source uid
  ;             on local machine     on destination machine
  (with-authenticated-packet (packet)
      (loenc:deserialize stream)
    (let ((rem-address (usocket:get-peer-address stream)))
      (destructuring-bind (destuid srcuid rem-port msg) packet ;; if not what we expected, this will signal error
        (when (debug-level 1)
          (debug-log :INCOMING-TCP msg :FROM rem-address :TO destuid))
        (let ((proxy (ensure-proxy-node :TCP rem-address rem-port srcuid)))
        ;ensure a local node of type proxy-gossip-node exists on this machine with
        ;  given rem-address, rem-port, and srcuid (the last of which will be the proxy node's real-uid that it points to).
          (incoming-message-handler msg (uid proxy) destuid) ; use uid of proxy here because destuid needs to see a source that's meaningful
        ;   on THIS machine.
          )))))

(defmethod serve-gossip-port ((mode (eql :UDP)) socket)
  "UDP gossip port server loop"
    (let ((maxbuf (make-array cosi-simgen::*max-buffer-length*
                              :element-type '(unsigned-byte 8))))
      (unwind-protect
          (loop
            (when (eql :SHUTDOWN-SERVER *shutting-down*)
                (setf *shutting-down* nil)
                (return-from serve-gossip-port))
	    (multiple-value-bind (buf buf-len rem-ip)
		(usocket:socket-receive socket maxbuf (length maxbuf))
              (let* ((saf-buf (if (eq buf maxbuf)
                                  (subseq buf 0 buf-len)
                                buf))
                     (shutdown-msg? (equalp *shutdown-msg* (subseq buf 0 (shutdown-msg-len)))))
                (when shutdown-msg?
                  (setf *shutting-down* nil)
                  (return-from serve-gossip-port))
                (incoming-message-handler-udp saf-buf rem-ip))))
        (usocket:socket-shutdown socket ':IO)
        (usocket:socket-close socket))))

; TCP not done yet
(defmethod serve-gossip-port ((mode (eql :TCP)) listening-socket)
  "TCP gossip port server loop"
  (unwind-protect
      (loop
        (when (eql :SHUTDOWN-SERVER *shutting-down*)
          (setf *shutting-down* nil)
          (return-from serve-gossip-port))
        (let ((stream nil)
              (timed-out? nil))
          ; Following always returns T in CCL. Thus sockets are "born blocking"
          ;(format t "~%Blocking status of socket: ~S" (ccl::get-socket-fd-blocking (ccl::socket-device listening-socket)))
          (setf stream (ignore-errors (usocket:socket-accept listening-socket)))
          
          (when (debug-level 3)
            (debug-log "Got connection from " (usocket:get-peer-address stream)))
          
          ;; At this point, we have a live incoming stream. Although it may be closing.
          
          (setf stream (usocket:wait-for-input stream :timeout 10 :ready-only t))
          (setf timed-out? (not (eql :READ (usocket::state stream))))
          
          (when (debug-level 3)
            (if timed-out?
                (debug-log "Giving up.")
                (debug-log "Got data.")))
          ;; At this point, we have data available. Probably (may be closed or timed out).
          (cond ((or (eql :SHUTDOWN-SERVER *shutting-down*)
                     timed-out?)
                 (when stream (usocket:socket-close stream)))
                ;; Really should have data at this point...
                (t
                 (incoming-message-handler-tcp stream)))))
    (when listening-socket (usocket:socket-close listening-socket))))

(defun open-passive-udp-socket (port)
  "Open a passive udp socket. Returns socket if successful.
   Returns a keyword if some kind of error."
  (handler-case (usocket:socket-connect nil nil
                        :protocol :datagram
                        :local-host usocket:*wildcard-host* ; must be 0, not "localhost", in order to receive on all IP addresses of all interfaces
                        :local-port port)
  (USOCKET:ADDRESS-IN-USE-ERROR ()
                                :ADDRESS-IN-USE)))

(defun open-passive-tcp-socket (port)
  "Open a passive tcp socket. Returns socket if successful.
  Returns a keyword if some kind of error."
  (handler-case (usocket:socket-listen
                 usocket:*wildcard-host*
                 port
                 :element-type '(unsigned-byte 8)
                 :reuse-address t)
    (USOCKET:ADDRESS-IN-USE-ERROR ()
                                  :ADDRESS-IN-USE)))

(defmethod start-gossip-server ((mode (eql :udp)) &optional (port *nominal-gossip-port*) (try-count 0))
  "Starts local passive gossip server and return it and its local port."
  (when (< try-count *max-server-tries*)
    (unless (find-process *udp-gossip-server-name*)
      (setf *shutting-down* nil)
      (let* ((socket (open-passive-udp-socket port)))
        (cond ((usocket:datagram-usocket-p socket)
               (mpcompat:process-run-function *udp-gossip-server-name* nil
                 'serve-gossip-port mode socket)
               (values (setf *udp-gossip-socket* socket)
                       (setf *actual-udp-gossip-port* (usocket:get-local-port socket))))
              ((eql :ADDRESS-IN-USE socket)
               (if (find-process *udp-gossip-server-name*)
                   ; if process exists for serving UDP in THIS LISP IMAGE, we're done
                   nil
                   ; otherwise, assume process exists for serving UDP on some OTHER lisp image, which means we can retry
                   (start-gossip-server mode (1+ port) (1+ try-count)))))))))

(defmethod start-gossip-server ((mode (eql :TCP)) &optional (port *nominal-gossip-port*) (try-count 0))
  (when (< try-count *max-server-tries*)
    (unless (find-process *tcp-gossip-server-name*)
      (setf *shutting-down* nil)
      (let* ((socket (open-passive-tcp-socket port)))
        (cond ((usocket:stream-server-usocket-p socket)
               (mpcompat:process-run-function *tcp-gossip-server-name* nil
                 'serve-gossip-port mode socket)
               (values (setf *tcp-gossip-socket* socket)
                       (setf *actual-tcp-gossip-port* (usocket:get-local-port socket))))
              ((eql :ADDRESS-IN-USE socket)
               (if (find-process *tcp-gossip-server-name*)
                   ; if process exists for serving TCP in THIS LISP IMAGE, we're done
                   nil
                   ; otherwise, assume process exists for serving TCP on some OTHER lisp image, which means we can retry
                   (start-gossip-server mode (1+ port) (1+ try-count)))))))))

;;; This is different from cosi-simgen::internal-send-socket.
;;; Don't create a new socket, and don't close it when we're done.
;;; Rather, reuse the one we already have. This is critical to ensure the
;;;   other end sees the proper port to respond to.

(let ((sender (ac:make-actor
               (lambda (socket packet nb addr port)
                 (unless (eql nb (usocket:socket-send socket packet nb
                                                      :host addr :port port))
                   (ac::pr :socket-send-error addr packet))))))
  
  (defun internal-send-socket (addr port packet)
    (let ((nb (length packet)))
      (when (> nb cosi-simgen::*max-buffer-length*)
        (error "Packet too large for UDP transmission"))
      (let ((socket *udp-gossip-socket*))
        ;; (pr :sock-send (length packet) addr packet)
        (ac:send sender socket packet nb addr port)))))

(defmethod shutdown-gossip-server ((mode (eql T)) &optional port)
  (declare (ignore port))
  (shutdown-gossip-server :UDP)
  (shutdown-gossip-server :TCP))

(defmethod shutdown-gossip-server ((mode (eql :UDP)) &optional (port *nominal-gossip-port*))
  (setf *shutting-down* :SHUTDOWN-SERVER)
  (uiop:if-let (process (find-process *udp-gossip-server-name*))
    (progn
      (sleep .5)
      (internal-send-socket "127.0.0.1" port *shutdown-msg*)
      (sleep .5)
      (process-kill process)
      (when *udp-gossip-socket*
        (usocket:socket-close *udp-gossip-socket*))
      (setf *udp-gossip-socket* nil
            *actual-udp-gossip-port* nil))))

(defmethod shutdown-gossip-server ((mode (eql :TCP)) &optional (port *nominal-gossip-port*))
  (declare (ignore port)) ; probably don't need this
  (setf *shutting-down* :SHUTDOWN-SERVER)
  (uiop:if-let (process (find-process *tcp-gossip-server-name*))
    (progn
      (sleep .5)
      (process-kill process)
      (when *tcp-gossip-socket*
        (usocket:socket-close *tcp-gossip-socket*))
      (setf *tcp-gossip-socket* nil
            *actual-tcp-gossip-port* nil))))

(defmethod transmit-msg ((msg gossip-message-mixin) (node gossip-node) srcuid)
  (declare (ignore srcuid))
  (error "Bug: Cannot transmit to a local node!"))

;; ------------------------------------------------------------------------------
;; We need to authenticate all messages being sent over socket ports
;; from this running Lisp image. Give us some signing keys to do so...

(defvar *this-lisp-key*
  (pbc:make-key-pair (list :lisp-authority (uuid:make-v1-uuid))))

(defun sign-message (msg)
  "Sign and return an authenticated message packet. Packet includes
original message."
  (assert (pbc:check-public-key (pbc:keying-triple-pkey *this-lisp-key*)
                                (pbc:keying-triple-sig  *this-lisp-key*)))
  (pbc:sign-message msg
                    (pbc:keying-triple-pkey *this-lisp-key*)
                    (pbc:keying-triple-skey *this-lisp-key*)))

;; ------------------------------------------------------------------------------

(defmethod transmit-msg ((msg gossip-message-mixin) (node udp-gossip-node) srcuid)
  "Send message across network.
   srcuid is that of the (real) local gossip-node that sent message to this node."
  (cond (*udp-gossip-socket*
         (maybe-log node :TRANSMIT msg)
         (let* ((payload (sign-message (list (real-uid node) srcuid *actual-udp-gossip-port* msg)))
                (packet  (loenc:encode payload))
                (nb      (length packet)))
           (when (> nb cosi-simgen::*max-buffer-length*)
             (error "Packet too large for UDP transmission"))
           (internal-send-socket (real-address node) (real-port node) packet)
           ))
        (t
         (maybe-log node :CANNOT-TRANSMIT "no socket"))))

(defmethod transmit-msg ((msg gossip-message-mixin) (node tcp-gossip-node) srcuid)
  "Send message across network.
  srcuid is that of the (real) local gossip-node that sent message to this node."
  (let ((stream (ensure-open-stream (real-address node) (real-port node))))
    (cond ((streamp stream)
           (let ((payload (sign-message (list (real-uid node) srcuid *actual-tcp-gossip-port* msg))))
             (loenc:serialize payload stream)))
          (t
           (maybe-log node :CANNOT-TRANSMIT "no stream")))))

(defun ensure-open-stream (addr port)
  (when (debug-level 3)
            (debug-log "Opening stream to " (usocket::host-to-vector-quad addr) port))
  (usocket:socket-connect addr port :protocol ':stream :element-type '(unsigned-byte 8)))

#|
(defun ensure-open-stream (ip port)
  (let* ((integer-ip (usocket::host-to-hbo ip))
         (key (if integer-ip
                  (+ (ash integer-ip 16) port)))
         (stream nil))
    (mpcompat:with-lock *stream-lock*
      (setf stream (gethash key *open-streams*))
      (when stream
        (remhash key *open-streams*)))
    
      
  (handler-case (usocket:socket-connect ip port :protocol :stream
			 :element-type '(unsigned-byte 8))
    (USOCKET:CONNECTION-REFUSED-ERROR () :CONNECTION-REFUSED)))
|#

(defmethod deliver-gossip-msg (gossip-msg (node gossip-node) srcuid)
  (setf gossip-msg (copy-message gossip-msg)) ; must copy before incrementing hopcount because we can't
  ;  modify the original without affecting other threads.
  (incf (hopcount gossip-msg))
  ; Remember the srcuid that sent me this message, because that's where reply will be forwarded to
  (locally-receive-msg gossip-msg node srcuid))

(defmethod deliver-gossip-msg (gossip-msg (node proxy-gossip-node) srcuid)
  "This node is a standin for a remote node. Transmit message across network."
   (transmit-msg gossip-msg node srcuid))

(defun archive-log ()
  "Archive existing *log* and start a new one.
   This function is not thread-safe, so don't call it if any actors might be using the log."
  (vector-push-extend *log* *archived-logs*)
  (setf *log* (new-log)))

(defun run-gossip-sim (&optional (protocol :TCP))
  "Archive the current log and clear it.
   Prepare all nodes for new simulation.
   Only necessary to call this once, or
   again if you change the graph or want to start with a clean log."
  (shutdown-gossip-server t)
  (archive-log)
  (sleep .5)
  (start-gossip-server protocol)
  (maphash (lambda (uid node)
             (declare (ignore uid))
             (setf (car (ac::actor-busy (actor node))) nil)
             (clear-caches node))
           *nodes*)
  (hash-table-count *nodes*))

(defun measure-timing (logcmd &optional (log *log*))
  "Returns maximum time elapsed between first log entry [of any type] and last of type logcmd.
   It's a good idea to call #'archive-log before using this to ensure you start with a fresh log.
   Assumes first element of every log entry is a timestamp with microsecond resolution."
  (let ((lastpos (position logcmd log :from-end t :key 'second)))
    (when lastpos
      (/ (- (first (elt log lastpos))
            (first (elt log 0)))
         1e6))))
; Ex:
; (archive-log)
; (solicit-wait <something>)
; (measure-timing :finalreply)

; (make-graph 10)
; (save-graph-to-file "tests/10nodes2.lisp")
; (restore-graph-from-file "tests/10nodes2.lisp") ;;; best test network
; (make-graph 5)
; (save-graph-to-file "~/gossip/5nodes.lisp")
; (restore-graph-from-file "~/gossip/5nodes.lisp")

; (make-graph 100)
; (make-graph 1000)
; (make-graph 10000)
; (visualize-nodes *nodes*)  ; probably not a great idea for >100 nodes

; PARTIAL GOSSIP TESTS
; (make-graph 10)
; (set-protocol-style :gossip 2)
; (get-protocol-style)
; (run-gossip-sim)
; (solicit-wait (random-node) :count-alive)
; (solicit-wait (random-node) :list-alive)
; (visualize-nodes *nodes*)
; (inspect *log*)

#+CCL ; because this method doesn't exist in CCL, and the default doesn't work for udpsockets.
(defmethod ccl::local-socket-address ((socket ccl::udp-socket))
  (getf (ccl::socket-keys socket) :local-address))

(defun other-udp-port ()
  (when *udp-gossip-socket*
    (if (= 65002 *actual-udp-gossip-port*)
        65003
        65002)))

(defun other-tcp-port ()
  (when *tcp-gossip-socket*
    (if (= 65002 *actual-tcp-gossip-port*)
        65003
        65002)))

    
; UDP TESTS
; Test 1
; ON SERVER MACHINE
; (clrhash *nodes*)
#+TEST1
(setf localnode (make-node
  :UID 200
  :NEIGHBORS nil))
; (run-gossip-sim :UDP)

; ON CLIENT MACHINE
; (clrhash *nodes*)
; (run-gossip-sim :UDP)
#+TEST-LOCALHOST
(setf rnode (ensure-proxy-node :UDP "localhost" (other-udp-port) 200)) ; assumes there's a node numbered 200 on another Lisp process at 65003
#+TEST-AMAZON
(setf rnode (ensure-proxy-node :UDP "ec2-35-157-133-208.eu-central-1.compute.amazonaws.com" *nominal-gossip-port* 200))


#+TEST1 ; create 'real' local node to call solicit-wait on because otherwise system will try to forward the
       ;   continuation that solicit-wait makes across the network
(setf localnode (make-node
  :UID 201
  :NEIGHBORS (list (uid rnode))))
; (solicit-wait localnode :count-alive)
; (solicit-wait localnode :list-alive)
; (solicit-wait localnode :list-addresses)
; (solicit localnode :gossip-relate-unique :foo :bar)
; (solicit-wait localnode :gossip-lookup-key :foo)

; Test 2: List live nodes at given address
; ; ON SERVER MACHINE
; (clrhash *nodes*)
; (make-graph 10)
; (run-gossip-sim :UDP)

; ON CLIENT MACHINE
; (clrhash *nodes*)
; (run-gossip-sim :UDP)
; (set-protocol-style :neighborcast)
#+TEST-LOCALHOST
(setf rnode (ensure-proxy-node :UDP "localhost" (other-udp-port) 0)) ; assumes there's a node numbered 200 on another Lisp process at 65003
#+TEST-AMAZON
(setf rnode (ensure-proxy-node :UDP "ec2-35-157-133-208.eu-central-1.compute.amazonaws.com" *nominal-gossip-port* 0))

#+TEST2 ; create 'real' local node to call solicit-wait on because otherwise system will try to forward the
       ;   continuation that solicit-wait makes across the network
(setf localnode (make-node
  :UID 201
  :NEIGHBORS (list (uid rnode))))
; (solicit-direct localnode :count-alive)
; (solicit-direct localnode :list-alive)
; (inspect *log*)


; Test 3: List live nodes at given address via TCP
; ; ON SERVER MACHINE
; (clrhash *nodes*)
; (make-graph 10)
; (run-gossip-sim :TCP)

; ON CLIENT MACHINE
; (clrhash *nodes*)
; (run-gossip-sim :TCP)
; (archive-log)
; (set-protocol-style :neighborcast)
#+TEST-LOCALHOST
(setf rnode (ensure-proxy-node :TCP "localhost" (other-tcp-port) 0)) ; assumes there's a node numbered 200 on another Lisp process at 65003
#+TEST-AMAZON
(setf rnode (ensure-proxy-node :TCP "ec2-35-157-133-208.eu-central-1.compute.amazonaws.com" *nominal-gossip-port* 0))

#+TEST2 ; create 'real' local node to call solicit-wait on because otherwise system will try to forward the
       ;   continuation that solicit-wait makes across the network
(setf localnode (make-node
  :UID 201
  :NEIGHBORS (list (uid rnode))))
; (solicit-direct localnode :count-alive)
; (solicit-direct localnode :list-alive)
; (inspect *log*)



; (run-gossip-sim)
; (solicit-wait (first (listify-nodes)) :list-alive)
; (solicit-direct (first (listify-nodes)) :list-alive)
; (solicit-direct (first (listify-nodes)) :count-alive)
; (solicit-progress (first (listify-nodes)) :list-alive)
; (solicit-wait (first (listify-nodes)) :count-alive)
; (solicit (first (listify-nodes)) :announce :foo)
; (solicit-wait 340 :list-alive) ; if there happens to be a node with UID 340
; (inspect *log*) --> Should see :FINALREPLY with all nodes (or something slightly less, depending on network delays, etc.) 

; (solicit (first (listify-nodes)) :gossip-relate-unique :foo :bar)
; (solicit-wait (first (listify-nodes)) :gossip-lookup-key :foo)
; (solicit (first (listify-nodes)) :gossip-relate :foo :baz)
; (solicit-wait (first (listify-nodes)) :gossip-lookup-key :foo)

; (set-protocol-style :gossip) ; ensure every node will likely not get the message
; (solicit (first (listify-nodes)) :gossip-relate-unique :foo 2)
; (solicit (second (listify-nodes)) :gossip-relate-unique :foo 17)
; (solicit (third (listify-nodes)) :gossip-relate-unique :foo 5)
; (set-protocol-style :neighborcast)
; (solicit-wait (first (listify-nodes)) :find-max :foo)
; (solicit-wait (first (listify-nodes)) :find-min :foo)

;; should produce something like (:FINALREPLY "node209" "sol255" (((:BAZ :BAR) . 4))) as last *log* entry

(defun get-kvs (key)
  "Shows value of key for all nodes. Just for debugging. :gossip-lookup-key is the proper way to do this."
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

#+TESTING
(setf node (make-node
  :UID 253
  :ADDRESS 'NIL
  :NEIGHBORS '(248 250 251)
  :LOGFN 'GOSSIP::DEFAULT-LOGGING-FUNCTION
  :KVS (as-hash-table 'eql '((key1 . 1) (key2 . 2) (key3 . 3)))))

#+TESTING
(save-node node *standard-output*)

#+TESTING
(setf msg (make-solicitation :kind :list-alive))
#+TESTING
(copy-message msg)

#| NOTES

NOTE A: About proactive reply cancellation
If node X is ignoring a solicitation* from node Y because it already
saw that solicitation, then it must also not expect a reply from node Y
for that same solicitation.
Here's why [In this scenario, imagine #'locally-receive-msg is acting on behalf of node X]:
If node X ignores a solicition from node Y -- because it's already seen that solicitation --
  then X knows the following:
  1. Node Y did not receive the solicition from X. It must have received it from somewhere else,
     because nodes *never* forward messages to their upstream.
  2. Therefore, if X forwards (or forwarded) the solicitation to Y, Y
     is definitely going to ignore it. Because Y has already seen it.
  3. Therefore (to recap) if X just ignored a solicitation from Y, then X
     knows Y is going to ignore that same solicitation from X.
  4. THEREFORE: X must not expect Y to respond, and that's why
     we call cancel-replier here. If we don't do this, Y will ignore
     and X will eventually time out, which it doesn't need to do.
  5. FURTHERMORE: Y knows X knows this. So it knows X will stop waiting for it.
* we take no special action for non-solicitation messages because they can't
  ever be replied to anyway.

NOTE B: About the repliers-expected slot.
This slot always contains a hashtable mapping a soluid to a value.
The value can be:
--Another hashtable, which maps srcuid to a subvalue. If subvalue is nil, it means this node
  expects a reply from srcuid for the soluid that represents this table. If the subvalue is non-nil,
  it can only be an interim-reply that srcuid has already sent. Future interim-replies from that srcuid
  will supersede older ones. (They will not be coalesced with older interim-replies; they replace them.)
  Subvalue will never be a final-reply because those don't get stored--they get coalesced with the value
  in reply-cache and stored in the reply-cache.
--The keyword :ANONYMOUS which indicates that replies from any srcuid are accepted. We don't expect to
  receive interim-replies from :ANONYMOUS repliers, so if we do receive any, they simply get dropped on
  the floor rather than stored. See Note C for more info.
--The value NIL, which indicates that no replies (or no further replies) are expected for this soluid.


NOTE C: About :ANONYMOUS expectations.
We have a mechanism to allow replies to a node to occur from unpredictable sources. This can happen
when we send a message with forward-to :NEIGHBORCAST or :GOSSIP but whose reply-to slot is a single UID.
In this case, there's no way to know who might reply in advance, so we have to allow replies (to this soluid)
from any srcuid. So in that case, we do
(kvs:relate-unique! (repliers-expected thisnode) soluid :ANONYMOUS)
to prepare for replies.
There are several implications here:
1. We cannot know in advance when we have received all possible replies. Thus we have to depend on a timeout
as the only mechanism to know when to ignore further replies (and to reply ourselves, if necessary).
2. We don't allow interim-replies to :ANONYMOUS expectations, because we're not creating a table to keep them
(because in place of that table, we just have the keyword :ANONYMOUS), and because I don't think we'll ever need
interim-replies in this case.
3. We still allow coalescence at the node expecting :ANONYMOUS replies; they just always come from final-replies.
4. There's a possible hazard condition here: If a single node sends us two final-replies with the same soluid,
we won't know to reject the second one, and we'll coalesce a value from that node twice, which would cause
an incorrect coalescence result. We may have to care about this in future, but I'm not going to worry about
it now. In working code, there's no code path that would cause a node to respond with a final-reply for a given
soluid twice.

|#