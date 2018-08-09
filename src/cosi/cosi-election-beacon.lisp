(in-package :emotiq/cosi)


(defmethod node-dispatcher ((msg-sym (eql :hold-an-election)) &key n beacon sig)
  (when (validate-election-message n beacon sig)
    (run-election n)))


(defmethod node-dispatcher ((msg-sym (eql :call-for-new-election)) &key pkey epoch sig)
  (when (and (validate-call-for-election-message pkey epoch sig) ;; valid call-for-election?
             (or (pr "Got call for new election") t)
             (>= (length *election-calls*)
                 (bft-threshold (get-witness-list))))
    (run-special-election)))


(defvar *all-nodes* nil
  "list of (pubkey stake) associations for all witness nodes")


(defun set-nodes (node-list)
  "NODE-LIST is a list of (public-key stake-amount) pairs"
  (ac:pr (format nil "election set-nodes ~A" node-list))
  (assert node-list)
  (setf *all-nodes* (mapcar (lambda (pair)
                              (destructuring-bind (pkey stake) pair
                                (list (pbc:public-key pkey)
                                      stake)))
                            node-list)))


(defun get-witness-list ()
  (if *use-real-gossip-p*
      *all-nodes*
      (or *all-nodes* ;; other sim
          (setf *all-nodes*
                (um:accum acc
                  (maphash (lambda (k node)
                             (declare (ignore k))
                             (acc (list (node:pkey node)
                                        (node:stake node))))
                           *ip->node*))))))


(defun get-witnesses-sans-pkey (pkey)
  (remove pkey (get-witness-list)
          :key  'first
          :test 'vec-repr:int=))


(defun witness-p (pkey)
  (find pkey (get-witness-list)
        :key  'first
        :test 'vec-repr:int=))

;; ---------------------------------------------------------------------------------------
;; Stake-weighted elections

(defclass tree-node ()
  ((l   :reader tree-node-l
        :initarg :l
        :initform nil)
   (r   :reader tree-node-r
        :initarg :r
        :initform nil)
   (sum :reader tree-node-sum
        :initarg :sum))
  (:documentation "Election tree node"))

(defmethod tree-node-p ((x tree-node))
  t)

(defmethod tree-node-p (x)
  (declare (ignore x))
  nil)


(defmethod node-stake ((node tree-node))
  (tree-node-sum node))

(defmethod node-stake ((node cons))
  (second node))


(defun make-tree-node (pair)
  "creates a tree of stakes, nodes at the leaves (very bottom of tree)"
  (destructuring-bind (l &optional r)
      pair
    (if r
        (make-instance 'tree-node
                       :l l
                       :r r
                       :sum (+ (node-stake l) (node-stake r)))
        l)))

(defun hold-election (nfrac &optional (nodes (get-witness-list)))
  "Given a fraction (0 < nfrac < 1) arrange all stakeholders into a
binary decision tree, and determine the node which wins the election,
based on their relative stake"
  (let* ((tree (car (um:nlet-tail iter ((nodes nodes))
                                  (if (= 1 (length nodes))
                                      nodes
                                      (iter (mapcar #'make-tree-node
                                                    (um:group nodes 2))))))))
    (when tree
      (um:nlet-tail iter ((tree tree))
                    (if (tree-node-p tree)
                        (let* ((l (tree-node-l tree))
                               (r (tree-node-r tree))
                               (lstake (node-stake l))
                               (tstake (node-stake tree)))
                          (if (< (/ lstake tstake) nfrac)
                              (iter r)
                              (iter l)))
                        (first tree)))))) ;; return pkey of winner

;; --------------------------------------------------------------------------

(defvar *election-central*
  ;; an actor to prevent SMP multiple mutation of seed. Could also use
  ;; a monitor.
  (ac:make-actor
   (let ((seed  nil)) ;; actually okay if never reset
     (lambda (&rest msg)
       (um:dcase msg

         (:next-after-reset (&optional reply-to)
          ;; use a coarse (1 hour) value that we would all likely
          ;; agree upon
          (setf seed (round (get-universal-time) 3600))
          (ac:self-call :next reply-to))
         
         (:next (&optional reply-to)
          (setf seed (hash:hash/256 seed))
          (let ((ans (float (/ (vec-repr:int seed) #.(ash 1 256)) 1d0)))
            (when reply-to
              (send reply-to ans))
            ans))

         (:update-seed (pkey)
          ;; perturb our seed so we don't mindlessly repeat the same
          ;; seed sequence as some other node after a major reset
          (setf seed (hash:hash/256 (uuid:make-v1-uuid) ;; varies with machine and time
                                    pkey                ;; varies with node
                                    seed)))             ;; based on last seed we generated

         )))))

(defmacro with-election-reply (reply-args msg &body body)
  ;; define convenient continuation interface for actors calling on
  ;; election services for some result
  `(=node-bind ,reply-args
       (send *election-central* ,msg (lambda (val)
                                           (=values val)))
     ,@body))

(defun update-election-seed (pkey)
  (send *election-central* :update-seed pkey))

;; -----------------------------------------
;; REPL interface...

(defun get-election-seed ()
  (ac:ask *election-central* :next))

(defun reset-and-get-election-seed ()
  (ac:ask *election-central* :next-after-reset))

(defun fire-election () ;; for REPL playing...
  (with-current-node *my-node*
    (send-hold-election)))

#|
(progn
  (setf *election-seed* nil)
  (let* ((arr (loop repeat 10000 collect (get-election-seed))))
    (plt:histogram 'histo arr :clear t)))

(=bind (seed)
    (send *election-central* :next (lambda (val)
                                     (=values val)))
  (pr seed))
 |#

;; ---------------------------------------------------------------------------

(defun make-election-message-skeleton (pkey seed)
  `(:hold-an-election
       :n      ,seed
       :beacon ,pkey
       :sig))

(defun make-signed-election-message (pkey seed skey)
  (let ((skel (make-election-message-skeleton pkey seed)))
    (um:conc1 skel (pbc:sign-hash (hash:hash/256 skel) skey))))

(defun validate-election-message (n beacon sig)
  (and (or (and (null *beacon*)      ;; it would be nil at startup
                (witness-p beacon))  ;;   from someone we know?
           (vec-repr:int= beacon *beacon*))   ;; or from the beacon we know?
       (let ((skel (make-election-message-skeleton beacon n)))
         (pbc:check-hash (hash:hash/256 skel) sig beacon)))) ;; not a forgery

(defun send-hold-election ()
  (with-election-reply (seed) :next
    (with-accessors ((pkey  node:pkey)
                     (skey  node:skey)) (current-node)
      (broadcast+me (make-signed-election-message pkey seed skey)))
    ))

;; --------------------------------------------------------------------------
;; Augment message handlers for election process

(defun run-special-election ()
  ;; try to resync on stalled system
  (with-election-reply (seed) :next-after-reset
    (run-election seed)))
   
(defvar *mvp-election-period*  20) ;; seconds between election rounds

(defun run-election (n)
  ;; use the election value n (0 < n < 1) to decide on staked winner
  ;; to become new leader node. Second runner up becomes responsible
  ;; for spawning an election beacon (one-shot).
  ;;
  ;; So election beacon becomes a roving responsibility. If it gets
  ;; knocked out, then we fall back to early elections with BFT
  ;; consensus on decision to resync.
  (let ((self      (current-actor))
        (node      (current-node)))
    
    (with-accessors ((stake       node:stake) ;; only used for diagnostic messages
                     (pkey        node:pkey)
                     (local-epoch node:local-epoch)) node

      (update-election-seed pkey)

      (let* ((winner     (hold-election n))
             (new-beacon (hold-election n (get-witnesses-sans-pkey winner))))

        (setf *leader*           winner
              *beacon*           new-beacon
              *local-epoch*      n  ;; unlikely to repeat from election to election
              *election-calls*   nil) ;; reset list of callers for new election
        
        (pr "~A got :hold-an-election ~A" (node:short-id node) n)
        (pr "election results ~A (stake = ~A)"
                     (if (vec-repr:int= pkey winner) " *ME* " " not me ")
                     stake)
        (pr "winner ~A me=~A"
                     (node:short-id winner)
                     (node:short-id pkey))

        (when (vec-repr:int= pkey new-beacon)
          ;; launch a beacon
          (node-schedule-after *mvp-election-period*
            (send-hold-election)))

        (cond ((vec-repr:int= pkey winner)
               ;; why not use ac:self-call?  A: Because doing it with
               ;; send, instead, allows queued up messages to be
               ;; handled first. Might be some transactions waiting to
               ;; enter the pool.
               (send self :become-leader)
               (send self :make-block))

              (t
               ;; ditto
               (send self :become-witness))

              )))))

;; ---------------------------------------------------------------
;; if we don't hold a new election before this timeout, established at
;; the end of commit phase, then call for a new election
(defvar *emergency-timeout*  20)

(defun setup-emergency-call-for-new-election ()
  ;; give the election beacon a chance to do its thing
  (let* ((node      (current-node))
         (old-epoch *local-epoch*))
    (node-schedule-after *emergency-timeout*
      ;; first time processing at startup
      ;; go gather keys and stakes.
      ;; *local-epoch* will also not have
      ;; changed
      (unless (get-witness-list)
        (cond (*use-real-gossip-p*
               (let* ((nodes/stakes (emotiq/config:get-stakes))
                      (my-pair      (find (node:pkey node) nodes/stakes
                                          :test 'vec-repr:int=
                                          :key  'first)))
                 (setf (node:stake node) (cadr my-pair))
                 (set-nodes nodes/stakes)))

              (t
               (unless (node:blockchain node)
                 (error "There is no blockchain. Cannot continue."))
               (let* ((genesis-block (first (node:blockchain node)))
                      (witnesses-and-stakes
                       (block:witnesses-and-stakes genesis-block))
                      (node-stake
                       (second (assoc (node:pkey node) witnesses-and-stakes
                                      :test 'vec-repr:int=))))
                 ;;; FIXME: this probably won't work for unstaked nodes.
                 ;;; They shouldn't be participating in elections, but as I
                 ;;; understand it, everyone is currently see this call.
                 (when (null node-stake)
                   (error "Stake is nil for this node. Cannot continue."))
                 (unless (txn:in-legal-stake-range-p node-stake)
                   (error "Stake value ~s is not valid for a stake." node-stake))
                 
                 (set-nodes witnesses-and-stakes)
                 (setf (node:stake node) node-stake)))
              ))
      
      (when (= *local-epoch* old-epoch) ;; anything changed?
        (call-for-new-election)))
    ))

;; ----------------------------------------------------------------
;; Startup Init Stuff...

(defun startup-elections ()
  ;; call this from global init after all housekeeping
  (with-current-node *my-node*
    (setup-emergency-call-for-new-election)))

(defmethod gossip:make-node ((kind (eql :cosi)) &key pkey skey)
  (setf *my-node* (make-instance 'node
                                 :pkey  (pbc:public-key pkey)
                                 :skey  (pbc:secret-key skey)))
  (gossip:initialize-node *my-node*
                          :pkey pkey
                          :skey skey)
  *my-node*)

(defun gossip:cosi-loaded-p ()
  t)

;; -----------------------------------------------------------

(Defun make-call-for-election-message-skeleton (pkey epoch)
  `(:call-for-new-election
    :pkey  ,pkey
    :epoch ,epoch
    :sig))

(defun make-signed-call-for-election-message (pkey epoch skey)
  (let ((skel  (make-call-for-election-message-skeleton pkey epoch)))
    (um:append1 skel (pbc:sign-hash (hash:hash/256 skel) skey))))

(defun validate-call-for-election-message (pkey epoch sig)
  (and (= epoch *local-epoch*)        ;; talking about current epoch? not late arrival?
       (witness-p pkey)               ;; from someone we know?
       (not (find pkey *election-calls*  ;; not a repeat call
                  :test 'vec-repr:int=))
       (let ((skel (make-call-for-election-message-skeleton pkey epoch)))
         (pbc:check-hash (hash:hash/256 skel) sig pkey))  ;; not a forgery
       (push pkey *election-calls*)))
  
(defun call-for-new-election ()
  (with-accessors ((pkey node:pkey)
                   (skey node:skey)) (current-node)
    (unless (find pkey *election-calls* ;; prevent repeated calls
                  :test 'vec-repr:int=)
      (push pkey *election-calls*) ;; need this or we'll fail with only 3 nodes...
      (gossip:broadcast (make-signed-call-for-election-message pkey *local-epoch* skey)
                        :graphID :UBER))))


