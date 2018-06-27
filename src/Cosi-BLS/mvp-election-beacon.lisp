
(in-package :cosi-simgen)

;; ---------------------------------------------------------------------------
;; list of (pubkey stake) associations for all witness nodes

(defvar *all-nodes*  nil) 

(defun set-nodes (node-list)
  "NODE-LIST is a list of (public-key stake-amount) pairs"
  (ac:pr (format nil "election set-nodes ~A" node-list))
  (assert node-list)
  (setf *all-nodes* node-list))

(defun get-witness-list ()
  *all-nodes*)

;; ---------------------------------------------------------------------------
;; Election Beacon - every node runs an election beacon.

(defvar *mvp-election-period*  20) ;; seconds between election rounds

#|
(defvar *mvp-election-beacon*
  (ac:make-actor
   (let ((prng  nil))
     (lambda (&rest msg)
       (labels ((hold-election ()
                  (let* ((election-result (lw:mt-random 1.0 prng))
                         (msg `(:hold-an-election :n ,election-result)))
                    ;; make sure our own Node gets the message too
                    (gossip:singlecast msg
                                       :graphID nil) ;; force send to ourselves
                    ;; this really should go to everyone
                    (gossip:broadcast msg
                                      :graphID :UBER))))
         (um:dcase msg

           (:kill ()
            (ac:pr "Election beacon wasn't running when kill received"))

           (:start ()
            ;; We start by seeding Mersenne Twister PRNG with
            ;; universal time rounded to nearest hour.
            (setf prng (lw:make-mt-random-state (round (get-universal-time) 3600)))
            (ac:recv

              ((list :kill)
               (ac:pr "Election beacon killed"))

              ((list :start)
               (ac:pr "Election beacon already running. Start message ignored.")
               (ac:retry-recv))

              ((list :hold-election)
               (hold-election)
               (ac:retry-recv))
              
              :TIMEOUT *mvp-election-period*
              :ON-TIMEOUT (progn
                            (hold-election)
                            (ac:retry-recv))
              ))

           (:hold-election ()
            (hold-election))
           )))
     )))

(defun fire-election ()
  (ac:send *mvp-election-beacon* :hold-election))

(defun start-mvp-election-beacon ()
  (ac:send *mvp-election-beacon* :start))

(defun kill-election-beacon ()
  (ac:send *mvp-election-beacon* :kill))
|#

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
  nil)

(defmethod node-stake ((node tree-node))
  (tree-node-sum node))

(defmethod node-stake ((node cons))
  (second node))

;; a tree of stakes, cosi-simgen:nodes at the leaves (very bottom of tree)

(defun make-tree-node (pair)
  (destructuring-bind (l &optional r) pair
    (if r
        (make-instance 'tree-node
                       :l  l
                       :r  r
                       :sum (+ (node-stake l)
                               (node-stake r)))
      ;; else
      l)))

(defun hold-election (nfrac &optional (nodes (get-witness-list)))
  "Given a fraction (0 < nfrac < 1) arrange all stakeholders into a
binary decision tree, and determine the node which wins the election,
based on their relative stake"
  (let* ((tree    (car (um:nlet-tail iter ((nodes nodes)) ;; tail recursive, scheme-like, named let
                    (if (= 1 (length nodes))
                        nodes
                      (iter (mapcar 'make-tree-node (um:group nodes 2))))))))
    (when tree
      (um:nlet-tail iter ((tree  tree))                       ;; tail recursive, scheme-like, named let
        (if (tree-node-p tree)
            (let* ((l      (tree-node-l tree))
                   (r      (tree-node-r tree))
                   (lstake  (node-stake l))
                   (tstake  (node-stake tree)))
              (if (< (/ lstake tstake) nfrac)
                    (iter r)
                  (iter l)))
          ;; else - we have arrived at a winner
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
          (setf seed (hash/256 seed))
          (let ((ans (float (/ (int seed) #.(ash 1 256)) 1d0)))
            (when reply-to
              (send reply-to ans))
            ans))
         
         )))))

;; -----------------------------------------
;; REPL interface...

(defun get-election-seed ()
  (ac:ask *election-central* :next))

(defun reset-and-get-election-seed ()
  (ac:ask *election-central :next-after-reset))

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

;; ---------------------------------------------

(defun broadcast+me (msg)
  ;; make sure our own Node gets the message too
  (gossip:singlecast msg
                     :graphID nil) ;; force send to ourselves
  ;; this really should go to everyone
  (gossip:broadcast msg
                    :graphID :UBER))
  
(defun make-election-message (pkey seed)
  `(:hold-an-election
       :n      ,seed
       :beacon ,pkey
       :sig))

(defun send-hold-election-from-node (node)
  (=bind (seed)
      (send *election-central* :next (lambda (val)
                                       (=values val)))
    (with-accessors ((pkey  node-pkey)
                     (skey  node-skey)) node
      (let* ((msg        (make-election-message pkey seed))
             (signed-msg (um:conc1 msg (pbc:sign-hash (hash/256 msg) skey))))
        (broadcast+me signed-msg)))
    ))
  
;; --------------------------------------------------------------------------
;; Augment message handlers for election process

(defmethod node-dispatcher ((msg-sym (eql :hold-an-election)) &key n beacon sig)
  (if *holdoff*
      (emotiq:note "Election held-off")
    ;; else
    (with-accessors ((current-beacon  node-current-beacon)) (current-node)
      (let ((chk-msg (make-election-message beacon n)))
        
        (when (and (pbc:check-hash (hash/256 chk-msg) sig beacon) ;; not a forged call?
                   (or (and (null current-beacon)             ;; null at start
                            (member beacon (get-witness-list) ;;   then must be member of known witness pool
                                    :key  'first
                                    :test 'int=))
                       (int= beacon current-beacon)))         ;; otherwise, from beacon as we know it?
          
          (run-election n))))
    ))

(defun run-special-election ()
  ;; try to resync on stalled system
  (let ((node (current-node)))
    (=bind (seed)
        (send *election-central* :next-after-reset (lambda (val)
                                                     (=values val)))
      (with-current-node node
        (run-election seed)))))
   
(defun run-election (n)
  ;; use the election value n (0 < n < 1) to decide on staked winner
  ;; to become new leader node. Second runner up becomes responsible
  ;; for spawning an election beacon (one-shot).
  ;;
  ;; So election beacon becomes a roving responsibility. If it gets
  ;; knocked out, then we fall back to early elections with BFT
  ;; consensus on decision to resync.
  
  (let ((node      (current-node))
        (witnesses (get-witness-list)))
    
    (with-accessors ((stake node-stake)
                     (pkey  node-pkey)) node
      
      (let* ((winner     (hold-election n))
             (new-beacon (hold-election n (remove winner witnesses
                                                  :key 'first
                                                  :test 'int=))))
        (setf *leader*          winner
              *beacon*          new-beacon
              *election-calls*  0)
        
        (emotiq:note "~A got :hold-an-election ~A" (short-id node) n)
        (emotiq:note "election results ~A (stake = ~A)"
                     (if (int= pkey winner) " *ME* " " not me ")
                     stake)
        (emotiq:note "winner ~A me=~A"
                     (short-id winner)
                     (short-id pkey))

        (set-holdoff)
        (when (int= pkey new-beacon)
          (ac:spawn (lambda ()
                      (recv
                        :TIMEOUT    *mvp-election-period*
                        :ON-TIMEOUT (send-hold-election-from-node node)))))

        (ac:self-call (if (int= pkey winner)
                          :become-leader
                        :become-witness))
        ))))


