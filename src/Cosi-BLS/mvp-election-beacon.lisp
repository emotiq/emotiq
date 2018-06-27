
(in-package :cosi-simgen)

;; ---------------------------------------------------------------------------
;; list of (pubkey stake) associations for all witness nodes

(defvar *all-nodes*  nil) 

(defun set-nodes (node-list)
  "NODE-LIST is a list of (public-key stake-amount) pairs"
  (ac:pr (format nil "election set-nodes ~A" node-list))
  (assert node-list)
  (setf *all-nodes* node-list))

;; ---------------------------------------------------------------------------
;; Election Beacon - every node runs an election beacon.

(defvar *mvp-election-period*  20) ;; seconds between election rounds

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

(defun hold-election (nfrac)
  "Given a fraction (0 < nfrac < 1) arrange all stakeholders into a
binary decision tree, and determine the node which wins the election,
based on their relative stake"
  (let* ((tree    (car (um:nlet-tail iter ((nodes *all-nodes*))  ;; tail recursive, scheme-like, named let
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

(defvar *election-seed* nil)

(defun init-election-seed ()
  (setf *election-seed* (lw:make-mt-random-state (round (get-universal-time) 3600))))

(defun get-election-seed ()
  (lw:mt-random 1.0 *election-seed*))

;; --------------------------------------------------------------------------
;; Augment message handlers for election process

(defmethod node-dispatcher ((msg-sym (eql :hold-an-election)) &key n)
  (cond (*holdoff*
         (emotiq:note "Election held-off"))
        (t
         (set-holdoff)
         (let* ((node   (current-node))
                (me     (node-pkey node))
                (winner (hold-election n)))
           (setf (node-current-leader node) winner)
           (emotiq:note "~A got :hold-an-election ~A" (short-id node) n)
           (let ((iwon (int= winner me)))
             (emotiq:note "election results ~A (stake = ~A)"
                          (if iwon " *ME* " " not me ")
                          (node-stake node))
             (emotiq:note "winner ~A me=~A"
                     (short-id winner)
                     (short-id me))
             (if iwon
                 (progn
                   (cosi-simgen:send me :become-leader)
                   (cosi-simgen:send me :make-block))
               (cosi-simgen:send me :become-witness)))
           ))
        ))
