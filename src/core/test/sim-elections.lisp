(in-package :blockchain-test)


(defvar *beacon-interval*  20)

;; list of lists of a public key and its associated stake
;; 
;; in real life, this should be an ordered list/vector of *real-nodes*
;; in the simulator, 1 machine, this is a list of all nodes (fake and real)
;; In the simulator, we only hold mock elections, and print the
;; results, since only real nodes can be leaders and in the simulator there
;; is just one real node, which must always be the leader
;; (defvar *all-nodes*  nil) 

(defun set-nodes (node-list)
  "NODE-LIST is a list of (public-key stake-amount) pairs"
  (ac:pr (format nil "election set-nodes ~A" node-list))
  (assert node-list)
  (setf cosi-simgen::*all-nodes* node-list))

;;; this can be done in the election handler, or use this code and ensure
;;; that each node-handler responds to :hold-an-election
;;; (defun send-all (n)
;;;   (mapc (lambda (node)
;;;           (send (node-ip-addr node) :hold-an-election n))
;;;         *all-nodes*))

(defvar *beacon-actor*
  (ac:make-actor
   (lambda (&rest msg)
     (labels ((hold-election ()
                (let ((rand (/ (random 1000000) 1000000)))
                  (ac:pr (format nil "sending :hold-an-election = ~A" rand))
                  (mapc #'(lambda (pair)
                            (destructuring-bind (node-pkey node-stake) pair
                              (ac:send node-pkey :hold-an-election
                                       :n rand)))
                        cosi-simgen::*all-nodes*))))
       (um:dcase msg
         (:start ()
          (ac:pr "Beacon started")
          ;; recv also restarts the clock
          (ac:recv
            ((list :start)
             (ac:pr "Beacon already running")
             (ac:retry-recv))
            
            ((list :kill)
             (ac:pr "Beacon terminated"))
            
            ((list :hold-election)
             (hold-election)
             (ac:retry-recv))
            
            :TIMEOUT    *beacon-interval*
            :ON-TIMEOUT (progn
                          (hold-election)
                          (ac:retry-recv))
            ))

         (:kill ()
          (ac:pr "Beacon wasn't running"))
         
         (:hold-election ()
          (hold-election))
         )))))

(defun fire-election ()
  ;; for manual testing...
  (ac:send *beacon-actor* :hold-election))

(defun make-election-beacon ()
  (ac:send *beacon-actor* :start))

(defun kill-beacon ()
  (ac:send *beacon-actor* :kill))

;; --------------------------------------------------

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

#|
(defmethod node-stake ((node cosi-simgen:node))
  (cosi-simgen:node-stake node))
|#
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
  (let* ((tree    (car (um:nlet-tail iter ((nodes cosi-simgen::*all-nodes*))  ;; tail recursive, scheme-like, named let
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



(defmethod cosi-simgen:node-dispatcher ((msg-sym (eql :hold-an-election)) &key n)
  (let* ((node   (node:current-node))
         (me     (node:pkey node))
         (stake  (node:stake node))
         (winner (hold-election n)))
    (setf (node:current-leader node) winner)
    (emotiq:note "~A got :hold-an-election ~A" (node:short-id node) n)
    (let ((iwon (vec-repr:int= winner me)))
      (emotiq:note "election results ~A (stake = ~A)"
                   (if iwon " *ME* " " not me ")
                   stake)
      (emotiq:note "winner ~A me=~A"
                   (node:short-id winner)
                   (node:short-id me))
      (if iwon
          (progn
            (cosi-simgen:send me :become-leader)
            (cosi-simgen:send me :make-block))
          (cosi-simgen:send me :become-witness)))))

(defmethod cosi-simgen:node-dispatcher :around ((msg-sym (eql :block-finished)) &key)
  (call-next-method))


        
    
                                      
