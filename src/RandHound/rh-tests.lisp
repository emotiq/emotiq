
(in-package :cl-user)

#|
(ql:quickload :emotiq/sim)
(emotiq/sim::initialize)
|#

#+nil
(defun test-rh ()
  (setf cosi-simgen::*testing-randhound* t)
  (let* ((nodes  (coerce cosi-simgen:*node-bit-tbl* 'list)))
    ;; (setf nodes (subseq nodes 0 6))
    (let* ((pkeys  (mapcar 'node-pkey nodes))
           (stakes (mapcar 'node-stake nodes)))
      (cosi-simgen::set-nodes (mapcar 'list pkeys stakes))
      (loop for node in nodes do
            (with-current-node node
              (setf *leader* (first pkeys)
                    *beacon* (second pkeys)
                    *local-epoch* pi
                    *election-calls* nil)))
      (with-current-node (second nodes)
        (start-randhound-round))
      )))

(prove:plan 2)
(let ((cosi-simgen::*testing-randhound* t))
  (prove:ok (emotiq:main) "Starting main loop…")
  (let ((node-bitmap-table (randhound:node-bitmap-table)))
    (prove:ok node-bitmap-table)
    (let ((nodes (coerce (randhound:nodes) 'list)))
      ;; (setf nodes (subseq nodes 0 6))
      ;; Should already have happened..
      #+(or)
      (cosi-simgen::set-nodes (mapcar 'list pkeys stakes))
      (let ((leader (first nodes))
            (beacon (second nodes)))
        (loop :for node :in nodes
           :doing
             (cosi-simgen:with-current-node node
               (setf *leader* leader
                     *beacon* beacon
                     *local-epoch* pi
                     *election-calls* nil)))
        (prove:ok leader "We have a leader…") ;; unused?
        (prove:ok beacon "We have a beacon leader…")

        (prove:plan 2)
        (let ((node (cosi-simgen:current-node)))
          (prove:ok node "Have a current node…")
          (prove:ok (cosi-simgen:node-pkey node) "Current node has a pkey…"))

        (cosi-simgen:with-current-node beacon
          (randhound:start-randhound-round))))))

(prove:finalize)
