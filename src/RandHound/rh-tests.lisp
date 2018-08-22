
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


(prove:plan 1)
(let ((cosi-simgen::*testing-randhound* t))
  (prove:ok (emotiq:main))
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
        (start-randhound-round)))))

(prove:finalize)

#|
(test-rh)
|#
