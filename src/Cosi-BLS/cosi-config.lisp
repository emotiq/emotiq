;;;; Configuration code for cosi handlers
;;;;
;;;; (blocked out separately for now to make merges easier)
(in-package :cosi-simgen)

(defun cosi-init (leader-node-ip)
  "Steps necessary to initialize specials in the cosi-bls system to
   create a machine local simulation network."
  (let ((machine-name "localhost"))
    (setf *local-nodes*  `((,machine-name . ,leader-node-ip))
          *real-nodes*   (list leader-node-ip)
          *leader-node*  leader-node-ip)))

(defun cosi-generate (&key (nodes 10))
  "Generate a Cosi network based on special variable settings."
  (cosi-simgen:generate-tree :datafile *default-data-file*
                             :keyfile  *default-key-file*
                             :nodes nodes))
