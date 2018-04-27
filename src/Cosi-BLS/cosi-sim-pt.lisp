(in-package :cosi-simgen)

(defun cosi-init-mte () 
  (setf *local-nodes*  '(("ProDesk" . "192.168.2.3")))
                         #+(or)
                         ("quip.local"     . "192.168.42.252")
        *real-nodes*  (mapcar 'cdr *local-nodes*)
        *leader-node*  (get-local-ipv4 "ProDesk"))


(defun cosi-generate ()
  (generate-tree :datafile *default-data-file* :keyfile *default-key-file* :nodes 10))
       
