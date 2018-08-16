(in-package :emotiq/app)

(defun make-node0-account ()
  (let ((r (make-instance 'account))
        (k (emotiq/config:get-nth-key 0)))
    (setf (emotiq/app::account-triple r) k
          (emotiq/app::account-skey r) (pbc:keying-triple-skey k)
          (emotiq/app::account-pkey r) (pbc:keying-triple-pkey k)
          (emotiq/app::account-name r) "node 0")
    r))

(defun make-node1-account ()
  (let ((r (make-instance 'account))
        (k (emotiq/config:get-nth-key 1)))
    (setf (emotiq/app::account-triple r) k
          (emotiq/app::account-skey r) (pbc:keying-triple-skey k)
          (emotiq/app::account-pkey r) (pbc:keying-triple-pkey k)
          (emotiq/app::account-name r) "node 1")
    r))

(defun make-node2-account ()
  (let ((r (make-instance 'account))
        (k (emotiq/config:get-nth-key 2)))
    (setf (emotiq/app::account-triple r) k
          (emotiq/app::account-skey r) (pbc:keying-triple-skey k)
          (emotiq/app::account-pkey r) (pbc:keying-triple-pkey k)
          (emotiq/app::account-name r) "node 2")
    r))

(defparameter *node0* nil)
(defparameter *node1* nil)
(defparameter *node2* nil)

(defun test-nodes ()
  (setf *node0* (make-node0-account))
  (setf *node1* (make-node1-account))
  (setf *node2* (make-node2-account))

  (let ((bal0 (get-balance *node0*))
        (bal1 (get-balance *node1*))
        (bal2 (get-balance *node2*)))
    (format *standard-output* "node 0 bal = ~A~%node 1 bal=~A~%node 2 bal = ~A~%"
            bal0 bal1 bal2)))
