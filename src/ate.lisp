;; API to REST interface

(in-package :emotiq/ate)

(defparameter *initialized* nil) ;; TODO: delete this kludge and replace with wind-down-sim which unitializes properly

(defun begin-sim ()
  "load, intialize and run an Emotiq Simulation system on one machine instance"
  (let ((output (make-string-output-stream)))
    (let ((*error-output* output)
          (*standard-output* output))
      (when (not *initialized*)  ;; prevent multiple initializations, TODO: kludge, fix this
        (emotiq/sim:initialize)
        (setf *initialized* t))
      (values
       (emotiq/sim:run-new-tx)
       output))))

(defun wind-down-sim ()
  "kill the simulator"
  (gossip:shutdown-gossip-server)
  (actors:kill-executives))

;; common to real and simulated
(defun introspect ()
  "return an alist of current system state"
  ;; TODO:  add more interesting stuff to tracker
  (emotiq/tracker:query-current-state))
