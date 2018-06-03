(in-package "EMOTIQ")

(defparameter *production* nil)

(defun production-p () *production*)

(defparameter *continuous-integration* nil)

(defun ci-p ()
  *continuous-integration*)

(defun set-ci ()
  (setf *continuous-integration* t))