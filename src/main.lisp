(in-package :emotiq)

(defun simple-test ()
  (pbc:make-key-pair :dave)
  (let ((signed (pbc:sign-message :hello)))
    (let ((ok (pbc:check-message signed)))
      ;; it seems that the delivered version does not print out the result by default, so let's print something
      (if ok 
	  (format *standard-output* "~%OK~%~%")
	(format *standard-output* "~%NOT OK~%~%"))
      ok)))

(defparameter *production* nil)
(defun production-p () *production*)

(defun production-start ()
  (setf *production* t)
  (start-main))

(defun start ()
  (setf *production* nil)
  (start-main))

;; delete this when we have nodes
(defun start-main ()
  (if (production-p)
      (format *standard-output* "~%running production~%")
    (format *standard-output* "~%running developer~%"))
  (simple-test))

