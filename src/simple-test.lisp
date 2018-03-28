(in-package "CL-USER")

(defun simple-test ()
  (pbc:make-key-pair :dave)
  (let ((signed (pbc:sign-message :hello)))
    (let ((ok (pbc:check-message signed)))
      ;; it seems that the delivered version does not print out the result by default, so let's print something
      (if ok 
	  (format *standard-output* "~%OK~%~%")
	(format *standard-output* "~%NOT OK~%~%"))
      ok)))

