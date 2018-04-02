(in-package "EMOTIQ")

(defun simple-test ()
  (pbc:make-key-pair :dave)
  (let ((signed (pbc:sign-message :hello)))
    (if (pbc:check-message signed)
        (format *standard-output* "~%OK~%")
      (format *standard-output* "~%NOT OK~%"))))

