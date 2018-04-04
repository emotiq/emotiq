(in-package "EMOTIQ")

(defun simple-test ()
  ;; (pbc:make-key-pair :dave)
  ;; (let ((signed (pbc:sign-message :hello)))
  ;;   (if (pbc:check-message signed)
  ;;     (format *standard-output* "~%NOT OK~%"))))
  (pbc:init-pairing)
  (format *standard-output* "~%OK~%")
)

(defparameter *production* nil)

(defun production-p () 
  *production*)

(defun production-start ()
  (setf *production* t)
  (start))

(defun dev-start ()
  (setf *production* nil)
  (start))

(defun start ()
  (format *standard-output* "1: alloc=~a~%" (hcl:total-allocation))
  ;(cl:room)
  (if (production-p)
      (format *standard-output* "~%running production~%")
    (format *standard-output* "~%running development~%"))
  (format *standard-output* "2: alloc=~a~%" (hcl:total-allocation))
  ;(cl:room)
  (simple-test)
  (format *standard-output* "3: alloc=~a~%" (hcl:total-allocation))
  (cl:room)
  )
    


