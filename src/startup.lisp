(in-package "EMOTIQ")

(defun simple-test ()
  ;; (pbc:make-key-pair :dave)
  ;; (let ((signed (pbc:sign-message :hello)))
  ;;   (if (pbc:check-message signed)
  ;;     (format *standard-output* "~%NOT OK~%"))))
  (pbc::need-pairing) ;; initializes init-pairing, not exported (no use for it in future systems)
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
  ;(cl:room)
  (if (production-p)
      (format *standard-output* "~%running production~%")
    (format *standard-output* "~%running development~%"))
  ;(cl:room)
  (simple-test)
  )
    


