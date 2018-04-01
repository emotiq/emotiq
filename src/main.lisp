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



(defun production-start ()
  (setq *production* t)
  (start))

(defun dev-start ()
  (start))
  
;; delete this when we have nodes
(defun start ()
  ;; -- start delete --
  (if *production*
      (progn
	(format *standard-output* "~%running production~%~%")
	(simple-test))
    (format *standard-output* "~%running developer version~%~%"))
  ;; -- end delete --
  

  ;; call node startup code

  )

