(in-package :emotiq)

(defparameter *production* nil)

(defun production-p () *production*)

(defun simple-test ()
  (pbc:make-key-pair :dave)
  (let ((signed (pbc:sign-message :hello)))
    (let ((ok (pbc:check-message signed)))
      ;; it seems that the delivered version does not print out the result by default, so let's print something
      (if ok 
	  (format *standard-output* "~%OK~%~%")
	(format *standard-output* "~%NOT OK~%~%"))
      ok)))

;; delete this when we have nodes
(defun start ()
  ;; 
  (let ((argv (or 
	       #+CLISP *args*
	       #+SBCL *posix-argv*  
	       #+LISPWORKS system:*line-arguments-list*
	       #+CMU extensions:*command-line-words*
	       nil)))
    (when (or 
	   (string= "emotiq" (first argv))
	   (string= "./emotiq" (first argv)))
      (setq *production* t)

      ;; -- start delete --
      (format *standard-output* "~%running production~%~%")
      (simple-test)
      ;; -- end delete --

      ;; call node startup code

      )
    (unless (production-p)
      (format *standard-output* "~%running developer version~%~%"))))

