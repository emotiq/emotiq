(in-package "EMOTIQ")

(defun main (&optional how-started-message?)
  (message-running-state how-started-message?)
  (format *standard-output* "Making key pair…")
  (let ((keypair (pbc:make-key-pair :foo)))
    (format *standard-output* "  Created ~a~&" keypair)))

(defun message-running-state (&optional how-started-message?)
  (format *standard-output* "~%Running ~a in ~a~%with args [~a]~%"
          (or how-started-message? "interactively")
          (if (production-p) "production" "development")
	  (argv)))


(defun argv ()
#+lispworks system:*line-arguments-list*)
  

(defun start ()
  ;; This is for running in the binary command line only. For now, if we're
  ;; starting from the command line, we assume it's for
  ;; production. Later, we'll have other means of setting
  ;; *production*. TEMPORARY! FIX! 4/6/18
  ;; ^^ in this context "production" ONLY means binary build.
  (setq *production* t)
  (message-running-state "from command line")
  
  (main))



