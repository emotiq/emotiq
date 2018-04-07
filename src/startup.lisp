(in-package "EMOTIQ")

(defun simple-test (&optional how-started-message?)
  (message-running-state how-started-message?)
  (pbc::need-pairing) ;; initializes init-pairing, not exported (no use for it in future systems)
  (pbc:init-pairing)
  (let ((context (start-blockchain-context)))
    (with-blockchain-context (context)
      (let ((genesis-block (make-genesis-block)))
        (format *standard-output*
                "~&Here is the first transaction of the genesis block:~%  ~a~%"
                genesis-block)))))


(defun message-running-state (&optional how-started-message?)
  (format *standard-output* "~%Running ~a in ~a~%"
          (or how-started-message? "interactively")
          (if (production-p) "production" "development")))


(defun start ()

  ;; This is for running in command line only. For now, if we're
  ;; starting from the command line, we assume it's for
  ;; production. Later, we'll have other means of setting
  ;; *production*. TEMPORARY! FIX! 4/6/18
  (setq *production* t)

  (message-running-state "from command line")

  (simple-test))
