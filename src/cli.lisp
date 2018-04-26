(in-package :emotiq/cli)

(defun spawn-cli ()
  (let ((*standard-output* *standard-output*) ;; may need to bind to
                                              ;; other fds depending
                                              ;; on the invocation
                                              ;; scenario
        (*standard-input* *standard-input*))
    (bordeaux-threads:make-thread 'emotiq/cli::main :name "cli-main")))

(defun output-prompt ()
  (format *standard-output* "~&emotiq> "))

(defun read-command ()
  (let ((raw (read-line)))
    (cl-ppcre:split "\\s+" raw)))

(defun main ()
  (let (terminate-cli)
    (loop
       :until terminate-cli
       :doing (progn
                (output-prompt)
                (let ((input (read-command)))
                  (cond
                    ((string-equal (first input)
                                   "quit")
                     (setf terminate-cli t))
                    ((string-equal (first input)
                                   "create-wallet")
                     (emotiq/wallet:create-wallet))
                    ((string-equal (first input)
                                   "open")
                     (emotiq/wallet:open-wallet))
                    ((string-equal (first input)
                                   "see")
                     (emotiq/wallet::see-genesis))
                    ((string-equal (first input)
                                   "help")
                     (format *standard-output* "~&Available commands are~&~
~tquit~&~
~tcreate-wallet~&~
~topen~&~
~thelp~&"))
                    (t 
                     (format *standard-output* "~&Unrecognized command '~{~a~}'.~&Try 'help'.~&" input))))))))
