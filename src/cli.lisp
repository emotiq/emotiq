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

(defparameter *pkey-hash* nil)
(defparameter *pkey* nil)
(defparameter *skey* nil)


(defun main ()
  (let (terminate-cli)
    (emotiq:set-owner "alice")
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
                                "test")
                  (emotiq::test-blockchain))

                 ((string-equal (first input)
                                "test-1")
                  (emotiq::blockchain-test-1))

                 ((string-equal (first input)
                                "test-2")
                  (emotiq::blockchain-test-2))

                 ((string-equal (first input)
                                "test-3")
                  (emotiq::blockchain-test-3))

                 ((string-equal (first input)
                                "test-4")
                  (emotiq::blockchain-test-4))

                 ((string-equal (first input)
                                "alice")
                  (emotiq:set-owner "alice"))
                 ((string-equal (first input)
                                "bob")
                  (emotiq:set-owner "bob"))
                    
                 ((string-equal (first input)
                                "bal")
                  (emotiq:print-balance))
                    
                 ((string-equal (first input)
                                "help")
                  (format *standard-output* "~&Available commands are~&~
~tquit~&~
~tcreate-wallet~&~
~topen~&~
~tbal~&~
~talice~&~
~tbob~&~
~thelp~&"))
                 (t 
                  (format *standard-output* "~&Unrecognized command '~{~a~}'.~&Try 'help'.~&" input))))))))