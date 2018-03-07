
(defvar *listener* (mp:make-mailbox))
(mp:process-run-function "Listener" () (lambda ()
                                         (loop
                                          (print (mp:mailbox-read *listener*)
                                                 mp:*background-error-output*))))

(defun tst ()
  (unwind-protect
      (progn
        (sleep 10)
        (mp:mailbox-send *listener* :done))
    (mp:mailbox-send *listener* :unwinding)))

(mp:mailbox-send *listener* :test-message)
(tst)


