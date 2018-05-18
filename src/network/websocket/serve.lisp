(defpackage wallet/server
  (:use :cl))
(in-package :wallet/server)

(defun note (message-or-format &rest args)
  "Emit a note of progress to the appropiate logging system."
  (let ((formats '(simple-date-time:|yyyymmddThhmmssZ|
                   simple-date-time:|yyyy-mm-dd hh:mm:ss|)))
    (format *error-output* 
            "~&~a ~a~&"
            (apply (second formats)
                   (list (simple-date-time:now)))
            (apply 'format 
                 nil
                 message-or-format
                 (if args args nil)))))

(defvar *application*
  (lambda (env)
    (let ((uri-path (getf env :request-uri)))
      (cond
        ((string= "/echo" uri-path)
         (let ((ws (wsd:make-server env)))
           (wsd:on :message ws
                   (lambda (message)
                     (format *standard-output* "Echo message: ~a~&" message)
                     (wsd:send ws message)))
           (lambda (responder)
             (declare (ignore responder))
             (wsd:start-connection ws))))
        ((string= "/wallet" uri-path)
         (let ((ws (wsd:make-server env)))
           (wsd:on :message ws
                   (lambda (message)
                     (wallet-handler ws message)))
           (lambda (responder)
             (declare (ignore responder))
             (wsd:start-connection ws))))
        (t '(200 (:content-type "text/html")
             ("<html><body>Wallet</body></html>")))))))

(defun wallet-handler (ws message)
  (wsd:Send ws (format nil "wallet: ~a" message)))

(defun main ()
  (clack:clackup *application* :server :hunchentoot :use-thread nil))
