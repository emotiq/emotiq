(defpackage websocket-test
  (:use :cl)
  (:export #:main))
(in-package :websocket-test)

(defparameter *handler* :hunchentoot)

(defvar *app*
  (lambda (env)
    (cond
      ((string= "/echo" (getf env :request-uri))
       (let ((ws (wsd:make-server env)))
         (wsd:on :message ws
             (lambda (message)
               (format *standard-output* "Echo message: ~a~&" message)
               (wsd:send ws message)))
         (lambda (responder)
           (declare (ignore responder))
           (wsd:start-connection ws))))
      (T
       '(200 (:content-type "text/html")
         ("<html>
  <head>
    <script type=\"text/javascript\">
      var ws = null;
      function connect() {
        ws = new WebSocket(\"ws://localhost:5000/echo\");
        ws.onmessage = function(evt) { console.log(evt.data); };
      }
      function send(message) {
        ws.send(message)
      }
    </script>
  </head>
  <body>
    Open JavaScript console.
  </body>
</html> "))))))

(defun main ()
  (clack:clackup *app* :server *handler* :use-thread nil))

#+(or)
(clack:clackup *app* :server *handler* :use-thread nil)
