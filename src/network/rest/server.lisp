(in-package :emotiq-rest)

(defvar *port* 3140)

(defun start-server (&key (port *port*))
  (let ((api :route.emotiq/0/0/1))
    (let ((base-uri
           (format nil "http://127.0.0.1:~a/" port))
          (server 
           (restas:start api
                         :port port
                         :acceptor-class 'restas:restas-acceptor)))
      (note "Starting REST server for api '~a' listening on port ~a" api port)
      (note "An index of available services be viewed as <~a>" base-uri)
      server)))
            
(defun stop-server ()
  (ignore-errors
    (note "Terminating all RESTAS acceptors with extreme prejudice.")
    (restas:stop-all)))

(defun restart-server ()
  (stop-server)
  (start-server))





    


