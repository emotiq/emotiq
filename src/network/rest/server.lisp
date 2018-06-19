(in-package :emotiq-rest)

(defun start-server (&key (port 3140) (host "127.0.0.1"))
  (let ((api :route.emotiq/0/0/1))
    (restas:start api
                  :port port
                  :acceptor-class 'restas:restas-acceptor
                  :address host)
    (emotiq:note "Starting REST server for api '~a' listening on port ~a" api port)
    (with-slots ((port hunchentoot::port)
                 (host hunchentoot::address))
        (find port restas::*acceptors* :key #'hunchentoot:acceptor-port)
      (emotiq:note "An index of available services be viewed as <http://~a:~a>" host port))))
            
(defun stop-server ()
  (ignore-errors
    (emotiq:note "Terminating all RESTAS acceptors with extreme prejudice.")
    (restas:stop-all)))

(defun restart-server ()
  (stop-server)
  (start-server))





    


