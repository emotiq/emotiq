(in-package :emotiq-rest)

(defvar *port* 3140)

(defvar *vc-root* (asdf:system-relative-pathname :emotiq "../"))

(defvar *server-log-dir* (merge-pathnames "var/log/" *vc-root*)
  "Where the REST server outputs its logs.")

(defclass http-acceptor (restas:restas-acceptor)
  ()
  (:default-initargs
   :access-log-destination (merge-pathnames *server-log-dir* "access.log")
   :message-log-destination (merge-pathnames *server-log-dir* "error.log")))

(defun start (&key (port *port*))
  (let ((api :route.emotiq/0/0/1))
    (ensure-directories-exist *server-log-dir*)
    (let ((base-uri
           (format nil "http://127.0.0.1:~a/" port))
          (server 
           (restas:start api
                         :port port
                         :acceptor-class 'http-acceptor)))
      (note "Starting REST server for api '~a' listening on port ~a" api port)
      (note "An index of available services be viewed as <~a>" base-uri)
      server)))
            
(defun stop ()
  (ignore-errors
    (note "Terminating all RESTAS acceptors with extreme prejudice.")
    (restas:stop-all)))

(defun restart-server ()
  (stop)
  (start))




    


