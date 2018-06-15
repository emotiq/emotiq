(defpackage wallet/server
  (:use :cl))
(in-package :wallet/server)

(defclass echo-server (hunchensocket:websocket-resource)
  ((path :initarg :path :reader path))
  (:default-initargs :client-class 'echo-server-client))

(defclass echo-server-client (hunchensocket:websocket-client)
  ((user-agent :initarg :user-agent :reader user-agent)))

(defvar *echo-server* (make-instance 'echo-server :path "/echo"))

(defun main-handler (request)
  (let ((path (hunchentoot:script-name request)))
    (if (string= path (path *echo-server*))
        *echo-server*
        (note "No path registered for request '~a'" path))))

(defun initialize-handlers ()
  (pushnew 'main-handler hunchensocket:*websocket-dispatch-table*))
  
(defun start-server ()
  (initialize-handlers)
  (hunchentoot:start 
   (make-instance 'hunchensocket:websocket-acceptor :port 3145)))

(defmethod hunchensocket:client-connected ((resource echo-server) user-agent)
  (note "~a has connected to ~a" user-agent resource))

(defmethod hunchensocket:client-disconnected ((resource echo-server) user-agent)
  (note "~a has disconnected from ~a" user-agent resource))

(defmethod hunchensocket:text-message-received ((resource echo-server) user-agent message)
  (note "~a send message ~a" user-agent message))



