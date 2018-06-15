;; A chat server in 30 lines
;; -------------------------

;; First define classes for rooms and users. Make these subclasses of
;; `websocket-resource` and `websocket-client`.

(defpackage :my-chat
  (:use :cl)
  (:export :main))
            
(in-package :my-chat)

(defclass chat-room (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this room!") :reader name))
  (:default-initargs :client-class 'user))

(defclass user (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this user!"))))

;; Define a list of rooms. Notice that
;; `hunchensocket:*websocket-dispatch-table*` works just like
;; `hunchentoot:*dispatch-table*`, but for websocket specific resources.

(defvar *chat-rooms* (list (make-instance 'chat-room :name "/bongo")
                           (make-instance 'chat-room :name "/fury")))

(defun find-room (request)
  (find (hunchentoot:script-name request) *chat-rooms* :test #'string= :key #'name))

(eval-when (:load-toplevel :execute)
  (pushnew 'find-room hunchensocket:*websocket-dispatch-table*))

;; OK, now a helper function and the dynamics of a chat room.

(defun broadcast (room message &rest args)
  (loop for peer in (hunchensocket:clients room)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defmethod hunchensocket:client-connected ((room chat-room) user)
  (broadcast room "~a has joined ~a" (name user) (name room)))

(defmethod hunchensocket:client-disconnected ((room chat-room) user)
  (broadcast room "~a has left ~a" (name user) (name room)))

(defmethod hunchensocket:text-message-received ((room chat-room) user message)
  (broadcast room "~a says ~a" (name user) message))  

;; Finally, start the server. `hunchensocket:websocket-acceptor` works
;; just like `hunchentoot:acceptor`, and you can probably also use
;; `hunchensocket:websocket-ssl-acceptor`.

(defvar *server* nil) 

(defun main (&key (port 5000))
  (unless *server*
    (format *standard-output* "~&Creating new *server*.~&")
    (setf *server*
     (make-instance 'hunchensocket:websocket-acceptor :port port)))
  (hunchentoot:start *server*))
