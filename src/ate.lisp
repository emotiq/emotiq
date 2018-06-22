;; ATE - automated test environment (borrowed from hardware term Automated Test Equipment)

;; API for Rest service

(in-package :emotiq)

(defparameter *initialize-once* nil) ;; TODO: change shutodwn so that it uninitializes correctly

(defun ate-startup ()
  "starts running the Node"
  (asdf:make :emotiq/startup)
  (unless *initialize-once*
    (setf *initialize-once* T))
  (emotiq:main))

(defun ate-shutown ()
  "shut the Node down"
  (setf *initialize-once* nil)
  (actors:kill-executives))

(defun ate-introspect ()
  "Return a copy of the system state as an alist"
  (tracker:query-state))


