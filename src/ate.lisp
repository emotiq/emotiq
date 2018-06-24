;; API to REST interface

(in-package :emotiq/ate)

(defun begin ()
  "load and run a non-simulation Emotiq Node"
  (error "NIY emotiq/ate:begin")
  (asdf:make :emotiq/startup)
  (emotiq:main))

(defun wind-down ()
  "kill a non-simulation Emotiq Node"
  (error "NIY emotiq/ate:wind-down")
  (kill-wallet)
  (gossip:shutdown-gossip-server)
  (actors:kill-executives))

(defparameter *initialized* nil) ;; TODO: delete this kludge and replace with wind-down-sim which unitializes properly

(defun begin-sim ()
  "load, intialize and run an Emotiq Simulation system on one machine instance"
  (when (not *initialized*)  ;; prevent multiple initializations, TODO: kludge, fix this
    (asdf:make "emotiq/sim")
    (emotiq/sim:initialize)
    (setf *initialized* t))
  (emotiq/sim:run-new-tx))

(defun wind-down-sim ()
  "kill the simulator"
  (kill-wallet)
  (gossip:shutdown-gossip-server)
  (actors:kill-executives))

(defun kill-wallet ()
  "wind down the wallet"
  ;; is this necessary?
  )

;; common to real and simulated
(defun introspect ()
  "return an alist of current system state"
  ;; TODO:  add more interesting stuff to tracker
  (emotiq/tracker:query-current-state))
