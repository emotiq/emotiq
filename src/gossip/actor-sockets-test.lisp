;;; actor-sockets-test.lisp
;;; Tests of actor sockets

(in-package :gossip)

(defun setup-server ()
  ; Start listener socket thread
  (start-gossip-server :TCP))

(defun setup-client ()
  ; Start listener socket thread
  (start-gossip-server :TCP))

  