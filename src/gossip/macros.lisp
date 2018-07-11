;;; macros.lisp
;;; macros for the gossip system

(in-package :gossip)

(defparameter *gossip-absorb-errors* t "True for normal use; nil for debugging")

(defmacro gossip-handler-case (form &rest clauses)
  "Use this when you're tempted to use handler-case.
   Because sometimes you really do want a backtrace when an error happens."
  `(if *gossip-absorb-errors*
       (handler-case ,form
         ,@clauses)
       ,form))