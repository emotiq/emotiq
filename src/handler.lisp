;; create a hook to node-dispatcher from cosi-handlers.lisp

(in-package :cosi-simgen)

(defun make-node-dispatcher (node)
  "override make-node-dispatcher in cosi-handlers.lisp with our own dispatcher which delegates unknown
   messages to cosi-handlers.lisp"
  (ac:make-actor
   ;; one of these closures is stored in the SELF slot of every node
   (lambda (&rest msg)
     (apply 'emotiq/sim::node-dispatcher node msg))))



(in-package :emotiq/sim)

(defun node-dispatcher (node &rest msg)
  (apply 'cosi-simgen::node-dispatcher node msg))
