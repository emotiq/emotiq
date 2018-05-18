;; create a hook to node-dispatcher from cosi-handlers.lisp

(in-package :cosi-simgen)

(defun make-node-dispatcher (node)
  "override make-node-dispatcher in cosi-handlers.lisp with our own dispatcher which delegates unknown
   messages to cosi-handlers.lisp"
  (ac:make-actor
   (lambda (&rest msg)
     (apply 'emotiq/sim::node-dispatcher node msg))))



(in-package :emotiq/sim)

(defun node-dispatcher (node &rest msg)

  (if (and (= 2 (length msg))
           (eq :hold-an-election (first msg))
           (numberp (second msg)))
      (let ((n (second msg)))
        (ac:pr (format nil "got :hold-an-election ~A" n))
        (let ((winner (emotiq/elections:hold-election n)))
          (let ((me (eq winner node)))
            (ac:pr (format nil "~%election results(~A) ~A~%" n (if me " *ME* " " not me ")))
            (ac:pr (format nil "~%winner ~A me=~A~%" winner node))
            (cosi-simgen::send node :make-block))))
    
    ;; else
    (if (and (= 1 (length msg))
             (eq :make-block (first msg)))
        (when (cosi-simgen::node-blockchain node) ;; don't publish empty blocks
          (cosi-simgen::leader-exec node cosi-simgen::*cosi-prepare-timeout* cosi-simgen::*cosi-commit-timeout*))
      
      (if (and (= 1 (length msg))
               (eq :block-finished (first msg))) ;; sent by tail end of cosi-handlers/leader-exec
          (progn
            (emotiq/elections::kill-beacon) ;; for simulator
            (force-epoch-end)
            (ac:pr "Block committed to blockchain")
            (ac:pr (format nil "Block signatures = ~D" (logcount (cosi/proofs:block-signature-bitmap new-block)))))
        
        ;; else delegate message to cosi-simgen::node-dispatcher
        (apply 'cosi-simgen::node-dispatcher node msg)))))
