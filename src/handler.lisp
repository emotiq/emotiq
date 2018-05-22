(in-package :emotiq/sim)


;; the code below creates a hierarchical dispatcher - it grabs 3 messages (:hold-an-election ,:make-block and :block-finished)
;; and deals with them, all other messages are sent downwards, to cosi-simgen::node-dispatcher

;; I have specifically not used dlambda, in hopes of demonstrating that a msg is very simple and can be pulled apart using
;; simple Lisp functions

;; :block-finished is a "new" message that is sent by the leader to itself in cosi-handlers, at the end of the COMMIT
;; phase, for synchronization, i.e. the Leader goes through the various phases (prepare and commit), then signals to
;; itself that COMMIT is finished (since everything is asynchronous, we can't know when the COMMIT has finished, other
;; than by sending a message).

(defmethod cosi-simgen:node-dispatcher ((msg-sym (eql :hold-an-election)) &key n)
  (ac:pr (format nil "got :hold-an-election ~A" n))
  (let* ((node   (cosi-simgen:current-node))
         (winner (emotiq/elections:hold-election n)))
    (let ((me (eq winner node)))
      (ac:pr (format nil "election results(~A) ~A" n (if me " *ME* " " not me ")))
      (ac:pr (format nil "winner ~A me=~A" winner node))
      (when me
        (cosi-simgen:send node :make-block)))))

(defmethod cosi-simgen:node-dispatcher ((msg-sym (eql :make-block)) &key)
  (cosi-simgen:leader-exec cosi-simgen:*cosi-prepare-timeout* cosi-simgen:*cosi-commit-timeout*))

(defmethod cosi-simgen:node-dispatcher :around ((msg-sym (eql :block-finished)) &key)
  (call-next-method))

#|
(defun node-dispatcher (node &rest msg)

  (if (and (= 2 (length msg))
           (eq :hold-an-election (first msg))
           (numberp (second msg)))
      (let ((n (second msg)))
        (ac:pr (format nil "got :hold-an-election ~A" n))
        (let ((winner (emotiq/elections:hold-election n)))
          (let ((me (eq winner node)))
            (ac:pr (format nil "election results(~A) ~A" n (if me " *ME* " " not me ")))
            (ac:pr (format nil "winner ~A me=~A" winner node))
            (cosi-simgen::send node :make-block))))
    
    ;; elsif
    (if (and (= 1 (length msg))
             (eq :make-block (first msg)))
        (when (cosi-simgen::node-blockchain node) ;; don't publish empty blocks
          (cosi-simgen::leader-exec node cosi-simgen::*cosi-prepare-timeout* cosi-simgen::*cosi-commit-timeout*))

      ;; elsif
      (if (and (= 1 (length msg))
               (eq :block-finished (first msg))) ;; sent by tail end of cosi-handlers/leader-exec
          (progn
            (emotiq/elections::kill-beacon) ;; for simulator - so that we don't get a periodic call for elections when the simulated run is finished
            (ac:pr "Block committed to blockchain")
            (ac:pr (format nil "Block signatures = ~D" (logcount (cosi/proofs:block-signature-bitmap new-block)))))
        
        ;; else delegate message to cosi-simgen::node-dispatcher
        (apply 'cosi-simgen::node-dispatcher node msg)))))
|#