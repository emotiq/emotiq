;; create a hook to node-dispatcher from cosi-handlers.lisp

#|
(in-package :cosi-simgen)

(defun make-node-dispatcher (node)
  "override make-node-dispatcher in cosi-handlers.lisp with our own dispatcher which delegates unknown
   messages to cosi-handlers.lisp"
  (ac:make-actor
   (lambda (&rest msg)
     (apply 'emotiq/sim::node-dispatcher node msg))))
|#


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
  (when cosi-simgen::*holdoff*
    (emotiq:note "Election delayed by holdoff"))
  (unless cosi-simgen::*holdoff*
    (let* ((node   (cosi-simgen:current-node))
           (me     (cosi-simgen:node-pkey node))
           (stake  (cosi-simgen:node-stake node))
           (winner (emotiq/elections:hold-election n)))
      (setf (cosi-simgen:node-current-leader node) winner)
      (ac:pr "~A got :hold-an-election ~A" (cosi-simgen::short-id node) n)
      (let ((iwon (vec-repr:int= winner me)))
        (emotiq:note "election results ~A (stake = ~A)"
                     (if iwon " *ME* " " not me ")
                     stake)
        (emotiq:note "winner ~A me=~A"
                     (cosi-simgen::short-id winner)
                     (cosi-simgen::short-id me))
        (if iwon
            (progn
              (cosi-simgen:send me :become-leader)
              (cosi-simgen:send me :make-block))
          (cosi-simgen:send me :become-witness))))))

(defmethod cosi-simgen:node-dispatcher :around ((msg-sym (eql :block-finished)) &key)
  ;; (emotiq/elections::kill-beacon) ;; for simulator - so that we don't get a periodic call for elections when the simulated run is finished
  (call-next-method))

