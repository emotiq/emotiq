(in-package :emotiq/sim)

(defmethod cosi-simgen:node-dispatcher ((msg-sym (eql :hold-an-election)) &key n)
  (when cosi-simgen::*holdoff*
    (ac:pr "Election delayed by holdoff"))
  (unless cosi-simgen::*holdoff*
    (let* ((node   (cosi-simgen:current-node))
           (me     (cosi-simgen:node-pkey node))
           (stake  (cosi-simgen:node-stake node))
           (winner (emotiq/elections:hold-election n)))
      (setf (cosi-simgen:node-current-leader node) winner)
      (emotiq:note "~A got :hold-an-election ~A" (cosi-simgen::short-id node) n)
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
  (call-next-method))
