(in-package :emotiq/sim)

(defmethod cosi-simgen:node-dispatcher ((msg-sym (eql :hold-an-election)) &key n)
  (let* ((node   (cosi-simgen:current-node))
         (winner (emotiq/elections:hold-election n)))
    (ac:pr (format nil "got :hold-an-election ~A #~A ~A in ~A (~A)" n (election-number-or-string)
                   (cosi-simgen::node-ip node)
                   (cosi-simgen::node-real-ip node)
                   (cosi-simgen::short-id node)))
    (let ((me (eq winner node)))
      (ac:pr (format nil "election results(~A) ~A" n (if me " *ME* " " not me ")))
      (ac:pr (format nil "winner ~A me=~A" winner node))
      (when me
        (increment-election-number)
        (cosi-simgen:send node :make-block)))))

(defmethod cosi-simgen:node-dispatcher ((msg-sym (eql :make-block)) &key)
  (cosi-simgen:leader-exec cosi-simgen:*cosi-prepare-timeout* cosi-simgen:*cosi-commit-timeout*))

(defmethod cosi-simgen:node-dispatcher :around ((msg-sym (eql :block-finished)) &key)
  (prog1
      (call-next-method)
    (when (simulation-ended-p)
      (emotiq/elections:kill-beacon)))) ;; for simulator
