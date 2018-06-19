(in-package :emotiq/tracker)

(defclass system-state ()
  ((leader :accessor system-leader :initform nil))
  ((witnesses :accessor system-witness-list :initform ()))) 
   
(let (state
      tracking-actor)

  (defun track (&rest msg)
    (actors:send tracking-actor msg))

  (defun start-tracker ()
    "returns an actor that can be sent messages about changes to the system state"
    (setf state (make-instance 'system-state)
          tracking-actor (lambda (&rest msg)
                           (do-tracking msg)))))

  (defun do-tracking (&rest msg)
    (case (first msg)
      (:election
       (setf (system-leader state) nil
             (system-witness-list state) nil))
      
      (:new-leader
       (let ((leader-node (second msg)))
         (setf (system-leader state) leader-node)))
      
      (:new-witness
       (setf (witness-node state) (second msg)))))
          
  (defun query-current-state ()
    (let ((result nil))
      (push (cons :leader (stringify-node (leader-node))) result)
      (let ((witnesses nil))
        (dolist (w (system-witness-list state))
          (push (stringify-node w) witness))
        (push (cons :witnesses w) result)
      result))))



         