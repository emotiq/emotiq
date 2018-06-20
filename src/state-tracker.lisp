(in-package :emotiq/tracker)

(defclass system-state ()
  ((leader :accessor system-leader :initform nil)
   (witnesses :accessor system-witness-list :initform nil)))
   
;(let (state
;      tracking-actor)
; a lexical binding at the top-level reduces the effectiveness of the LW debugger

(defparameter *state* nil)
(defparameter *tracking-actor* nil)

(defun track (&rest msg)
  (if (and
       (eql (first msg) :reset)
       (null *tracking-actor*))
      (emotiq:note "~%*** Don't care :reset ***~%")  ;; don't care about this condition
    (actors:send *tracking-actor* msg)))

(defun start-tracker ()
  "returns an actor that can be sent messages about changes to the system state"
  (setf *state* (make-instance 'system-state)
        *tracking-actor* (actors:make-actor #'do-tracking))
  (emotiq:note "running start-tracker ~A ~A" *state* *tracking-actor*))

(defun do-tracking (msg)
  (emotiq:note "do-tracking ~A" msg)
  (case (first msg) 
    (:reset
     (emotiq:note "tracker got :reset - state cleared")
     (start-tracker))

    (:election
     (emotiq:note "tracker got :election - state cleared")
     (start-tracker))

    ((:make-block :block-finished :commit :prepare)
     ;; tbd
     )
    
    (:new-leader
     (let ((leader-node (second msg)))
       (emotiq:note "New Leader ~A" (stringify-node leader-node))
       (setf (system-leader *state*) leader-node)))
    
    (:new-witness
     (let ((witness-node (second msg)))
       (emotiq:note "New witness ~A" (stringify-node witness-node))
       (push witness-node(system-witness-list *state*))))))

(defun query-current-state ()
  "return current system state as an alist"
  (let ((result nil))
    (let ((witnesses nil))
      (dolist (w (system-witness-list *state*))
        (push (stringify-node w) witnesses))
      (push (cons :witnesses witnesses) result)
      (push (cons :leader (stringify-node (system-leader *state*))) result)
      result)))

(defun stringify-node (n)
  "return some string representation for given node"
  (cosi-simgen::node-ip n)) ;; whatever is most appropriate - is node-ip is useful, then export it