(in-package :emotiq/tracker)

(defclass system-state ()
  ((leader :accessor system-leader :initform nil)
   (witnesses :accessor system-witness-list :initform nil)
   (all-nodes :accessor system-all-nodes :initform nil)))  ;; contains leader, witnesses and any other nodes (in early testing, no others)
   
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
  ;; started in more than one place - emotiq:main starts it
  ;; and, because the REST server is running the simulator, it starts it again (by calling initialize)
  ;; start it only once
  (unless (and *state* *tracking-actor*)  
    (let ((state (make-instance 'system-state))
          (tracking-actor (actors:make-actor #'do-tracking)))
      (emotiq:note "running start-tracker ~A ~A" state tracking-actor)
      (setf *state* state
            *tracking-actor* tracking-actor)))
  (assert (eq 'actors:actor (type-of *tracking-actor*)))
  (assert (eq 'system-state (type-of *state*)))
  (values *state* *tracking-actor*))

(defun do-tracking (msg)
  (case (first msg) 
    (:reset
     (emotiq:note "Tracker: :reset - state cleared")
     (setf (system-leader *state*) nil
           (system-witness-list *state*) nil))

    (:node
     (let ((node (second msg)))
       (assert (not (member node (system-all-nodes *state*))))
       (unless (member node (system-all-nodes *state*))
         (push node (system-all-nodes *state*)))))

    (:election
     (emotiq:note "Tracker: :election - state cleared")
     (start-tracker))

    (:block-finished
     (emotiq:note "Tracker: :block-finished, ~%state = ~A" (query-current-state)))

    ((:make-block :commit :prepare)
     ;; tbd
     )
    
    (:new-leader
     (let ((leader-node (second msg)))
       (setf (system-leader *state*) leader-node)))
    
    (:new-witness
     (let ((witness-node (second msg)))
       (push witness-node (system-witness-list *state*))))))

(defun query-current-state ()
  "return current system state as an alist"
  (let ((result nil))
    (let ((witnesses nil))
      (dolist (w (system-witness-list *state*))
        (push (stringify-node w) witnesses))
      (push (cons :witnesses witnesses) result)
      (push (cons :leader (stringify-node (system-leader *state*))) result)
      (push (cons :all-nodes (mapcar #'stringify-node (system-all-nodes *state*))) result)
      result)))

(defun stringify-node (n)
  "return some string representation for given node"
  (or
   ;; whatever is most appropriate - if node-ip is useful, then export it
   (ignore-errors (cosi-simgen::node-ip n))
   (ignore-errors (format nil "~s" n))
   "Failed to stringify node"))


;;
;; examples of how to fetch all nodes in a system (sim) and
;; how to grab slots and create string representations
;; - see emotiq/src/Cosi-BLS/new-transactions.lisp/dump-txs
;; - look at the definition of "node" in emotiq/src/Cosi-BLS/cosi-construction.lisp for
;;   what slots are available
;;

(defun query-raw-nodes ()
  "return a list of nodes as lisp data, which can be further nspected"
  (system-all-nodes *state*))

(defun get-notes (n)
  "return a string of notes from given node n (once-only)"
  (get-output-stream-string (cosi-simgen::node-notestream n)))

(defun get-all-info ()
  "return a string of notes from each of the nodes in the system"
  (with-output-to-string (out)
    (mapc #'(lambda (n)
              (format out "~%~%~a:~%~a~%mempool=~%~a~%blockchain=~%~a"
                      (stringify-node n)
                      (get-notes n)
                      (get-mempool n)
                      (get-blockchain n)))
          (query-raw-nodes))))

(defun get-mempool (n)
  "returns a string dump of mempool for node n"
  (let ((emotiq:*notestream* (make-string-output-stream)))
    (if (= 0 (hash-table-count (cosi-simgen::node-mempool n)))
        (emotiq:note "EMPTY")
      (cosi/proofs/newtx:dump-txs :mempool t :node n))
    (get-output-stream-string emotiq:*notestream*)))

(defun get-blockchain (n)
  "returns a string dump of blockchain for node n"
  (let ((emotiq:*notestream* (make-string-output-stream)))
    (if (= 0 (length (cosi-simgen::node-blockchain n)))
        (emotiq:note "EMPTY")
      (cosi/proofs/newtx:dump-txs :blockchain t :node n))
    (get-output-stream-string emotiq:*notestream*)))
