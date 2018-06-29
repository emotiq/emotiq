(in-package :emotiq/tracker)

(defclass system-state ()
  ((leader :accessor system-leader :initform nil)
   (witnesses :accessor system-witness-list :initform nil)
   (events :accessor system-events :initform nil)
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
  (flet ((clear-state ()
           (setf (system-leader *state*) nil
                 (system-witness-list *state*) nil
                 (system-events *state*) nil)))
    (case (first msg) 
      (:reset
       (emotiq:note "Tracker: :reset")
       (let ((node (second msg)))
         (push `(:idle . ,(stringify-node node)) (system-events *state*))))

      (:idle
       (emotiq:note "Tracker: :reset")
       (let ((node (second msg)))
         (push `(:idle . ,(stringify-node node)) (system-events *state*))))

      (:node
       (let ((node (second msg)))
         (assert (not (member node (system-all-nodes *state*))))
         (unless (member node (system-all-nodes *state*))
           (push node (system-all-nodes *state*)))))
      
      (:election
       (emotiq:note "Tracker: :election - state cleared"))

      (:block-finished
       (emotiq:note "Tracker: :block-finished, ~%state = ~A" (query-current-state))
       (let ((leader-node (second msg)))
         (if (eq leader-node (system-leader *state*))
             (push :block-finished (system-events *state*))
           (progn
             (emotiq:note "wrong leader block-finished ~a <> ~a" leader-node (system-leader *state*))
             (push `(:wrong-leader-block-finished . ,leader-node) (system-events *state*))))))

      (:prepare
       (let ((node (second msg)))
         (push `(:node-enters-prepare . ,(stringify-node node)) (system-events *state*))))

      (:commit
       (let ((node (second msg)))
         (push `(:node-enters-commit . ,(stringify-node node)) (system-events *state*))))

      (:prepare-signed
       (let ((node (second msg)))
         (push `(:node-signed-prepare . ,(stringify-node node)) (system-events *state*))))

      (:commit-signed
       (let ((node (second msg)))
         (push `(:node-signed-commit . ,(stringify-node node)) (system-events *state*))))

      (:make-block
       ;; tbd
       )
    
      (:new-leader
       (let ((leader-node (second msg)))
         (setf (system-leader *state*) leader-node)))
    
      (:new-witness
       (let ((witness-node (second msg)))
         (push witness-node (system-witness-list *state*))))

      (:leader-sends-prepare
       (let ((leader-node (second msg)))
         (if (eq leader-node (system-leader *state*))
             (push :leader-sends-prepare (system-events *state*))
           (progn
             (emotiq:note "wrong leader sends prepare ~a <> ~a" leader-node (system-leader *state*))
             (push `(:wrong-leader-sends-prepare . ,leader-node) (system-events *state*))))))

      (:leader-sends-commit
       (let ((leader-node (second msg)))
         (if (eq leader-node (system-leader *state*))
             (push :leader-sends-commit (system-events *state*))
           (progn
             (emotiq:note "wrong leader sends commit ~a <> ~a" leader-node (system-leader *state*))
             (push `(:wrong-leader-sends-commit . ,leader-node) (system-events *state*)))))))))
      
(defun query-current-state ()
  "return current system state as an alist"
  (let ((result nil))
    (let ((witnesses nil))
      (dolist (w (system-witness-list *state*))
        (push (stringify-node w) witnesses))
      (push (cons :witnesses witnesses) result)
      (push (cons :leader (stringify-node (system-leader *state*))) result)
      (push (cons :all-nodes (mapcar #'stringify-node (system-all-nodes *state*))) result)
      (push (cons :events `(,(reverse (system-events *state*)))) result)
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

(defun get-blockchain (n)
  "returns a string dump of blockchain for node n"
  (let ((emotiq:*notestream* (make-string-output-stream)))
    (if (= 0 (length (cosi-simgen::node-blockchain n)))
        (emotiq:note "EMPTY")
      (cosi/proofs/newtx:dump-txs :blockchain t :node n))
    (get-output-stream-string emotiq:*notestream*)))

(defun get-mempool (n)
  "returns a string dump of mempool for node n"
  (let ((emotiq:*notestream* (make-string-output-stream)))
    (if (= 0 (hash-table-count (cosi-simgen::node-mempool n)))
        (emotiq:note "EMPTY")
      (cosi/proofs/newtx:dump-txs :mempool t :node n))
    (get-output-stream-string emotiq:*notestream*)))

;; code borrowed from emotiq/src/Cosi-BLS/new-transactions.lisp/dump-tx
(defmethod tx-as-alist ((tx cosi/proofs/newtx:transaction) &key out-only)
  (let ((result nil))
    (unless (or out-only
                (member (cosi/proofs/newtx:transaction-type tx)
                        '(:coinbase :collect))) ; no-input tx types
      (loop for tx-in in (cosi/proofs/newtx:transaction-inputs tx)
            do
            (push `(:input . ((:index . ,(format nil "~A" (cosi/proofs/newtx:tx-in-index tx-in)))
                              (:txid . ,(cosi/proofs/newtx:txid-string (cosi/proofs/newtx::tx-in-id tx-in)))))
                  result)))
    (loop for tx-out in (cosi/proofs/newtx::transaction-outputs tx)
          as i from 0
          do
          (push `(:output . ((:amount . ,(format nil "~A" (cosi/proofs/newtx:tx-out-amount tx-out)))
                             (:pubkey . ,(format nil "~A" (cosi/proofs/newtx:tx-out-public-key-hash tx-out)))))
                result))
    (push `(:txid . ,(cosi/proofs/newtx::txid-string (cosi/proofs/newtx:transaction-id tx))) result)
    (unless (eq (cosi/proofs/newtx::transaction-type tx) ':spend)
      (push `(:transaction-type . ,(format nil "~A" (cosi/proofs/newtx:transaction-type tx))) result))
    result))

(defun example ()
  "example of fetching alist of tx after a run of the run-new-tx simulator"
  (let* ((nodes (query-raw-nodes))
         (blockchain (cosi-simgen::node-blockchain (first nodes)))
         (topblock (first blockchain))
         (transactions-in-top-block (cosi/proofs:block-transactions topblock))
         (tx2 (second transactions-in-top-block))
         (a (tx-as-alist tx2)))
    a))

