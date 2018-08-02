(in-package :emotiq/node)


(defvar *top-node* nil
  "current leader node")

(defvar *current-node* nil
  "for sim = current node running")

(defun current-node ()
  *current-node*)


(define-symbol-macro *blockchain*  (blockchain     *current-node*))
(define-symbol-macro *blocks*      (blocks         *current-node*))
(define-symbol-macro *mempool*     (mempool        *current-node*))
(define-symbol-macro *utxos*       (utxos          *current-node*))
(define-symbol-macro *leader*      (current-leader *current-node*))
(define-symbol-macro *tx-changes*  (tx-changes     *current-node*))
(define-symbol-macro *had-work-p*  (had-work-p     *current-node*))

;; election related items
(define-symbol-macro *election-calls* (election-calls *current-node*))
(define-symbol-macro *beacon*         (current-beacon *current-node*))
(define-symbol-macro *local-epoch*    (local-epoch    *current-node*))


;; ----------------------------------------------------------------------
;; Network Tree Nodes

(defclass node (gossip:gossip-node)
  ((ip                          ;; IPv4 string for this node
    :accessor ip       
    :initarg  :ip)
   (real-ip                     ;; real node for this node
    :accessor real-ip  
    :initarg  :real-ip)
   (pkey-zkp                    ;; public key + ZKP
    :accessor pkey-zkp
    :initarg  :pkey-zkp)
   (skey                        ;; private key
    :accessor skey     
    :initarg  :skey)
   (pkey                        ;; cached ECC point for public key
    :accessor pkey     
    :initarg  :pkey)
   (parent                      ;; points to node of group parent
    :accessor parent   
    :initarg  :parent
    :initform nil)
   (other-members               ;; list of group members beyond self
    :accessor other-members
    :initarg  :other-members
    :initform (make-other-members))
   (bitpos                      ;; bit position in bitmap
    :accessor bitpos   
    :initform 0)
   (stake
    :accessor stake
    :initarg  :stake
    :initform 0)
   ;; -------------------------------------
   ;; pseudo-globals per node
   (blockchain
    :accessor blockchain
    :initform nil)
   (blocks
    :accessor blocks
    :initform (make-hash-table :test #'equalp))
   (mempool
    :accessor mempool
    :initform (make-hash-table :test #'equalp))
   (utxos
    :accessor utxos
    :initform (make-hash-table :test #'equalp))
   (current-leader                ;; holds pkey of current leader node
    :accessor current-leader
    :initform nil) 
   (current-beacon                ;; holds pkey of 1-shot beacon assignee
    :accessor current-beacon
    :initform nil) 
   (election-calls                ;; list of pkeys that have called for new election
    :accessor election-calls
    :initform nil)
   (local-epoch                   ;; last election seed
    :accessor local-epoch
    :initform 0)
   (had-work-p                    ;; had-work if we were witness and was called
    :accessor had-work-p          ;; to perform work on a block
    :initform nil)
   (tx-changes
    :accessor tx-changes
    :initform nil)
   ;; -------------------------------------
   (byzantine-p                 ;; indicates byzantine misbehavior
    :accessor byzantine-p      
    :initform nil)
   (corrupted-p                 ;; if true then node was corrupted
    :accessor corrupted-p      
    :initform nil)
   (cpu-load                    ;; cpu loading of group for this node
    :accessor cpu-load     
    :initform 1)
   (self                        ;; ptr to Actor handler
    :accessor self     
    :accessor gossip:application-handler ;; accessor used by gossip system to find our Actor
    :initarg :self)))


;; all lists in slots contain key vectors
(defstruct tx-changes
  tx-dels    ;; list of pending transation removals from mempool
  utxo-adds  ;; list of pending new utxos
  utxo-dels) ;; list of pending spent utxos


(defvar *bins-per-node* 9) ;; prolly needs to be >3 for BFT

(defun make-other-members ()
  (make-array *bins-per-node* :initial-element nil))


(defmethod iteri-other-members ((node node) fn)
  (loop
     :for m :across (other-members node)
     :for ix :from 0
     :when m
     :do (funcall fn ix m)))

(defmethod iter-other-members ((node node) fn)
  (iteri-other-members node (lambda (ix m)
                              (declare (ignore ix))
                              (funcall fn m))))


(defmethod bitmap ((node node))
  (ash 1 (bitpos node)))


(defmethod set-cpu-load (node)
  (setf (cpu-load node)
        (1+ (loop
               :for m :across (other-members node)
               :when m
               :sum (cpu-load m)))))


(defmethod short-id ((node node))
  (short-id (pkey node)))

(defmethod short-id (x)
  (let* ((str (vec-repr:hex-str x))
         (len (length str)))
    (if (> len 14)
        (concatenate 'string (subseq str 0 7)
                     ".."
                     (subseq str (- len 7)))
        str)))

(defmethod print-object ((node node) out-stream)
  (format out-stream "#<NODE ~A>" (short-id node)))

