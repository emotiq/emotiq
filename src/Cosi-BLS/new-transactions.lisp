;;;; new-transactions.lisp

(in-package :cosi/proofs/newtx)




;;;; Utilities

;; Later, probably these should be moved elsewhere, i.e., to a central
;; utilities module!  -mhd, 6/4/18

(defmacro development-type-checks (type &rest objects)
  "In #+development do a runtime type check on each element in
OBJECTS. Arg TYPE is implicitly quoted (not evaluated)."
  #-development (declare (ignore type objects))
  #-development `(progn)
  #+development `(progn 
                   ,@(loop for object in objects 
                           collect `(check-type ,object ,type))))





;;;; Transaction Types

;;; A `transaction' in Emotiq Lisp software is implemented an instance of class
;;; TRANSACTION. The Lisp type of an instance of this class is TRANSACTION. In
;;; the following discussion, however, the word "type" is used in its generic
;;; meaning, not Lisp/programming-language sense.

;;; Emotiq has six types of transactions:
;;;
;;;   - Spend transaction to transfer tokens, with two subtypes
;;;     - cloaked
;;;     - uncloaked
;;;   - Collect transaction for collecting block fees
;;;   - Stake transaction for sending tokens into PoS escrow
;;;   - Unstake transaction for withdrawing tokens from PoS escrow
;;;   - Slash trasaction to request punishment for a cheating node
;;;   - Disperse transaction to disperse tokens from a cheating node escrow between validators

;;; A transaction instance indentifies its type via its transaction-type slot,
;;; which should be set near the beginning of the life of the transaction
;;; initialized to one of the following valid values, which in turn determine
;;; valid values for other slots of a transaction.  A `transaction type' is
;;; represented as a Lisp keyword. The defined types so far are:
;;;
;;;   :spend -- for an uncloaked spend transaction
;;;   :spend-cloaked -- for a cloaked spend transaction
;;;   :collect -- for a collect transaction




;;;; Transaction Objects



;;; When you use a transaction (TX), you always completely consume its
;;; output.  Any amount not sent to some other party is conventionally
;;; sent back to the sender's same address, which is then referred to
;;; as a so-called "change address".

(defclass transaction ()
  ((transaction-type
    :reader transaction-type :initarg :transaction-type :initform :spend)

   ;; [First put outputs, since they start out at first unspent until a
   ;; later transaction arrives with inputs authorized to redeem them.]

   ;; sequence of transaction-output structures (see below)
   (transaction-outputs :reader transaction-outputs :initarg :transaction-outputs)

   ;; sequence of transaction-input structions (see below)
   (transaction-inputs :reader transaction-inputs :initarg :transaction-inputs)


   (lock-time :initform 0))) ; not yet used, planned for later use ala Bitcoin

;; ---!!! TODO FIX: we currently have variant of segregating signature
;; ---!!! ("witness") data ala Bitcoin Segwit soft fork.  We are currently not
;; ---!!! including signature data in the regular hash of transaction data that
;; ---!!! gets signed (see serialize-transaction below). However, we need to
;; ---!!! follow through with the other side of "segwit", that is, to create
;; ---!!! "witness hash" (a "WTXID") consisting of both the transaction and
;; ---!!! signature data.  In Bitcoin under SegWit, the WTXID is used to compute
;; ---!!! the Witness merkle root, which is put in an output of the coinbase
;; ---!!! transaction of the block. In Bitcoin it is held in an output of the
;; ---!!! Coinbase transaction, but that was only done for compatibility: they
;; ---!!! would have put it in the block if they could have done so compatibly,
;; ---!!! but putting it in an output of the coinbase transaction was the next
;; ---!!! best thing.  Decide whether to keep or discard the actual signature
;; ---!!! data as opposed to its hash (or possibly to have that be optional per
;; ---!!! node).  -mhd, 6/8/18



(defmethod transaction-id ((tx transaction))
  "Get hash of this transaction, which also uniquely* identifies it, as hash:hash/256 instance."
  (let ((message (serialize-transaction tx)))
    (bev-vec (hash-for-transaction-id message))))

;; ---!!! * "uniquely", assuming certain rules and conventions are
;; ---!!! followed to ensure this, some of which are still to-be-done;
;; ---!!! see notes at serialize-transaction -mhd, 6/5/18
  





;;; Classic Standard Bitcoin Transaction: Pay-to-Pubkey-Hash (P2PKH)
;;;
;;; Locking script on transaction output:
;;;
;;;   scriptPubKey: OP_DUP OP_HASH160 <pubKeyHash> OP_EQUALVERIFY OP_CHECKSIG
;;;
;;; Unlocking script on transaction input:
;;;
;;;   scriptSig: <sig> <pubKey>

;; Source:
;; https://en.bitcoin.it/wiki/Script#Standard_Transaction_to_Bitcoin_address_.28pay-to-pubkey-hash.29


;;; Newer Standard Bitcoin Transaction: Pay-to-Witness-Public-Key-Hash (P2WPKH)
;;;
;;; The newer transaction uses BIP 141 "Segregated Witness" (SegWit).
;;;
;;; 

(defclass transaction-output ()
  ((tx-out-public-key-hash              ; like the Bitcoin address
    :reader tx-out-public-key-hash
    :initarg :tx-out-public-key-hash)
   (tx-out-lock-script               ; what Bitcoin calls scriptPubKey
    :reader tx-out-lock-script
    :initarg :tx-out-lock-script)

   ;; for uncloaked spend transaction or for collect transaction: amount
   (tx-out-amount              ; integer number of smallest coin units
    :reader tx-out-amount
    :initarg :tx-out-amount)

   ;; for cloaked spend transaction: proof, message
   (tx-out-proof
    :reader tx-out-proof
    :initarg :tx-out-proof)
   (tx-out-message
    :reader tx-out-message
    :initarg :tx-out-message)))

(defclass transaction-input ()
  ((tx-in-id :reader tx-in-id :initarg :tx-in-id)
   (tx-in-index :reader tx-in-index :initarg :tx-in-index)
   (tx-in-unlock-script                  ; what Bitcoin calls scriptSig
    ;; ^-- we create a list of args to pass to the lock script
    :reader tx-in-unlock-script
    :initarg :tx-in-unlock-script)
  
   ;; direct pointers: in memory only: a local cache, basically
   (%tx-in-public-key :initarg :%tx-in-public-key :accessor %tx-in-public-key)
   (%tx-in-signature :initarg :%tx-in-signature :accessor %tx-in-signature)))
  


(defvar *transaction-scripts* '()
  "An a-list of the form
    ((name . (arglist . body)) ...)")

(defun get-locking-script (name)
  "If found, returns a list of the form (NAME FUNCTION-NAME (ARGS...))"
  (assoc name *transaction-scripts*))

(defun make-locking-script (name)
  (let ((new (list name (list '()))))
    (push new *transaction-scripts*)
    new))

(defun get-or-make-locking-script (name)
  (or (get-locking-script name)
      (make-locking-script name)))



(defun get-unlocking-script (name)
  (get-locking-script name))

;; For now, locking and unlockin scripts are the same below this level, but keep
;; the distinction higher up for documentation, and maybe enforce some day.
  
  

(defun define-script (name arglist body)
  (let* ((entry (get-or-make-locking-script name))
         (arglist-and-body (cdr entry)))
    (setf (first arglist-and-body) (copy-tree arglist))
    (setf (cdr arglist-and-body) (copy-tree body))))

(defmacro def-script (name arglist &body body)
  `(progn
     (define-script ',name ',arglist ',body)
     ',name))

(defmacro def-locking-script (name arglist &body body)
  `(def-script ,name ,arglist ,@body))

(defmacro def-unlocking-script (name arglist &body body)
  `(def-script ,name ,arglist ,@body))


(defvar *script-ops* '()
  "An a-list of the form
     ((name op-dispatch-function) ...)")

(defun get-script-op (name)
  "If found, returns a list of the form (NAME OP-FUNCTION-NAME)"
  (assoc name *script-ops*))

(defvar *script-vars* '()
  "An a-list of the form
     ((name var-value-fetch-function) ...)")

(defmacro def-script-var (name fetch-function-name)
  `(progn
     (push '(,name ,fetch-function-name)
           *script-vars*)
     ',name))


;;; A basic Lispy evaluator:
;;;
;;;   EVAL (expr X, environment N)
;;;     if X is a literal, return its literal value;
;;;     if X is a symbol, return (lookup-symbol X N);
;;;     ...
;;;     Finally: X is a list (fn . args): (APPLY fn args)
;;;
;;;   APPLY (function F, arguments Args)
;;;     if F is built in, return built-in interpretation of (F . Args)
;;;     let N = (look-up-bindings F)
;;;     let B = (look-up-body F)
;;;     return (eval B N)

;;; EVAL-X: evaluate EXPR, in the Lispy script language, with variable BINDINGS
;;; in effect to determine the value of free variables.  BINDINGS is an a-list
;;; of the form ((variable-symbol . value) ...).
;;;
;;; A few examples
;;; (eval-x '(list 1 2 3 foo 5) '((foo . 4))) => (1 2 3 4 5)
;;; (eval-x '(and (= (* a b) c) (or x y)) '((a . 1) (b . 2) (c . 2) (x . nil) (y . 13))) => 13

(defun eval-x (expr bindings)
  (cond
    ((atom expr)
     (case expr
       ((t) t)
       ((nil) nil)
       (otherwise
        (cond
          ((symbolp expr)
           (let ((entry? (assoc expr bindings)))
             (if entry?
                 (cdr entry?)
                 (eval-x-error 'unbound-symbol expr))))
          ((or (numberp expr) (stringp expr))
           expr)
          (t
           (eval-x-error 'strange-atom expr))))))
    ((atom (cdr expr))
     (eval-x-error 'illegal-dotted-list expr))
    (t
     (case (first expr)
       ((quote) (if (cdr (rest expr))
                    (eval-x-error 'too-many-args-to-quote expr)
                    (second expr)))
       ((not) (let ((rest (rest expr)))
                (if (not (and rest (consp rest)))
                    (eval-x-error 'bad-args-to-not (rest expr))
                    (not (eval-x (first rest) bindings)))))
       ((and) (loop with v
                    for e in (rest expr)
                    while (setq v (eval-x e bindings))
                    finally (return v)))
       ((or) (loop for e in (rest expr) thereis (eval-x e bindings)))
       ((if) (let ((last (last expr)) length)
               (unless (atom (cdr last))
                 (eval-x-error 'bad-if-form-non-list expr))
               (setq length (length expr))
               (unless (member length '(3 4))
                 (eval-x-error 'bad-if-form-weird-length length expr))
               (if (eval-x (second expr) bindings) 
                   (eval-x (third expr) bindings)
                   (and (eql length 4) (eval-x (fourth expr) bindings)))))
       ((+ - * = /= > >= < <=)
        (apply (first expr) 
               (loop for e in (rest expr) 
                     as v = (eval-x e bindings)
                     unless (numberp v)
                       do (eval-x-error
                           'bad-non-numeric-arg-to-numeric-operator
                           (first expr) v expr)
                     collect (eval-x v bindings))))
       ((/) (let* ((args (rest expr)) (length (length args)))
              (if (> length 2)
                  (eval-x-error 'n-arg-divide-not-allowed length expr)
                  (let* ((v-1 (eval-x (first args) bindings))
                         (v-2 (eval-x (second args) bindings)))
                    (unless (and (numberp v-1) (numberp v-2))
                      (eval-x-error
                       'bad-non-numeric-arg-to-numeric-operator
                       (first expr) 
                       (if (numberp v-1) v-2 v-1)
                       expr))
                    (if (= v-2 0)
                        (eval-x-error 'divide-by-zero expr)
                        (/ v-1 v-2))))))
       ((list) (loop for arg in (rest expr)
                     as v = (eval-x arg bindings)
                     collect v))
       ;; otherwise, assume function to apply
       (otherwise
        (let* ((function-name (first expr))
               (args (loop for arg in (rest expr)
                           collect (eval-x arg bindings))))
          (apply-x function-name args bindings)))))))

(defun apply-x (function-name args bindings)
  (let ((script? (get-locking-script function-name)))
    (if script?
        (destructuring-bind (name vars body)
            script?
          (declare (ignore name))
          (loop with new-bindings = bindings
                for var in vars
                as arg in args
                do (setq new-bindings (acons var arg new-bindings))
                finally (return (eval-x body new-bindings))))
        (let ((script-op? (get-script-op function-name)))
          (if script-op?
              (destructuring-bind (name op-fn) 
                  script-op?
                (declare (ignore name))
                (apply op-fn args))
              (eval-x-error
               'undefined-function-or-script-op
               function-name))))))

(defun eval-x-error (type &rest additional-args)
  (warn "Error in eval-x of type ~a: additional info: ~s"
        type additional-args)
  (fail-script))
    
    


;;; Example spend transaction:
;;;
;;;   First, there's a UTXO like so to Alice's account:
;;;
;;;   transaction
;;;     transaction id: abc123....
;;;     [transaction output]
;;;       tx-out-public-key-hash: def456...
;;;       tx-out-amount: 50
;;;       tx-out-lock-script:
;;;         (public-key signature)
;;;         (and (public-key-equal-verify public-key ^tx-public-key-hash)
;;;              (check-signature public-key signature))
;;;     [transaction input]
;;;       ...
;;;
;;;  Then, there's a transaction that spends the UTXO:
;;;
;;;   transaction
;;;     transaction id: bcd234....
;;;     [transaction output]
;;;       ...
;;;     [transaction input]
;;;       tx-in-id: abc123....
;;;       tx-in-index: 0
;;;       tx-in-unlock-script:
;;;         (^tx-public-key ^tx-signature)

;; These need better logic for set up; 2nd args ignored!!
(def-script-var ^tx-public-key-hash ignore)
(def-script-var ^tx-public-key ignore)
(def-script-var ^tx-signature ignore)
(def-script-var ^current-epoch ignore)
(def-script-var ^initial-epoch-of-stake ignore)
(def-script-var ^n-epochs-until-unstake ignore)


(defvar *transaction-script-catch-tag* (list nil)
  "An arbitrary unique value used as the tag to throw out of a
   transaction script.")

(defvar *in-transaction-script-context* nil)

(defvar *current-script-op* nil)

(defun require-transaction-script-context ()
  (unless *in-transaction-script-context*
    (error "Must execute in a Transaction-Script context, can't continue.")))

(defun require-transaction-script-op-context ()
  (when (null *current-script-op*)
    (error "Must execute in a Transaction-Script-OP context, can't continue.")))
    

(defmacro def-script-op (name arglist &body body)
  (let* ((op-dispatch-function-name
           (intern (format nil "OP-~a" NAME)))
         (entry
           (list name op-dispatch-function-name)))
    `(progn
       (defun ,op-dispatch-function-name ,arglist
         (require-transaction-script-context)
         (let ((*current-script-op* ',name))
           ,@body))
       (unless (equal (get-script-op ',name) ',entry)
         (push ',entry *script-ops*))
       ',name)))


(defmacro in-transaction-script-context (&body body)
  `(let ((*in-transaction-script-context* t))
     (catch *transaction-script-catch-tag*
       (if (progn ,@body)
           t
           nil))))

(defvar *current-transaction* nil)

(defmacro in-transaction ((transaction) &body body)
  `(let* ((*current-transaction* ,transaction))
     ,@body))

(defun fail-script ()
  (cond
    (*current-transaction*
     (require-transaction-script-context)
     (warn "Failure in script.")        ; bind current script?
     (throw *transaction-script-catch-tag*
       nil))
    (t
     (cerror
      "Continue regardless (presumably in a REPL)"
      "Not in a Transaction-Script context, as required."))))

  
(defparameter *debugging-chainlisp* nil
  "When true, fail-script-op signals a continuable error on a failed
  script operation instead of calling WARN.")


(defun fail-script-op ()
  (cond
    (*current-script-op*
     (require-transaction-script-op-context)
     (if *debugging-chainlisp*
         (cerror "Continue" "Failure in script op ~a" *current-script-op*)
         (warn "Failure in script op ~a" *current-script-op*))
     (throw *transaction-script-catch-tag*
       nil))
    (t
     ;; Allow going off into the wild blue yonder, since this makes it
     ;; very convenient to run in a REPL and simply get back to top
     ;; level.  If deep down in the stack, it may not work out.
     (cerror
      "Continue regardless (presumably in a REPL)"
      "Not in a Transaction-Script-OP context, as required."))))


(defparameter *minimum-transaction-fee* 10)

(defun transaction-fee-too-low-p (transaction-fee)
  (< transaction-fee *minimum-transaction-fee*))               ; ---!!! review!!


(defun validate-transaction (transaction)
  "Validate TRANSACTION, and either accept it (by returning true) or
   reject it by returning false (nil).  This is for transactions
   transmitted via gossip network.  The following checks are done:

   (Items in square brackets ([]) are not yet implemented and may be somewhat in doubt.)

   - Reject if transaction-inputs or transaction-outputs are nil.

   [- Reject if any output amount or the total is outside the legal money range.]

   - Reject if this appears to be a coinbase transaction (created by block creator only).

   - Reject if we have a matching transaction in the mempool or in a block of the blockchain.

   - For each input, if the referenced output exists in any other tx in the mempool, reject this transaction.

   - For each input, if the referenced output does not exist or has already been spent, reject this transaction.

   - Reject if the sum of input values < sum of output values.

   - Reject if transaction fee (= sum of input values minus sum of output values) would be too low.

   - Reject if input missing witness data (i.e., transaction has not
     been signed, i.e., signature and public-key slots are unbound).

   - For each input, apply the unlock script function to no args,
     check that the result is a valid arg list (typically the
     list (signature public-key)), then apply the result to the lock
     script function of the output of the transaction referenced by
     the input. If any of these scripts do not succeed (indicated by
     returning nil), reject this transaction.

   - Add the transaction to the mempool."

  (when (null (transaction-inputs transaction))
    (warn "Transaction with no inputs rejected")
    (return-from validate-transaction nil))
  (when (null (transaction-outputs transaction))
    (warn "Transaction with no outputs rejected")
    (return-from validate-transaction nil))

  (let ((txid (transaction-id transaction)))
    (when (find-transaction-per-id txid t)
      ;; This is sort of a subcase of a double-spend attempt: so give
      ;; a message to that effect. This corresponds to above rule
      ;; 'Reject if we have a matching transaction in the mempool or
      ;; in a block of the blockchain'
      (warn "Double-spend attempt: a transaction with this ID (~a) is already in the mempool or on the blockchain. Rejected."
            (txid-string txid))
      (return-from validate-transaction nil))
    (loop with succeed-p
          for tx-in in (transaction-inputs transaction)
          as id = (tx-in-id tx-in)
          as index = (tx-in-index tx-in)
          as input-tx = (find-transaction-per-id id t)
          as input-tx-outputs 
            = (cond
                ((null input-tx)
                 (warn
                  "~%Input transaction with presumed UTXO does not exist: no transaction with TxID ~a found in the mempool or on the blockchain." 
                  (txid-string id))
                 (return nil))
                (t (transaction-outputs input-tx)))
          as utxo
            = (let ((tx-out
                      (or (nth index input-tx-outputs)
                          (unless (tx-ids= id (transaction-id input-tx))
                            (warn "TX ~a output does not match input spec [~a/~d]."
                                  input-tx id index)
                            (return nil))
                          ;; debugging:
                          (error "TX ~a output [~a/~d] lost - likely programming error"
                                 input-tx id index))))
                (when (double-spend-tx-out-p id index t)
                  (warn "Double-spend attempt: TxID: ~a. Index: ~a. Rejected."
                        (txid-string id) index)
                  (return nil))
                tx-out)
          as input-subamount = (tx-out-amount utxo)
          when (coinbase-transaction-input-p tx-in)
            do (warn "Transaction with coinbase input - rejected")
               (return nil)
          when (or (not (slot-boundp tx-in '%tx-in-signature))
                   (null (%tx-in-signature tx-in))
                   (not (slot-boundp tx-in '%tx-in-public-key))
                   (null (%tx-in-public-key tx-in)))
            do (warn "Transaction missing witness data. Forgot to sign? Rejected.")
               (return nil)                 
          do (setq succeed-p (run-chainlisp-script transaction tx-in utxo))
          when (not succeed-p)
            do (return nil)
          sum input-subamount into sum-of-inputs
          finally (let ((sum-of-outputs
                          (loop for tx-out in (transaction-outputs transaction)
                                sum (tx-out-amount tx-out))))
                    (when (< sum-of-inputs sum-of-outputs)
                      (warn "TX sum of inputs values < sum of output values. Rejected.")
                      (return nil))
                    (when (transaction-fee-too-low-p 
                           (- sum-of-inputs sum-of-outputs))
                      (warn "TX transaction fee ~d would be too low. Minimum = ~d. Rejected."
                            (- sum-of-inputs sum-of-outputs)
                            *minimum-transaction-fee*)
                      (return nil))
                    (emotiq:note "~%Node ~a declares Successful transaction ~a~%  TxID = ~a~%"
                                 (cosi-simgen:current-node)
                                 transaction
                                 (txid-string (transaction-id transaction)))
                    (add-transaction-to-mempool transaction)
                    (return t)))))

(defun run-chainlisp-script (transaction transaction-input utxo)
  (in-transaction (transaction)
    (in-transaction-script-context
      (let* ((lock-script (tx-out-lock-script utxo))
             (unlock-script (tx-in-unlock-script transaction-input))
             (public-key-hash (tx-out-public-key-hash utxo)))
      (when (null lock-script)
        (fail-script))
      (when (null unlock-script)
        (fail-script))
      (let* ((unlock-script-function-name (first unlock-script))
             (unlock-args
               (apply-x
                unlock-script-function-name
                '()
                `((^signature . ,(%tx-in-signature transaction-input))
                  (^public-key . ,(%tx-in-public-key transaction-input)))))
             (lock-script-function-name (first lock-script)))
        (apply-x
         lock-script-function-name
         unlock-args

         ;; Establish and handle implicit/global
         ;; parameters by putting them in the bindings
         ;; list, and then referring to them as from
         ;; script as script vars. Kludge this for now --
         ;; automate soon.

         `((^tx-public-key-hash
            . ,public-key-hash))))))))

;; Above rules/logic/documentation loosely based on Bitcoin "tx"
;; documentation here:
;; https://en.bitcoin.it/wiki/Protocol_rules#.22tx.22_messages





;;; SERIALIZE-TRANSACTION: emit all the fields that should be hashed in a
;;; transaction. Note: for now [Mar 26, 2018], we just hash all inputs and all
;;; outputs. We must review and refine later to have this be parameterized to
;;; hash parts depending on what the situation calls for.

;;; In Bitcoin, there is a SIGHASH type set of bits with meanings as follows:
;;;
;;;   flag val    what to sign
;;;   all  0x01   all inputs, all outputs
;;;   none 0x02   all inputs, no outputs
;;;   one  0x03   all inputs, one output w/same index as signed input
;;;
;;; (So, in effect, what we now do below is the all case.)

;;; The parts of an output (UTXO) hashed are
;;;
;;;   public key hash
;;;   amount
;;;   lock script
;;;
;;; The parts of an input hashed are
;;;
;;;   ID (the hash of a previous transaction input)
;;;   index

;;; ---***!!! FIX: coinbase transactions could rarely result duplicate
;;; ---***!!! TXID. Bitcoin BIP 30 made those not allowed (via a
;;; ---***!!! network rule)
;;; ---***!!! https://github.com/bitcoin/bips/blob/master/bip-0030.mediawiki
;;; ---***!!! NB: for coinbase transactions, do as in Bitcoin BIP 34,
;;; ---***!!! which required coinbase transactions to include the
;;; ---***!!! height of the block they were mining in to their
;;; ---***!!! transaction data, so that coinbase transactions could be
;;; ---***!!! different.  This avoids the possibility of duplicate
;;; ---***!!! TXIDs. https://github.com/bitcoin/bips/blob/master/bip-0034.mediawiki#Motivation
;;; ---***!!! -mhd, 6/4/18
;;;
;;; Normally, you do not have block info - most normal transactions
;;; are created in wallet client.  Think about how to implement this
;;; feature. Only needed for case of creating a coinbase transaction,
;;; created by miners/minters when a block is created.

(defun serialize-transaction (transaction)
  (with-output-to-string (out)
    (flet ((emit (x)
             (format out "~a " x)))
      (loop for tx-out in (transaction-outputs transaction)
            as hash = (tx-out-public-key-hash tx-out) ; base58 string
            as amt = (tx-out-amount tx-out)           ; integer
            as script = (tx-out-lock-script tx-out)   ; chainlisp form
            do (emit hash) (emit amt) (emit script))
      (loop for tx-in in (transaction-inputs transaction)
            as id = (tx-in-id tx-in)        ; string of 0's (coindbase case) or octet vector (not good to emit)
            as id-hex-string = (hex-str id) ; better: emit this
            as index = (tx-in-index tx-in)  ; integer
            do (emit id-hex-string) (emit index)))))

(defun serialize-current-transaction ()
  (serialize-transaction *current-transaction*))



(defun hash-for-transaction-id (message)
  "Hash MESSAGE, the serialization returned by serialize-transaction."
  (hash:hash/256 message))





(def-script-op public-key-equal-verify (public-key public-key-hash)
  (cond
    ((not (typep public-key 'pbc:public-key))
     (emotiq:note "~%Arg public-key (~s) is not of correct type: ~s~%"
             public-key 'pbc:public-key)
     (fail-script-op))
    ((not (stringp public-key-hash))
     (emotiq:note "~%Public-key-hash ~s not a string but string expected~%"
             public-key-hash)
     (fail-script-op))
    (t
     (let* ((public-key-hash-from-public-key
              (cosi/proofs:public-key-to-address public-key))
            (l1 (length public-key-hash))
            (l2 (length public-key-hash-from-public-key)))
       (cond
         ((not (stringp public-key-hash-from-public-key))
          (emotiq:note "~%Public-key hash ~s derived from public-key ~s not a string but string expected~%"
                  public-key-hash-from-public-key public-key)
          (fail-script-op))
         ((not (= l1 l2))
          (emotiq:note "~%Public key hash ~s length ~s ~%  not same as public key ~s hash ~s length ~s~%"
                  public-key-hash l2
                  public-key public-key-hash-from-public-key l2)
          (fail-script-op))
         ((string= public-key-hash public-key-hash-from-public-key)
          t)
         (t
          (fail-script-op)))))))

(def-script-op check-signature (public-key signature)
  (check-hash
   (hash:hash/256 (serialize-current-transaction))
   signature public-key))


(def-locking-script script-pub-key (public-key signature)
  (and (public-key-equal-verify public-key ^tx-public-key-hash)
       (check-signature public-key signature)))

(def-locking-script script-pub-key-cloaked (public-key signature proof message)
  (and (public-key-equal-verify public-key ^tx-public-key-hash)
       (check-signature public-key signature)
       ;; proof: a "bulletproof" (cloaked amount) giving the amount of tokens to
       ;; transfer

       ;; message: a message encrypted with recipient's public key containing
       ;; the amount and gamma
       ))

;;; foo asdf aslk


;;;
;;;

(def-unlocking-script script-sig ()
  (list ^public-key ^signature))





;;;; Genesis Block Creation

;;; In Bitcoin the first transaction of a block is created by a miner, and it's
;;; a special transaction with one input that's very wacky called a coinbase
;;; transaction with a so-called coinbase input. While every other transaction
;;; on the blockchain has as input a reference to a previous transaction, a
;;; coinbase transaction refers to a "null" transaction.

;;; Coinbase transaction are the main way coin gets into the system in
;;; Bitcoin. The other way is through transaction fees. In Bitcoin the miner who
;;; creates the block creates the coinbase transaction and is awarded the mining
;;; fee. The coinbase transaction issues one output to one address and that is
;;; to the miner's "wallet", i.e, their account (i.e., designated by their
;;; public key hash), and that is in the amount of the current block reward,
;;; which famously know gets halved every four years (currently [2018] 12.5
;;; BTC). So that's mining, and Emotiq does not feature mining.

;;; In addition to the mining reward, the coinbase transaction includes fees for
;;; the miner. That's a way for Bitcoin to pay a little more besides mining
;;; fees, and will be the only way miners get paid after miner awards approach
;;; and eventually reach zero.

;;; Now, since we, Emotiq, do not feature mining, what do we do at block
;;; creation time with the first transaction. I.e., how does the main amount of
;;; coin get into the system? Here is the answer.

;;; The Emotiq Whitepaper states that an initial supply of 10^9 coins will be
;;; created when EMTQ mainnet is launched. So, the genesis block would contain
;;; transactions paying 10^9 to one address (TBD), and from there onto others as
;;; desired.


;;;; Coin and Token Units

;;; NB: while the Emotiq Whitepaper talks of EMTQ tokens and EMTQ child tokens,
;;; this is probably simpler than what's there and probably does not match the
;;; level of sophistication implied there.

;;; At a later point we might switch to the term "token", but for now we're
;;; using coin", as follows.  

;;; A `coin unit' or more simply `unit' or `coin' (and plural versions with
;;; `units' or `coins') all refer the fully valued coin in the cyrpto currency
;;; here implemented.

;;; A `coin subunit' or `subunit' (plural `subunits') all refer to the smallest
;;; fraction of a coin unit.

;;; A `coin amount' or `amount' is a number of coin subunits.

;;; When expressing a number of units messages with users, it is fine to use
;;; floats or ratios, but as a coding convention internally we use integer math
;;; only, via the following constants.

(defmacro initial-coin-units ()
  '#.(expt 10 9)) ; 1,000,000,000

(defmacro subunits-per-coin ()
  '#.(expt 10 8)) ; 100,000,000 (same as Satoshi)

;; Review: consider an increase -- significantly higher in, e.g., Ethereum.


;;; COIN-AMOUNT: (macro) given a number UNITS of units of any kind (integer,
;;; float, ratio, integer), convert it to the closest integer number of
;;; subunits. This can only be used in cases where the remainder, if nonzero,
;;; would be a fraction of a subunit, would therefore typically be considered
;;; too insignificant to worry about.

(defmacro coin-amount (units)
  (if (integerp units)           ; optimization of literal constant integer case
      `(* ,units (subunits-per-coin))
      `(round (* ,units (subunits-per-coin)))))



;;; INITIAL-TOTAL-COIN-AMOUNT: with no args gives the total number of subunits
;;; of coin to be initially allocated in the genesis block.

(defmacro initial-total-coin-amount ()
  `(* (initial-coin-units) (subunits-per-coin)))


(defparameter *minimum-spend-amount* 0) ; ---!!! review!!
(defparameter *maximmum-spend-amount* (initial-total-coin-amount))

(defun in-legal-money-range-p (amount)
  (and (>= amount *minimum-spend-amount*)
       (<= amount *maximmum-spend-amount*)))




(defparameter *coinbase-fee-in-coin-units* (/ 5 8)
  "The default coinbase fee. This is the amount awarded to the creator of each
  new ledger block.  This can be set to either an integer, float, or ratio. Use
  function get-coinbase-transaction-fee-amount to scale and round this to get it
  as a coin amount.")

(defun get-coinbase-transaction-fee-amount ()
  (coin-amount *coinbase-fee-in-coin-units*))




;;; The next two values to initialize the id and index of the input for a coinbase transaction
;;; are settings are imitative of Bitcoin, which similarly has
;;; all 0 bits for Tx ID, all 1 bits for Tx index. These slots do not serve
;;; their normal function, so these values are arbitrary. These two ARE part
;;; of the hash of the transaction.

(defvar *initial-coinbase-tx-in-id-value*
  "0000000000000000000000000000000000000000000000000000000000000000")

(defvar *initial-coinbase-tx-in-index-value*
  -1)

(defun coinbase-transaction-input-p (transaction-input)
  (with-slots (tx-in-id tx-in-index) transaction-input
    (and (equal tx-in-id *initial-coinbase-tx-in-id-value*)
         (equal tx-in-index *initial-coinbase-tx-in-index-value*))))

(defun make-coinbase-transaction-input ()
  (make-instance
   'transaction-input
   :tx-in-id *initial-coinbase-tx-in-id-value*
   :tx-in-index *initial-coinbase-tx-in-index-value*

   ;; These are also not going to be used as for a normal transaction.  The rest
   ;; are NOT part of the hash of the transaction.
   :tx-in-unlock-script nil

   :%tx-in-public-key nil
   :%tx-in-signature nil))

(defun make-coinbase-transaction-output (public-key-hash amount)
  (make-instance
   'transaction-output
   :tx-out-public-key-hash public-key-hash
   :tx-out-amount amount
   :tx-out-lock-script (get-locking-script 'script-pub-key)))

;; (get-locking-script 'script-pub-key) under consideration. Needs work.



(defun make-genesis-transaction (public-key-hash)
  (make-and-maybe-sign-transaction
   (list (make-coinbase-transaction-input))
   (list (make-coinbase-transaction-output
          public-key-hash 
          (initial-total-coin-amount)))))



(defun make-coinbase-transaction (public-key-hash)
  (make-and-maybe-sign-transaction
   (list (make-coinbase-transaction-input))
   (list (make-coinbase-transaction-output
          public-key-hash
          (get-coinbase-transaction-fee-amount)))))




;;;; Transaction Contexts


(defvar *transaction-context*)

(defclass transaction-context ()
  ((tx-in-utxo :initarg :tx-in-utxo :reader tx-in-utxo)))

(defmacro with-transaction-context ((context &optional options) &body body)
  (declare (ignore options))            ; reserved for later
  `(let* ((*transaction-context* ,context))
     ,@body))

;; Should an attempt be made to detect reentrant usage and signal an
;; error?



(defun expect-transaction-context ()
  "Used for development-only testing. This may NOT be relied upon at run time in
production to stop execution if there is no transaction context. When called in
development, this signals a continuable error if there is no transaction."
  (when (not (boundp '*transaction-context*))
    (cerror "continue anyhow" "not in a transaction context")))




(defun tx-ids= (tx-id-1 tx-id-2)
  "Return true if transaction IDs are equal. TX-ID-1 and TX-ID-2 must
   transaction ID's, which are represented as hashes, i.e., as
   instances of hash:hash."
  (equalp tx-id-1 tx-id-2))

(defmacro do-blockchain ((block-var) &body body)
  "Iterate over the blocks of the blockchain (i.e., the value of
  cosi-simgen:*blockchain*) beginning the most recent minted and
  ending with the genesis block, with BLOCK-VAR bound during each
  iteration to the block."
  `(loop for ,block-var in cosi-simgen:*blockchain*
         do (progn ,@body)))

(defmacro do-transactions ((tx-var blk) &body body)
  "Bind each transaction of block BLK to TX-VAR in order from latest
   spent to earliest spent around the execution of BODY. It's possible
   to return using RETURN although RETURN-FROM to a lexical tag is
   recommended as the most reliable and clear method."
  `(loop with reversed-transactions
           = (reverse (cosi/proofs:block-transactions ,blk))
         for ,tx-var in reversed-transactions
         do (progn ,@body)))

(defmacro do-all-transactions ((tx-var &optional block-var?) &body body)
  "Iterate over all transactions of the blockchain from latest spent
   to earliest spent around the exectution of BODY. To exit early you
   must return using RETURN-FROM to a lexical tag."
  (LET ((block-var (or block-var? '#:block)))
    `(do-blockchain (,block-var)
       (do-transactions (,tx-var ,block-var)
         ,@body))))

;; Note that the order from latest to earliest spent is currently
;; relied upon by, e.g., double-spend-tx-out-p to detect double-spend
;; attempts. -mhd, 6/17/18

;; ---!!! TODO: must improve architecture to avoid this
;; ---!!! consing/copying inefficiency! -mhd, 6/17/18



(defun list-all-transactions ()
  (let ((result '()))
    (do-all-transactions (tx)
      (push tx result))
    result))


(defun find-transaction-per-id (id &optional also-search-mempool-p)
  "Search blockchain for a transaction with TXID matching ID. If
also-search-mempool-p is true, this also searches the current
mempool. This returns the transaction as a first value if found, and
false (nil) otherwise. In the found case, if found in the mempool,
this returns a second value of t; if found in the blockchain, this
returns the block the transaction was found in as a second value."
  (let* ((result-from-mempool?
           (and also-search-mempool-p
                (loop for tx being each hash-value of cosi-simgen:*mempool*
                      when (tx-ids= (transaction-id tx) id)
                        return tx))))
    (if result-from-mempool?
        (values result-from-mempool? t)
        (do-blockchain (block)
          (do-transactions (tx block)
            (when (tx-ids= (transaction-id tx) id)
              (return-from find-transaction-per-id (values tx block))))))))

(defun double-spend-tx-out-p (id index &optional also-search-mempool-p)
  (when also-search-mempool-p
    (loop with transaction-seen-p = nil
          for tx being each hash-value of cosi-simgen:*mempool*
          do (loop for tx-in in (transaction-inputs tx)
                   as tx-in-id = (tx-in-id tx-in)
                   as tx-in-index = (tx-in-index tx-in)
                   when (and (tx-ids= tx-in-id id)
                             (= tx-in-index index))
                     ;; earlier spend block found (double spend attempt)
                     do (return-from 
                         double-spend-tx-out-p
                          (values t :found-in-mempool)))
          when (and (not transaction-seen-p)
                    (tx-ids= (transaction-id tx) id))
            do (setq transaction-seen-p t)
          finally
             ;; Can stop search (not a double-spend) if found
             ;; transaction in mempool, evidently a UTXO.  Must go
             ;; through whole loop, cannot exit -- mempool unordered,
             ;; unlike the blockchain (cf below)
             (when transaction-seen-p
               (return-from double-spend-tx-out-p
                 (values nil :utxo-in-mempool)))))
  (do-all-transactions (tx)
    (loop for tx-in in (transaction-inputs tx)
          as tx-in-id = (tx-in-id tx-in)
          as tx-in-index = (tx-in-index tx-in)
          when (and (tx-ids= tx-in-id id)
                    (= tx-in-index index))
            ;; earlier spend block found (double spend attempt)
            do (return-from 
                double-spend-tx-out-p
                 (values t :found-on-blockchain)) ; as opposed to mempool
               ;; Can stop search (not a double-spend) when reach ID,
               ;; indicating we have an usnpent transaction output
               ;; (UTXO)
               (when (tx-ids= (transaction-id tx) id)
                 (emotiq:note "~%Hey look: the TxID~%  ~a~%is the same as the arg TxID~%  ~a~%Wow!~%"
                         (txid-string (transaction-id tx))
                         (txid-string id))
                 (return-from double-spend-tx-out-p
                   (values nil :utxo-on-blockchain))))))


(defun trace-compare-all-tx-ids (id)
  (emotiq:note "~%Trace:~%")
  (do-blockchain (blk)
    (do-transactions (tx blk)
      (emotiq:note "~%TX id = ~s   vs~%  ~s [~a]"
              (transaction-id tx) id
              (tx-ids= (transaction-id tx) id))))
  (emotiq:note "~&end trace~%"))
    



;;; The following is from the OmniLedger paper. We would like to incorporate
;;; this into transaction processing.  This is referred to as "V. B." in the
;;; "Emotiq Yellow Paper" in our repo
;;; online. (https://github.com/emotiq/yellowpaper/blob/master/yellowpaper.md).
;;;
;;;   B. Parallelizing Block Commitments We now show how ByzCoinX parallelizes
;;;   block commitments in the UTXO model by carefully analyzing and handling
;;;   dependencies between transactions. We observe that transactions that do
;;;   not conflict with each other can be committed in different blocks and
;;;   consequently can be safely processed in parallel. To identify conflicting
;;;   transactions, we need to analyze the dependencies that are possible
;;;   between transactions. Let txA and txB denote two transactions. Then, there
;;;   are two cases that need to be carefully handled: (1) both txA and txB try
;;;   to spend the same UTXO and (2) an UTXO created at the output of txA is
;;;   spent at the input of txB (or vice versa). To address (1) and maintain
;;;   consistency, only one of the two tx can be committed. To address (2), txA
;;;   has to be committed to the ledger before txB,i.e., txB has to be in a
;;;   block that depends (transitively) on the block containing txA. All
;;;   transactions that do not exhibit these two properties can be processed
;;;   safely in parallel. In particular we remark that transactions that credit
;;;   the same address do not produce a conflict, because they generate
;;;   different UTXOs To capture the concurrent processing of blocks, we adopt a
;;;   block-based directed acyclic graph (blockDAG) [33] as a data structure,
;;;   where every block can have multiple parents. The ByzCoinX protocol leader
;;;   enforces that each pending block includes only non-conflicting
;;;   transactions and captures UTXO dependencies by adding the hashes of former
;;;   blocks (i.e.,backpointers) upon which a given block’s transactions
;;;   depend. To decrease the number of such hashes, we remark that UTXO
;;;   dependencies are transitive, enabling us to relax the requirement that
;;;   blocks have to capture all UTXO dependencies directly. Instead, a given
;;;   block can simply add backpointers to a set of blocks, transitively
;;;   capturing all dependencies.
;;;
;;; Here is the referenced "blockDAG" paper URL (Inclusive Block Chain
;;; Protocols):
;;;
;;;   http://fc15.ifca.ai/preproceedings/paper_101.pdf




(defun require-blockchain ()
  "Check for at the genesis block, otherwise signal an error."
  (when (null cosi-simgen:*blockchain*)
    (error "Blockchain requires at least 1 block.")))



(defun get-hash-key-of-transaction (transaction)
  "Transactions may be stored in EQUALP hash tables. The mempool, for
  example, is an equalp hash table. This returns a key that is a byte
  vector, i.e., such that it has the property that it may be compared
  using equalp.  Specifically it's the big-endian byte vector
  representing the hash that is the transaction ID of TRANSACTION."
  (transaction-id transaction))



(defun add-transaction-to-mempool (transaction)
  "Add TRANSACTION to the mempool. It is OK to call this when the
   transaction is already in the mempool.  This returns TRANSACTION as its value."
  (let ((key (get-hash-key-of-transaction transaction)))
    (setf (gethash key cosi-simgen:*mempool*) transaction)))

(defun remove-transaction-from-mempool (transaction mempool)
  "Remove TRANSACTION, a transaction instance, from MEMPOOL, and return true if
   there was such an entry or false (nil) otherwise."
  (remhash (get-hash-key-of-transaction transaction) mempool))





(defun make-transaction-inputs (input-specs &key transaction-type)
  "Make transaction inputs for TRANSACTION-TYPE, which defaults to :SPEND."
  (loop with transaction-type = (or transaction-type :spend)
        with unlock-script 
          = (map-transaction-type-to-unlock-script transaction-type)
        for (id index) in input-specs
        collect (make-instance
                 'transaction-input
                 :tx-in-id id
                 :tx-in-index index
                 :tx-in-unlock-script unlock-script)))



;;; MAP-TRANSACTION-TYPE-TO-UNLOCK-SCRIPT: map transaction-type to its
;;; corresponding Chainlisp function name.
;;;
;;; The currently supported types :spend, :spend-cloaked, and :collect all
;;; supply the same function: SCRIPT-SIG, which supplies the public key hash
;;; (a/k/a address) and signature for verification, needed to unlock the value
;;; of the transaction at hand.

(defun map-transaction-type-to-unlock-script (transaction-type)
  (get-unlocking-script 
   (ecase transaction-type
     ((:spend :spend-cloaked :collect) 
      'script-sig))))
  


(defun make-transaction-outputs (output-specs &key transaction-type)
  (loop with transaction-type = (or transaction-type :spend)
        with lock-script
          = (map-transaction-type-to-lock-script transaction-type)
        for (public-key-hash amount) in output-specs
        collect (make-instance
                 'transaction-output
                 :tx-out-public-key-hash public-key-hash
                 :tx-out-amount amount
                 :tx-out-lock-script lock-script)))



(defun map-transaction-type-to-lock-script (transaction-type)
  (get-locking-script 
   (ecase transaction-type
     (:spend 'script-pub-key)
     (:spend-cloaked 'script-pub-key-cloaked)
     (:spend-cloaked 'script-collect))))


(defun check-private-keys-for-transaction-inputs (private-keys tx-inputs)
  (unless (= (length private-keys) (length tx-inputs))
    (warn 
     "Unequal-length private-keys/tx-input hashes~%  ~a~%  length = ~d~%vs.~%  ~a~%  length = ~d.~%Programming error likely!"
     private-keys
     (length private-keys)
     tx-inputs
     (length tx-inputs))))

(defun check-public-keys-for-transaction-inputs (public-keys tx-inputs)
  (unless (= (length public-keys) (length tx-inputs))
    (warn 
     "Unequal-length public-keys/tx-input hashes~%  ~a~%  length = ~d~%vs.~%  ~a~%  length = ~d.~%Programming error likely!"
     public-keys
     (length public-keys)
     tx-inputs
     (length tx-inputs))))


(defun make-and-maybe-sign-transaction (tx-inputs tx-outputs &key skeys pkeys)
  "Create a transaction. Then, if keyword keys is provided non-nil,
the transaction is signed. In the signing case, keys should supply one
private key for each input, either as a single private atomic private
key or singleton list in the case of a single input or as a list of
keys in the case of two or more inputs. Normally, all transactions
should be signed, and only coinbase transactions are not signed."
  (let* ((transaction (make-transaction tx-inputs tx-outputs)))
    ;; Got keys? Sign transaction if so:
    (when skeys
      (sign-transaction transaction skeys pkeys))
    transaction))



(defun make-transaction (tx-inputs tx-outputs)
  "Create a transaction with inputs TX-INPUTS and outpus TX-OUTPUTS."
  (make-instance
   'transaction
   :transaction-inputs tx-inputs
   :transaction-outputs tx-outputs))

(defun sign-transaction (transaction skeys pkeys)
  "Sign transactions with private and public key or keys SKEYS and PKEYS,
   respectively. SKEYS/PKEYS should be supplied as either a single atomic key or
   a singleton list in the case of a single input or as a list of two or more
   keys in the case of two or more inputs. Normally, all transactions should be
   signed, and only coinbase transactions are not signed.  Each resulting
   signature is stored in the %tx-in-signature slot of each input, and note also
   that the signature stores and makes accessible its corresponding public key."
  (with-slots (transaction-inputs) transaction
    (let* ((message (serialize-transaction transaction))
           (private-keys (if (and skeys (atom skeys)) (list skeys) skeys)))
      (check-private-keys-for-transaction-inputs private-keys transaction-inputs)
      (let ((public-keys (if (and pkeys (atom pkeys)) (list pkeys) pkeys)))
        (check-public-keys-for-transaction-inputs public-keys transaction-inputs)
        (loop for tx-input in transaction-inputs
              as private-key in private-keys
              as public-key in public-keys
              as signature = (sign-hash (hash:hash/256 message) private-key)
              do (setf (%tx-in-signature tx-input) signature)
                 (setf (%tx-in-public-key tx-input) public-key))))))
  





(defun account-addresses= (address-1 address-2)
  (string= address-1 address-2))



;;; UTXO-P: search first the mempool and then blockchain exhaustively starting
;;; with the most-recently-created transaction returning true if the output of
;;; TRANSACTION at OUTPUT-INDEX is found to have been spent, and false (nil) if
;;; TRANSACTION itself or the beginning of the blockchain is reached.

(defun utxo-p (transaction output-index)
  (expect-transaction-context)
  (do-blockchain (block)
    (return
      (do-transactions (tx block)
        (return
          (if (eq tx transaction)
              nil
              (loop for txi in (transaction-inputs tx)
                    as index = (tx-in-index txi)
                    when (eql index output-index)
                      return t)))))))



(defun get-utxos-per-account (address)
  "Return the UTXOs for ADDRESS as list of n-tuples of the form

  (TXO PARENT-TX ...)

ADDRESS here is taken to mean the same thing as the public key hash."
  (expect-transaction-context)
  (let ((utxos-so-far '()))
    (do-blockchain (block)
      (do-transactions (tx block)
        (loop for txo in (transaction-outputs tx)
              as public-key-hash = (tx-out-public-key-hash txo)
              as index from 0
              when (and (account-addresses= public-key-hash address)
                        (utxo-p tx index))
                collect (list txo tx)   ; ... addition entries maybe later?
                  into result
              finally (setq utxos-so-far
                            (nconc utxos-so-far result)))))
    utxos-so-far))



(defun get-balance (address)
  "Return the sum of all amounts of all UTXOs of ADDRESS."
  (expect-transaction-context)
  (let ((utxos (get-utxos-per-account address)))
    (loop for (txo) in utxos
          as amount = (tx-out-amount txo)
          collect amount into amounts
          finally (return (apply #'+ amounts)))))





;;;; Printing Utilities


(defun txid-string (transaction-id)
  "Return a string representation of TRANSACTION-ID, a byte vector,
   for display. The result is a lowercase hex string."
  (nstring-downcase (format nil (hex-str transaction-id))))
  


(defun dump-tx (tx &key out-only)
  (emotiq:note "~&  TxID: ~a~%" (txid-string (transaction-id tx)))
  (unless out-only
    (loop for tx-in in (transaction-inputs tx)
          do (emotiq:note "    input outpoint: index = ~a/TxID = ~a~%"
                     (tx-in-index tx-in) (txid-string (tx-in-id tx-in)))))
  (emotiq:note "    outputs:~%")
  (loop for tx-out in (transaction-outputs tx)
        as i from 0
        do (emotiq:note "      [~d] amt = ~a (out to) addr = ~a~%"
                   i (tx-out-amount tx-out) (tx-out-public-key-hash tx-out))))

(defun dump-txs (&key file mempool block blockchain node)
  (flet ((dump-loops ()
           (when block
             (emotiq:note "~%Dump txs in block = ~s:~%" block)
             (do-transactions (tx block)
               (dump-tx tx)))
           (when mempool
             (emotiq:note "~%Dump txs in mempool:~%")
             (loop for tx being each hash-value of cosi-simgen:*mempool*
                   do (dump-tx tx)))
           (when blockchain
             (emotiq:note "~%Dump txs on blockchain:~%")
             (do-blockchain (block)
               (emotiq:note " Dump txs in block = ~s:~%" block)
               (do-transactions (tx block)
                 (dump-tx tx))))))
    (let ((cosi-simgen:*current-node*
            (or node 
                cosi-simgen:*current-node*
                cosi-simgen:*top-node*)))
      (if file
          (with-open-file (*standard-output*
                           file
                           :direction :output :if-exists :supersede)
            (dump-loops))
          (dump-loops)))))



;;;; Waiting for Transactions

(defparameter *wait-for-tx-count-tick-print* ".")
(defparameter *wait-for-tx-count-tick-every* 10)
(defparameter *wait-for-tx-count-message* "Waiting for tx count: ")

(defun wait-for-tx-count (n &key interval timeout node
                                 tick-print tick-every message)
  "Alternate sleeping for a small INTERVAL while waiting for there to
   have been a total of N transaction on the blockchain. INTERVAL
   defaults to about 1/10th of a second, and a user-specified value is
   restricted to a reasonable range > 0 and <= 1 second.  The
   blockchain is with respect to NODE, which can be a specified node,
   and otherwise defaults to the current node, if non-nil, and
   otherwise the top node. A timeout occurs when TIMEOUT, which
   defaults to nil, has been supplied non-nil, in which case it should
   ben an interval in seconds. Then, when that amount of time has
   elapsed, a timeout has been reached, and the function returns.  If
   the count is reached, this returns true; if a timeout occurs, this
   returns nil; otherwise, this does not return, and simply continues
   alternating between checking the count of transactions and sleeping
   for brief intervals."
  (when interval
    (setq interval (max 1 (min 1/1000 interval))))
  (setq tick-print
        (or tick-print
            *wait-for-tx-count-tick-print*))
  (setq message
        (or message
            *wait-for-tx-count-message*))
  (setq tick-every
        (or tick-every
            *wait-for-tx-count-tick-every*))
  (loop with start-ut = (get-universal-time)
        with interval = (or interval 1/10)
        with cosi-simgen:*current-node*
          = (or node 
                cosi-simgen:*current-node*
                cosi-simgen:*top-node*)
        as count                        ; see note!
          = (let ((count-so-far 0))
              (do-all-transactions (tx)
                tx                   ; (ignored, just to gag compiler)
                (incf count-so-far))
              count-so-far)
        as time-through
        initially
           (emotiq:note "~%~a~d" message n)
        when (>= count n)
          return (values t (- (get-universal-time) start-ut))
        when (and timeout
                  (>= (- (get-universal-time) start-ut)
                      timeout))
          return nil
        do (sleep interval)
        when (= (mod time-through tick-every) 0)
          do (princ tick-print)))

;; The counting method is inefficient, since it counts all the way
;; from the beginning each time. Only OK for low-transaction-count
;; early blockchains, OK for now. Improve later! -mhd, 6/22/18





;;;; Getting Transactions for Blocks



(defun transaction-must-precede-p (t1 t2)
  "Predicate on two transactions T1 and T2. True if any input of T2
   refers to T1, meaning T1 must precede T2 on the blockchain."
  (loop with t1-txid = (transaction-id t1)
        for t2-transaction-input in (transaction-inputs t2)
        thereis (tx-ids= t1-txid t2-transaction-input)))

(defun get-transactions-for-new-block (&key max)
  (loop for tx being each hash-value of cosi-simgen:*mempool*
        count t into tx-count
        collect tx into transactions
        finally
           ;; topologically sort transactions:
           (setq transactions (sort transactions #'transaction-must-precede-p))
           (when (and max (> max 0) (> tx-count max))
             ;; Now, if necessary, it's safe to remove from
             ;; back. Reverse the list, then remove from the back
             ;; until down to max, then rereverse the list, leaving
             ;; variable transactions shortened from the back. Note
             ;; that the mempool is not altered (no transactions added
             ;; or removed).
             (let ((rev-txs (nreverse transactions)))
               (loop do (pop rev-txs)
                     while (> tx-count max)
                     do (decf tx-count))
               (setq transactions (nreverse rev-txs))))
           #+development (assert (= (length transactions) tx-count))
           (ac:pr (format nil "~D Transactions" tx-count))
           (return transactions)))  

;; Note: new transactions currently do not use UTXO database, only
;; mempool and blockchain.

;; Checking for double spending is not done here: it is done in
;; cosi/proofs/newtx:validate-transaction only.

;; Topological sorting of transactions is done here for now.  There is
;; internally a proposal to reject transactions that spend a UTXO in
;; the same block. However, it's being considered, and I'm not sure
;; how to implement it correctly right now, especially for early
;; transactions. -mhd, 6/13/18

;; Improvements needed here: this is very crude for now. It could
;; potentially leave transactions hanging around a long time based
;; merely on having a hash that gets iterated to later -- essentially
;; a random characteristic.  Later, the ordering should be
;; prioritized. Typically based on a combination of weightings: how
;; long has the transaction been sitting around waiting (the longer,
;; the higher the priority); how much is offered in fees (the more the
;; higher priority). -mhd, 6/13/18



(defun check-block-transactions (blk)  
  "Return nil if invalid block. This is run by a CoSi block 
   validator. Validate by recomputing the full merkle hash on all the 
   transactions and comparing with that with that saved in the 
   merkle-root-hash slot of the block." 
  (hash:hash=  
   (cosi/proofs:compute-merkle-root-hash (cosi/proofs:block-transactions blk)) 
   (cosi/proofs:block-merkle-root-hash blk)))



(defun clear-transactions-in-block-from-mempool (block)
  "Remove transactions that have just been added the block from the mempool that
   is globally bound to cosi-simgen:*mempool*. Note that these transactions may
   not be identical EQ Lisp instances, so even if you're holding a pointer to
   transaction that's in the mempool in your hand, to find the corresponding
   instance in the block, you must compare must compare two instances using
   their hash keys, not EQ. That also acts as a vefification that the contents
   are exactly the same, have not undergone any changes after being encoded,
   transferred from one node to the other over the network, and decoded, etc."
  (loop with mempool = cosi-simgen:*mempool*
        for transaction in (cosi/proofs:block-transactions block)
        count (remove-transaction-from-mempool transaction mempool)
          into n-removed
        finally
           (emotiq:note "~%Removed ~d transaction~p from mempool.~%"
                   n-removed n-removed)
           (cosi/proofs/newtx:dump-txs :mempool t)))

;; Modularity issue: note that this should not really "know" that
;; block-transactions is list. It is now specified as sequence, and it's left
;; open for it to become a merkle tree; clean up later!  -mhd, 6/20/18
