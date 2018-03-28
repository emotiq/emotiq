;;;; blockchain.lisp

(in-package :emotiq)



(defstruct (hash-pointer
            (:print-object print-hash-pointer-structure))
  item
  digest-hex-string)              ; sha-256 hash as 64-long hex string



(defun print-hash-pointer-structure (hash-pointer outstream)
  "Print HASH-POINTER to OUTSTREAM in the form #<H(...) ITEM>, where ITEM is
   replaced by the printed representation of the item slot's value, and
   H(hhh..hh) shows an abbreviated hash digest hex string.  For example, a hash
   pointer with a transaction as its item might print as follows: #<H(81ba38..fe9)
   #<transaction => 2bd479..290/51000>>"
  (print-unreadable-object (hash-pointer outstream)
    (with-slots (item digest-hex-string) hash-pointer
      (format outstream "H(")
      (abbrev-hash digest-hex-string outstream)
      (format outstream ") ~a" item))))






(defstruct (blockchain-block 
            (:conc-name "")
            (:print-object print-blockchain-block-structure))

  timestamp             ; Lisp universal time

  hash-pointer-of-previous-block  ; nil for genesis block, otherwise a
                                  ;   hash pointer for previous block

  hash-pointer-of-transaction)



(defun abbrev-hash (hex-string outstream)
  (let ((end (length hex-string)))
    ;; (write-string hex-string outstream :end 6)
    (write-string hex-string outstream :end 6)
    (write-string ".:" outstream)
    ;; (write-string hex-string outstream :start (- end 3))
    ))



(defun stamp-time (timestamp outstream)
  "Write TIMESTAMP, a universal-time bignum integer, to outstream as,
   for example: 2018-02-22T15:51:38 (GMT-8)"
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time timestamp)
    (declare (ignore day-of-week dst-p))
    (format outstream "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d (GMT~@d)"
            year month date hour minute second (- tz))))


(defun get-block-timestamp-string (blockchain-block)
  (with-output-to-string (out)
    (stamp-time (timestamp blockchain-block) out)))
  



(defun print-blockchain-block-structure (blockchain-block outstream)
  "Print BLOCKCHAIN-BLOCK to OUTSTREAM as follows:

    For the genesis block (has no previous block):

      #<BLOCKCHAIN-BLOCK 0 2018-07-16T19:20:30 (GMT-5) (Genesis)>

    For a regular block (shows first and last three nibles of the previous block
    hash pointer):

      #<BLOCKCHAIN-BLOCK 0 2018-07-16T07:20:39 (GMT-5) (Follows: 862..74)>"
  (print-unreadable-object (blockchain-block outstream)
    (with-slots 
          (timestamp
           hash-pointer-of-previous-block)
        blockchain-block
      (format outstream "~(~a~) " (type-of blockchain-block))
      (stamp-time timestamp outstream)
      (if (null hash-pointer-of-previous-block)
          (write-string " (Genesis)" outstream)
          (let* ((hex-string
                   (hash-pointer-digest-hex-string hash-pointer-of-previous-block)))
            (write-string " (Follows: " outstream)
            (abbrev-hash hex-string outstream)
            (write-string ")" outstream))))))


(defun sha-256-string-for-block (blockchain-block)
  (with-slots 
        (timestamp
         hash-pointer-of-previous-block hash-pointer-of-transaction)
      blockchain-block
    (let* ((prev-block-hash
             (if (null hash-pointer-of-previous-block)
                 nil
                 (hash-pointer-digest-hex-string hash-pointer-of-previous-block)))
           (tx-hash
             (hash-pointer-digest-hex-string hash-pointer-of-transaction)))
      (emotiq:sha-256-string
       (format
        nil "~a-~a-~a"
        timestamp prev-block-hash tx-hash)))))

(defun make-hash-pointer-for-block (blockchain-block)
  (make-hash-pointer 
   :item blockchain-block
   :digest-hex-string (sha-256-string-for-block blockchain-block)))




;;; When you use a transaction (TX), you always completely consume its
;;; output.  Any amount not sent to some other party is conventionally
;;; sent back to the sender's same address, which is then referred to
;;; as a so-called "change address".

(defstruct (transaction 
            (:conc-name "")
            (:print-object print-transaction-structure))

  ;; a hash of this transaction, which also uniquely identifies it
  transaction-id

  ;; [First put outputs, since they start out at first unspent until a
  ;; later transaction arrives with inputs authorized to redeem them.]

  ;; sequence of transaction-output structures (see below)
  transaction-outputs

  ;; sequence of transaction-input structions (see below)
  transaction-inputs)


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

(defstruct (transaction-output (:conc-name ""))
  tx-out-public-key-hash       ; like the Bitcoin address
  tx-out-amount                ; integer number of smallest coin units
  tx-out-lock-script)          ; what Bitcoin calls scriptPubKey

(defstruct (transaction-input (:conc-name ""))
  tx-in-id
  tx-in-index
  tx-in-unlock-script                   ; what Bitcoin calls scriptSig
  ;; ^-- we create a list of args to pass to the lock script
  
  ;; direct pointers: in memory only
  %tx-in-utxo
  %tx-in-public-key
  %tx-in-signature)
  


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
       ((+ - * = /=)
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

(def-script-var ^tx-public-key-hash get-current-tx-public-key-hash)

(def-script-var ^tx-public-key get-current-tx-public-key)
(def-script-var ^tx-signature get-current-tx-signature)


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

  
(defun fail-script-op ()
  (cond
    (*current-script-op*
     (require-transaction-script-op-context)
     (warn "Failure in script op ~a" *current-script-op*)
     (throw *transaction-script-catch-tag*
       nil))
    (t
     ;; Allow going off into the wild blue yonder, since this makes it
     ;; very convenient to run in a REPL and simply get back to top
     ;; level.  If deep down in the stack, it may not work out.
     (cerror
      "Continue regardless (presumably in a REPL)"
      "Not in a Transaction-Script-OP context, as required."))))





(defun eval-transaction (transaction)
  ;; simplified: for now: assume 1 input, n outputs; will generalize next
  (loop with tx-in = (first (transaction-inputs transaction))
        with id = (tx-in-id tx-in)
        with index = (tx-in-index tx-in)
        with utxo = (%tx-in-utxo tx-in)
        with utxo-outputs = (transaction-outputs utxo)
        with utxo-output
          = (or (nth index utxo-outputs) 
                ;; debugging code:
                (unless (tx-ids= id (transaction-id utxo))
                  (error "UTXO ~a does not match input spec [~a/~d]"
                         utxo id index))
                (error "UTXO ~a output [~a/~d] lost - likely programming error"
                       utxo id index))
        with unlock-script = (tx-in-unlock-script tx-in)
        with public-key-hash = (tx-out-public-key-hash utxo-output)
        ;; with tx-out-amount = (tx-out-amount utxo-output)
        with lock-script = (tx-out-lock-script utxo-output)
        do (return
             (in-transaction (transaction)
               (in-transaction-script-context
                 (when (null lock-script)
                   (fail-script))
                 (when (null unlock-script)
                   (fail-script))
                 (let* ((unlock-script-function-name (first unlock-script))
                        (unlock-args
                          (apply-x
                           unlock-script-function-name
                           '()
                           `((^signature . ,(%tx-in-signature tx-in))
                             (^public-key . ,(%tx-in-public-key tx-in)))))
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
                       . ,public-key-hash)))))))))





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

(defun serialize-transaction (transaction)
  (with-output-to-string (out)
    (flet ((emit (x)
             (format out "~a " x)))
      (loop for tx-out in (transaction-outputs transaction)
            as hash = (tx-out-public-key-hash tx-out)
            as amt = (tx-out-amount tx-out)
            as script = (tx-out-lock-script tx-out)
            do (emit hash) (emit amt) (emit script))
      (loop for tx-in in (transaction-inputs transaction)
            as id = (tx-in-id tx-in)
            as index = (tx-in-index tx-in)
            do (emit id) (emit index)))))

(defun serialize-current-transaction ()
  (serialize-transaction *current-transaction*))



     

(defun hash-string= (hash-string-1 hash-string-2)
  (string-equal hash-string-1 hash-string-2))

(def-script-op public-key-equal-verify (public-key public-key-hash)
  (let ((pkh-for-public-key (hash-160-string public-key)))
    ;; For debugging/diagnostics:
    (unless (= (length pkh-for-public-key) (length public-key-hash))
      (warn 
       "Unequal-length public key hashes ~d vs. ~d. Programming error likely!"
       (length pkh-for-public-key) (length public-key-hash)))
    (cond
      ((hash-string= pkh-for-public-key public-key-hash)
       t)
      (t
       (fail-script-op)))))

(def-script-op check-signature (public-key signature)
  (verify-signature
   public-key (serialize-current-transaction) signature))


(def-locking-script script-pub-key (public-key signature)
  (and (public-key-equal-verify public-key ^tx-public-key-hash)
       (check-signature public-key signature)))



(def-unlocking-script script-sig ()
  (list ^public-key ^signature))
   

        
  




(defun print-transaction-structure (transaction outstream)
  (print-unreadable-object (transaction outstream)
    (with-slots 
          (transaction-id transaction-inputs transaction-outputs)
        transaction
      (format outstream "~(~a~) " (type-of transaction))
      (format outstream "[Tx ID=")
      (if (or (null transaction-id)
              (string= transaction-id ""))
          "none"
          (write-string transaction-id outstream))
      (format outstream "] ")
      (loop for transaction-input in transaction-inputs
            as first-time = t then nil
            as tx-in-id = (tx-in-id transaction-input)
            as index = (tx-in-index transaction-input)
            when (not first-time)
              do (format outstream ", ")
            do (abbrev-hash tx-in-id outstream)
               (format outstream "[~d]" index))
      (if (null transaction-inputs)
          (format outstream "[] ")      ; COINBASE (no inputs)
          (format outstream " "))
      (format outstream "=> ")
      (loop for tx-out in transaction-outputs
            as public-key-hash = (tx-out-public-key-hash tx-out)
            as amt = (tx-out-amount tx-out)
            as first-time = t then nil
            when (not first-time)
              do (format outstream ", ")
            do (abbrev-hash public-key-hash outstream)
               (format outstream "/~d" amt)))))

;; Note: to avoid describe output truncation, may need something like
;;
;;   (setq *print-right-margin* 1000)
;;
;; I found this to be true on SBCL.  -mhd, 2/25/18




(defun make-hash-pointer-for-transaction (transaction)
  (make-hash-pointer 
   :item transaction
   :digest-hex-string (hash-transaction transaction)))



(defparameter *current-coinbase-amount* 50000)
(defparameter *current-transaction-fee* 1000)



(defun get-coinbase-amount ()
  (+ *current-coinbase-amount*
     *current-transaction-fee*))




(defun get-timestamp ()
  (get-universal-time))



(defun hash-transaction (transaction)
  (hash-serialized-transaction (serialize-transaction transaction)))

(defun hash-serialized-transaction (serialized-transaction)
  (hash-256-string serialized-transaction))
  


(defparameter *alice-pkey-hash* "3bab17cf1c0d97d88e28ef7af96dbe60b8daef26")
(defparameter *alice-pkey* "7ac0728d574b2269628e2ec57d9520a74f7f03877d5b38c9c94221556679c7fa")
(defparameter *alice-skey* "0e765439d985ff2c70bbe42f1f84b6418afc3fc56dc85872a028025503b49516")

(defun make-genesis-transaction ()
  (let* ((transaction
           (make-transaction
            :transaction-inputs '()
            ;; A coinbase transaction that mints new coins simply issues the
            ;; coin to Alice's account in this simple blockchain.
            :transaction-outputs 
              (list
               (make-transaction-output
                :tx-out-public-key-hash *alice-pkey-hash*
                :tx-out-amount (get-coinbase-amount)
                :tx-out-lock-script (get-locking-script 'script-pub-key))))))
    (setf (transaction-id transaction) (hash-transaction transaction))
    transaction))

(defun make-genesis-block ()
  (let* ((b (make-blockchain-block))
         (transaction (make-genesis-transaction)))
    (setf (timestamp b) (get-timestamp))
    (setf (hash-pointer-of-previous-block b) nil)
    (setf (hash-pointer-of-transaction b)
          (make-hash-pointer-for-transaction transaction))
    (values transaction b)))


(defvar *genesis-block* nil)

(defvar *last-block* nil)
(defvar *last-transaction* nil)

(defun start-blockchain ()
  (multiple-value-bind (tx b)
      (make-genesis-block)
    (setq *genesis-block* b)
    (setq *last-transaction* tx)
    (setq *last-block* b)
    (values tx b)))

(defun restart-blockchain ()
  (start-blockchain))

(defun reset-blockchain ()
  (setq *genesis-block* nil)
  (setq *last-block* nil)
  (setq *last-transaction* nil))


(defun tx-ids= (id-1 id-2)
  (string-equal id-1 id-2))

;; For now, using hex strings, so compare using string-equal. This can be
;; tightened later for efficiency.


(defun previous-block (block)
  ;; Should this reauthenticate? In some modes?
  (let ((p (hash-pointer-of-previous-block block)))
    (if p
        (hash-pointer-item p)
        nil)))


(defmacro do-blockchain ((block-var) &body body)
  "Execute BODY with BLOCK-VAR to each block from last to first.
BODY can returned early, and particular values can be returned using
RETURN or RETURN-FROM. Otherwise, this returns nil after all blocks
have been visited."
  `(loop with ,block-var = *last-block*
         while ,block-var
         do (progn ,@body)
            (setq ,block-var (previous-block ,block-var))))

(defmacro do-transactions ((tx-var block) &body body)
  (let ((tx-list '#:tx-list))
    `(let ((,tx-list                    ; temporary for now! real multiple
                                        ; transactions per block soon!
             (list
              (hash-pointer-item
               (hash-pointer-of-transaction ,block)))))
       (loop for ,tx-var in ,tx-list
             do (progn ,@body)))))


(defun find-transaction-per-id (id)
  (do-blockchain (block)
    (do-transactions (tx block)
      (when (tx-ids= (transaction-id tx) id)
        (return-from find-transaction-per-id (values tx block))))))

(defun find-transaction-per-id-or-double-spend (id)
  (do-blockchain (block)
    (do-transactions (tx block)
      (loop for tx-in in (transaction-inputs tx)
            as tx-in-id = (tx-in-id tx-in)
            when (tx-ids= tx-in-id id)
              ;; earlier spend block found (double spend attempt)
              do (return-from 
                  find-transaction-per-id-or-double-spend
                   (values tx block :double-spend)))
      ;; If not a double-spend, then return a matched tx if found.
      (when (tx-ids= (transaction-id tx) id)
        (return-from find-transaction-per-id-or-double-spend
          (values tx block))))))


(defun find-utxo (id &key no-warn)
  (multiple-value-bind
        (found-tx? found-block? problem-found?)
      (find-transaction-per-id-or-double-spend id)
    (cond
      (problem-found?
       (case problem-found?
         ((:double-spend)
          (unless no-warn
            (warn "Double spend attempt: TX ~a already spent in block ~a."
                  ;; OK short term only: showing Lisp printed reps; review
                  ;; later!
                  found-tx? found-block?))
          nil)
         (otherwise
          (unless no-warn
            (warn "Unknown problem: ~a while finding UTXO. TX: ~a; Block: ~a"
                  problem-found? found-tx? found-block?))
          nil)))
      (t (values found-tx? found-block?)))))



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

(defun next-transaction (input-specs output-specs)
  (when (null *last-block*)
    (start-blockchain))
  (loop with utxo
        with utxo-transaction-outputs
        with n-outputs
        with indexed-output
        with subamount
        with total-output-amount
        with tx-outputs
        for (id index public-key private-key) in input-specs
        do (unless (setq utxo (find-utxo id))
             (warn "Transaction failure: no UTXO found.")
             (return nil))
           (setq utxo-transaction-outputs (transaction-outputs utxo))
           (setq n-outputs (length utxo-transaction-outputs))
           (when (not (< index n-outputs))
             (warn "Transaction failure: out-of-range index in UTXO: ~d" 
                   index)
             (return nil))
           (setq indexed-output (elt utxo-transaction-outputs index))
           (setq subamount (tx-out-amount indexed-output))
        sum subamount into total-input-amount
        collect (make-transaction-input
                 :tx-in-id id
                 :tx-in-index index
                 :tx-in-unlock-script (get-unlocking-script 'script-sig)
                 ;; :tx-in-unlock-args (list public-key ':signature-needed-here)
                 :%tx-in-utxo utxo
                 :%tx-in-public-key public-key)
          into tx-inputs
        collect private-key
          into private-keys-for-inputs
        ;; total-input-amount is the amount that had been in an unspent output,
        ;; which must now be consumed as "input", and then must be consumed as
        ;; output.
        finally

           (setq total-output-amount
                 (loop for (public-key-hash amount) in output-specs
                       collect (make-transaction-output
                                :tx-out-public-key-hash public-key-hash
                                :tx-out-amount amount
                                :tx-out-lock-script (get-locking-script 'script-pub-key))
                         into tx-outs
                       sum amount into amount-sum
                       finally (setq tx-outputs tx-outs)
                               (return amount-sum)))

           (format t "~%Input specs: ~a~%" input-specs)
           (format t "Output specs: ~a~%" output-specs)
           (format t "UTXO found: ID = ~a, Index = ~a~%" id index)
           (format t "Transaction total input amount: ~a~%" total-input-amount)
           (format t "Transaction total output amount: ~a~%" total-output-amount)
           ;; check for overspend:
           (when (> total-output-amount total-input-amount)
             (warn "Overspend attempt: Output total ~a > input total ~a."
                   total-output-amount total-input-amount)
             (warn "Transaction failure.")
             (return nil))
           ;; check for zero (or negative) spend: (Combine checks. OK?)
           (when (<= total-output-amount 0)
             (warn "~a spend attempt: output total ~a: must spend more than zero."
                   (if (< total-output-amount 0) "Zero" "Negative")
                   total-output-amount)
             (warn "Transaction failure.")
             (return nil))
           (let* ((transaction
                    (make-transaction
                     :transaction-inputs tx-inputs
                     :transaction-outputs tx-outputs))
                  (message (serialize-transaction transaction)))
             (setf (transaction-id transaction) (hash-serialized-transaction message))
             ;; Sign transaction:
             (loop for tx-input in tx-inputs
                   as private-key in private-keys-for-inputs
                   as signature = (sign-message private-key message)
                   do (setf (%tx-in-signature tx-input) signature))
             (cond
               ((not (eval-transaction transaction))
                ;; Transaction failed.
                (warn "Transaction failed in script evaluation.")
                (return nil))
               (t
                ;; Transaction succeeded.
                (let* ((hash-pointer-of-transaction
                         (make-hash-pointer-for-transaction transaction))
                       (hash-pointer-of-previous-block
                         (make-hash-pointer-for-block *last-block*))
                       (timestamp (get-timestamp))
                       (b (make-blockchain-block
                           :timestamp timestamp
                           :hash-pointer-of-previous-block 
                           hash-pointer-of-previous-block
                           :hash-pointer-of-transaction
                           hash-pointer-of-transaction)))
                  (setq *last-block* b)
                  (setq *last-transaction* transaction)
                  (return (values transaction b))))))))





;;;; Wallet / Demo Stuff


(defun account-addresses= (address-1 address-2)
  (string-equal address-1 address-2))

(defun get-credits-per-account (account)
  (let ((credits '()))
    (do-blockchain (blk)
      (do-transactions (tx blk)
        (loop for tx-out in (transaction-outputs tx)
              as public-key-hash = (tx-out-public-key-hash tx-out)
              ;; ---*** update terminology!
              as amount = (tx-out-amount tx-out)
              as index from 0
              when (account-addresses= account public-key-hash)
                do (push (list (transaction-id tx) index amount 
                               (transaction-inputs tx)
                               (transaction-outputs tx))
                         credits))))
    credits))

(defun find-transaction-with-particular-input (input-tx-id input-index)
  (do-blockchain (blk)
    (do-transactions (tx blk)
      (loop for tx-in in (transaction-inputs tx)
            as tx-in-id = (tx-in-id tx-in)
            as tx-in-index = (tx-in-index tx-in)
            when (and (tx-ids= tx-in-id input-tx-id)
                      (= tx-in-index input-index))
              do (return-from find-transaction-with-particular-input 
                   tx)))))

(defun get-credit-redemptions (credits)
  (loop for credit in credits
        as (tx-id index) = credit
        unless (find-transaction-with-particular-input tx-id index)
          collect credit))

(defun get-balance (account)
  (let* ((credits (get-credits-per-account account))
         (credits-after-redemptions (get-credit-redemptions credits)))
    (loop for (tx-id nil amount inputs outputs) in credits
          do (format t "~%In tx ~a:" tx-id)
             (format t "~%  Coin amount IN: ~a" amount)
             (format t "~%  ... in via: ")
             (if (null inputs)
                 (format t " COINBASE")
                 (loop for tx-in in inputs
                       do (format t "Tx ~a, index = ~a" (tx-in-id tx-in) (tx-in-index tx-in))))
             (format t "~%  Coin OUT")
             (loop for tx-out in outputs
                   as acct = (tx-out-public-key-hash tx-out)
                   as amt = (tx-out-amount tx-out)
                   do (format t "~%    amount ~d to ~Aacct~A ~a"
                              amt
                              (if (account-addresses= acct account) "*" "")
                              (if (account-addresses= acct account) "*" "")
                              acct)))
    (list (loop for (nil nil amount) in credits
                sum amount)
          (loop for (nil nil amount) in credits-after-redemptions
                sum amount))))
          





;;;; Demo/Test



(defun print-blockchain-info (&key reverse)
  (let ((blocks '()))
    (do-blockchain (block)
      (push block blocks))
    (when reverse
      (setq blocks (nreverse blocks)))
    (loop for block in blocks
          do (print-block-info block))))

(defun print-block-info (block)
  (format t "Time: ~a~%" (get-block-timestamp-string block))
  (let ((transaction (hash-pointer-item (hash-pointer-of-transaction block))))
    (format t "TX: ~a~%" transaction)))


;; To generate keys for these tests, for now, use:
;;
;;   (multiple-value-setq (skey pkey pkey-hash) (keygen))

(progn 
  (defparameter *bob-pkey-hash* "220a3dd905912f74d004bf83989f2b76f41b7d85")
  (defparameter *bob-pkey* "0762e9167c4f22c7e537a95128ca5eedc3a0b4a7e9fdaf300647ac8fa2555113")
  (defparameter *bob-skey* "95b5fce84d948dcc2120ca9e93028e21e806a4ed2edb55a006492479c77cc67a")

  (defparameter *cindy-pkey-hash* "2a8ef419267c2700e32f9941ca8f37a62b80faf5")
  (defparameter *cindy-pkey* "de942256ecd8c7b717b06322d233a0782c520987214f7dcb6e826d1f41a9741d")
  (defparameter *cindy-skey* "1c46a159d771c5a1f89451fea875742424811447296268745386869758843aeb")

  (defparameter *david-pkey-hash* "8a5aa89686f7e546fdb4dd0b8f2bf119f72528bb")
  (defparameter *david-pkey* "0e27079c886b963a946878696f3682070609e504753a53360fe6137c5bd78d20")
  (defparameter *david-skey* "0f48e3479e134278ccd55a841cfe952a4c54d9dc87482ad98a1836952469b694")
  )



(defun tx-input-sequence (previous-tx index public-key private-key
                          &rest additional)
  (list* (list (transaction-id previous-tx) index public-key private-key)
         (when additional
           (loop for (tx idx pk sk) on additional by #'cddr
                 collect `(list ,(transaction-id tx) ,idx ,pk ,sk)))))
  


(defvar *tx-test-attempt-counter* 0)
(defvar *tx-test-success-counter* 0)
(defvar *tx-test-prev-result* nil)
(defvar *tx-test-prev-tx* nil)          ; once it's good, it stays good

(defun next-tx-test (input-specs output-specs &key restart)
  (when restart
    (setq *tx-test-attempt-counter* 0)
    (setq *tx-test-success-counter* 0)
    (setq *tx-test-prev-result* nil)
    (setq *tx-test-prev-tx* nil))
  (let ((tx 
          (if restart
              (restart-blockchain)
              (next-transaction input-specs output-specs))))
    (setq *tx-test-prev-result* tx)
    (incf *tx-test-attempt-counter*)
    (cond
      (tx
       (setq *tx-test-prev-tx* tx)
       (incf *tx-test-success-counter*)
       (if restart
           (format t "~%(GENESIS)~%  Tx-~d: ~A~%" *tx-test-success-counter* tx)
           (format t "~%  Tx-~d: ~A~%" *tx-test-success-counter* tx))))))




;;; The following sets of args are order-dependent. I.e., first set are
;;; fine. The second are double-spends coming after this first set.  The third
;;; refer to transactions never were known.

(defun blockchain-test-1 ()
  "Test by resetting the blockchain, which issues 51000 to Alice,
who sends 40000 to Bob, who sends 20000 to David, 5000 back to Alice, and 1000
to David."

  (next-tx-test nil nil :restart t)
  ;; Genesis Block Coinbase Tx gives 510000 to Alice initially.

  (next-tx-test
   (tx-input-sequence *tx-test-prev-tx* 0 *alice-pkey* *alice-skey*)
   `((,*bob-pkey-hash* 40000)                ; Alice pays 40,000 to Bob
     (,*alice-pkey-hash* ,(- 51000 40000)))) ; change back: 11,000 

  (next-tx-test
   (tx-input-sequence *tx-test-prev-tx* 0 *bob-pkey* *bob-skey*)
   `((,*david-pkey-hash* 20000)            ; Bob pays 20,000 to David
     (,*bob-pkey-hash* ,(- 40000 20000)))) ; change back: 20,000

  (next-tx-test
   (tx-input-sequence *tx-test-prev-tx* 1 *bob-pkey* *bob-skey*)
   `((,*alice-pkey-hash* 5000)            ; Bob pays 5,000 to Alice
     (,*bob-pkey-hash* ,(- 20000 5000)))) ; change back: 15,000

  (next-tx-test
   (tx-input-sequence *tx-test-prev-tx* 1 *bob-pkey* *bob-skey*)
   `((,*david-pkey-hash* 1000)            ; Bob pays 1,000 to David
     (,*bob-pkey-hash* ,(- 15000 1000)))) ; change back: 14,000
  )

(defun blockchain-test-2 ()
  "Test by first invoking function blockchain-test-1, q.v., and then attempting
two double-spends: trying to send 40000 again from Alice to Bob (spending the
output of the first transaction, which had already been spent), and then trying
to send 1000 again from Bob to Alice, from the second-to-last transaction, which
had just been used in the last transaction."
  (blockchain-test-1)
  (flet ((genesis-tx ()
           (hash-pointer-item
            (hash-pointer-of-transaction
             *genesis-block*)))
         (penultimate-tx ()
           (hash-pointer-item
            (hash-pointer-of-transaction
             (hash-pointer-item
              (hash-pointer-of-previous-block *last-block*))))))    
    (next-tx-test
     (tx-input-sequence (genesis-tx) 0  ; <= bad - already used Tx!
                        *alice-pkey* *alice-skey*)
     `((,*bob-pkey-hash* 40000)
       (,*alice-pkey-hash* ,(- 51000 40000))))
    (next-tx-test
     (tx-input-sequence (penultimate-tx) 1 ; <= bad - already used Tx!
                        *david-pkey* *david-skey*)
     `((,*david-pkey-hash* 1000) (,*bob-pkey-hash* 14000)))))

(defun blockchain-test-3 ()
  "Test by first invoking function blockchain-test-2, q.v., and then attempting
.... [See inline comments in code for full details.]"
  (blockchain-test-2)

  ;; Now the last transaction (stored in variable *last-transaction*) sent
  ;; 14,000 to Bob as the 2nd output (as change). Try to spend 15,000 from
  ;; *last-transaction*'s, sending it to Alice, but it should fail, since
  ;; there's only 14,000 available:
  (next-tx-test
   (tx-input-sequence *last-transaction* 1 ; <= bad - overspend!
                      *bob-pkey* *bob-skey*)
   `((,*alice-pkey-hash*
      15000)))                          ; <= too many units
  ;; Now try to have Bob send 14,000 to Alice. Since that's exactly what was
  ;; there, this will leave zero (0) extra, meaning zero as a transaction fee
  ;; (!). This will be allowed (for now).  Maybe zero transaction fees are
  ;; kind of iffy, but review that issue later!
  (next-tx-test
   (tx-input-sequence *last-transaction* 1 ; <= GOOD TRANSACTION HERE
                      *bob-pkey* *bob-skey*)
   `((,*alice-pkey-hash* 
      14000)))                       ; <= leaves 0 change, 0 for transaction fee
  ;; Now try spending zero (0) from Alice back to Bob. Should be rejected:
  ;; zero or negative amounts cannot be transferred.

  (next-tx-test
   (tx-input-sequence *tx-test-prev-tx* 0
                      *alice-pkey* *alice-skey*)
   `((,*bob-pkey-hash* 0)))           ; <= bad (attempt to transfer zero amount)
  ;; Now try spending negative (-1) million from Alice back to Bob. Should
  ;; be rejected: zero or negative amounts cannot be transferred.

  (next-tx-test
   (tx-input-sequence *tx-test-prev-tx* 0
                      *alice-pkey* *alice-skey*)
   `((,*bob-pkey-hash* -1000000))) ; <= bad (attempt to transfer negative amount)
  )



(defvar *nonexistent-test-transaction-id*
  (hash-256-string "arbitrary-hash-doesnotexist")
  "A piece of 'fixture data' just be to be used in the next test
   as a dummy transaction ID, i.e., for a dummy reference to
   a transaction that does not exist, i.e., for testing a bad
   reference in a transaction.")

(defun blockchain-test-4 ()
  "Test by first invoking function blockchain-test-3, q.v., and then attempting
   a couple more bad transactions: one with an index out of range, and one that
   tries to use as input a nonexistent transaction."
  (blockchain-test-3)
  (next-tx-test
   (tx-input-sequence *last-transaction* 13 ; <= bad - index out of range!
                      *alice-pkey* *alice-skey*)
   `((,*bob-pkey-hash* 4000)
     (,*alice-pkey-hash* (- 51000 4000))))
    
    
  (next-tx-test
   (list (list *nonexistent-test-transaction-id* ; <= bad: nonexistent TX ID
               0 *bob-pkey* *bob-skey*))
   `((,*alice-pkey-hash* 5000) 
     (,*bob-pkey-hash* ,(- 20000 5000)))))


(defun test-blockchain ()
  (blockchain-test-4)
  (format t "~%---~%Transactions tests finished. Now printing blockchain...~%")
  (print-blockchain-info)
  (format t "~%DONE.~%"))
