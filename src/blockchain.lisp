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

  hash-pointer-of-transaction

  (epoch-of-block 0))     ; initially 0, increments by 1 each new block



(defparameter *hash-abbrev-length* 5)
(defparameter *hash-abbrev-ending* ".:")

(defun abbrev-hash (hex-string outstream)
  "By default abbreviate HEX-STRING by writing just its first 5 (controlled by
parameter *hash-abbrev-length*) characters of hex-string and then ending with
the 2-character sequence

  .:

controlled by parameter *hash-abbrev-ending*, a string.

The idea of this ending that it's quite distinct; it's like the 3 dots of an
ellipsis (...); and it only uses 2 character cells for a bit of compactness."
  (let ((length (length hex-string)))
    (write-string hex-string outstream :start 0 :end (min length *hash-abbrev-length*))
    (write-string *hash-abbrev-ending* outstream)))



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
    (stamp-time (block-timestamp blockchain-block) out)))
  



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
  
  ;; direct pointers: in memory only: a local cache, basically
  %tx-in-utxo
  %tx-in-public-key
  %tx-in-signature
  (%tx-in-epoch 0))
  


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
  (when (> (length (transaction-inputs transaction)) 1)
    (cerror "Continue regardless"
            "Only single-input transactions handled in current implementation"))

  ;; simplified: for now: assume 1 input, n outputs; will generalize next

  (loop with current-epoch     ; for staking; see below
          = (get-current-epoch)
        with tx-in = (first (transaction-inputs transaction))
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
                       . ,public-key-hash)

                      ;; vars involved in staking transactions:
                      (^current-epoch
                       . ,current-epoch)
                      (^initial-epoch-of-stake
                       . ,(%tx-in-epoch tx-in))
                      (^n-epochs-until-unstake
                       . 3)))))))))





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
       "Unequal-length public key hashes~%  ~a~%  length = ~d~%vs.~%  ~a~%  length = ~d.~%Programming error likely!"
       pkh-for-public-key
       (length pkh-for-public-key)
       public-key-hash
       (length public-key-hash)))
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





;;; Epoch number is part of the block header and increases by 1 for each
;;; block.

;;; Escrow: we cannot ultimately allow there to be "escrow for N blocks": it’s
;;; "escrow until the retire stake transaction is posted".

;;; But implementing retire-stake-transaction-posted-p is hard: all full nodes
;;; are supposed to maintain this info as part of the state of the world.  To be
;;; more precise, nodes should build a list of validators (with stakes) when
;;; bootstrapping. They do this by processing the blockchain from genesis and
;;; calculating the stake for each validators by processing the stake and
;;; unstake transactions. Afterwards, they update this info based on blocks
;;; added to the blockchain.

;;; However, we do not need to implement the unstake transaction for the MVP,
;;; and we have also agreed that we may use fixed (in genesis block) stakes for
;;; MVP, if we don't end up having enough time to implement staking.

(def-locking-script stake (public-key signature)
  (and 
   (> ^current-epoch
      (+ ^initial-epoch-of-stake
         ^n-epochs-until-unstake))
   (public-key-equal-verify public-key ^tx-public-key-hash)
   (check-signature public-key signature)))
          
   

        
  




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
      (format outstream " ")            ; (normal tx has 1+ inputs)
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




(defun get-timestamp ()
  (get-universal-time))



(defun hash-transaction (transaction)
  (hash-160-string (serialize-transaction transaction)))

;; Is this really needed? (once "baby blockchain" is gone?). Review. -mhd, 4/3/18





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



(defparameter *coinbase-fee-in-coin-units* (/ 5 8)
  "The default coinbase fee. This is the amount awarded to the creator of each
  new ledger block.  This can be set to either an integer, float, or ratio. Use
  function get-coinbase-transaction-fee-amount to scale and round this to get it
  as a coin amount.")

(defun get-coinbase-transaction-fee-amount ()
  (coin-amount *coinbase-fee-in-coin-units*))


(defun make-coinbase-transaction-input ()
  (make-transaction-input
   ;; The next two value settings are imitative of Bitcoin, which similarly has
   ;; all 0 bits for Tx ID, all 1 bits for Tx index. These slots do not serve
   ;; their normal function, so these values are arbitrary. These two ARE part
   ;; of the hash of the transaction.
   :tx-in-id "0000000000000000000000000000000000000000000000000000000000000000"
   :tx-in-index "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

   ;; These are also not going to be used as for a normal transaction.  The rest
   ;; are NOT part of the hash of the transaction.
   :tx-in-unlock-script nil
   :%tx-in-utxo nil

   :%tx-in-public-key nil
   :%tx-in-signature nil))

(defun make-coinbase-transaction-output (public-key-hash amount)
  (make-transaction-output
   :tx-out-public-key-hash public-key-hash
   :tx-out-amount amount
   :tx-out-lock-script (get-locking-script 'script-pub-key)))

;; (get-locking-script 'script-pub-key) under consideration. Needs work.



(defun make-genesis-transaction (public-key-hash)
  (make-hash-and-maybe-sign-transaction
   (list (make-coinbase-transaction-input))
   (list (make-coinbase-transaction-output
          public-key-hash 
          (initial-total-coin-amount)))))



(defun make-coinbase-transaction (public-key-hash)
  (make-hash-and-maybe-sign-transaction
   (list (make-coinbase-transaction-input))
   (list (make-coinbase-transaction-output
          public-key-hash
          (get-coinbase-transaction-fee-amount)))))





;;; Here we generate the keys for MINTER-0. During early testnet, with
;;; memory-only frequent-recreation of the Genesis block, we here create the
;;; keys for "minter 0", whose public key hash, *minter-0-pkey-hash*, is the
;;; recipient of the initial total coin amount as the first transaction of the
;;; genesis block.

(eval-when (:compile-toplevel :execute :load-toplevel)

(defvar *minter-0-pkey-hash*)
(defvar *minter-0-pkey*)
(defvar *minter-0-skey*)

)

(multiple-value-setq (*minter-0-skey* *minter-0-pkey* *minter-0-pkey-hash*)
  (keygen))


(defun make-genesis-block ()
  (let ((tx (make-genesis-transaction *minter-0-pkey-hash*)))
    (values tx (create-block 0 nil (list tx)))))





;;;; Blockchain Context

;;; A blockchain context encapsulates a few specials for running one
;;; or more blockchains.  To create and run a blockchain, create a
;;; blockchain context by calling START-BLOCKCHAIN-CONTEXT and binding
;;; the result.  Then, wrap WITH-BLOCKCHAIN-CONTEXT, invoking it with
;;; the context, around any code that does operations, whether
;;; directly or indirectly, on the blockchain, such as
;;; NEXT-TRANSACTION.  Repeated invocations of with-blockchain-context
;;; may be made without limit. There can also be any number of
;;; parallel invocations of with-blockchain-context. However, it is
;;; not reentrant.

;;; The variable 
;;;
;;;   *current-blockchain-context*
;;;
;;; and the setf'able accessor macro
;;;
;;;   (last-transaction)
;;;   (last-block)
;;;   (genesis-block)
;;;
;;; should only be accessed in the scope of
;;; with-blockchain-context. They are established via the the
;;; above-described mechanism. At top level they are effectively
;;; unbound.

(defstruct (blockchain-context (:conc-name ""))
  (blockchain-last-transaction nil)
  (blockchain-last-block nil)
  (blockchain-genesis-block nil))


(defvar *current-blockchain-context*)


;;; The macros last-transaction, last-block, and genesis-block
;;; function to some degree as setf'able global variables, except that
;;; they must be called in a current blockchain context.  They set and
;;; get the slots of that structure.

(defmacro last-transaction ()
  `(blockchain-last-transaction *current-blockchain-context*))

(defmacro last-block ()
  `(blockchain-last-block *current-blockchain-context*))

(defmacro genesis-block ()
  `(blockchain-genesis-block *current-blockchain-context*))


(defun start-blockchain-context ()
  (multiple-value-bind (tx b)
      (make-genesis-block)
    (make-blockchain-context
     :blockchain-last-transaction tx
     :blockchain-last-block b
     :blockchain-genesis-block b)))


(defmacro with-blockchain-context ((context &optional options) &body body)
  (declare (ignore options))            ; reserved for later
  `(let* ((*current-blockchain-context* ,context))
     ,@body))

;; Should an attempt be made to detect reentrant usage and signal an
;; error?



(defun expect-transaction-context ()
  "Used for development-only testing. This may NOT be relied upon at run time in
production to stop execution if there is no transaction context. When called in
development, this signals a continuable error if there is no transaction."
  (when (not (boundp '*current-blockchain-context*))
    (cerror "continue anyhow" "not in a transaction context")))




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
  `(loop with ,block-var = (last-block)
         while ,block-var
         do (progn ,@body)
            (setq ,block-var (prev-block ,block-var))))

(defmacro do-transactions ((tx-var block) &body body)
  (let ((tx-list '#:tx-list))
    `(let ((,tx-list                    ; temporary for now! real multiple
                                        ; transactions per block soon!
             (transactions ,block)))
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




(defun require-blockchain ()
  "Check for at the genesis block, otherwise signal an error."
  (when (null (last-block))
    (error "Blockchain requires at least 1 block.")))



(defun next-transaction (input-specs output-specs)
  (require-blockchain)                  ; error checking
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
           (let ((signed-transaction
                   (make-hash-and-maybe-sign-transaction
                    tx-inputs tx-outputs :keys private-keys-for-inputs)))
             (cond
               ((not (eval-transaction signed-transaction))
                ;; Transaction failed.
                (warn "Transaction failed in script evaluation.")
                (return nil))
               (t
                ;; Transaction succeeded. Add it to the blockchain.
                (return (add-transaction-to-blockchain signed-transaction)))))))



(defun make-hash-and-maybe-sign-transaction (tx-inputs tx-outputs &key keys)
  "Create a transaction and hash it (creating its transaction ID). Then, if
keyword keys is provided non-nil, the transaction is signed. In the signing
case, keys should supply one private key for each input, either as a single
private atomic private key or singleton list in the case of a single input or as
a list of keys in the case of two or more inputs. Normally, all transactions
should be signed, and only coinbase transactions are not signed."
  (let* ((transaction
           (make-transaction
            :transaction-inputs tx-inputs
            :transaction-outputs tx-outputs))
         (message (serialize-transaction transaction)))
    (setf (transaction-id transaction) (hash-160-string message))
    ;; Got keys? Sign transaction if so:
    (when keys
      (let ((private-keys (if (and keys (atom keys)) (list keys) keys)))
        (unless (= (length private-keys) (length tx-inputs))
          (warn 
           "Unequal-length private-keys/tx-input hashes~%  ~a~%  length = ~d~%vs.~%  ~a~%  length = ~d.~%Programming error likely!"
           private-keys
           (length private-keys)
           tx-inputs
           (length tx-inputs)))
        (loop for tx-input in tx-inputs
              as private-key in private-keys
              as signature = (sign-message private-key message)
              do (setf (%tx-in-signature tx-input) signature))))
    transaction))



(defun next-coinbase-transaction (public-key-hash)
  (let ((transaction (make-coinbase-transaction public-key-hash)))
    (add-transaction-to-blockchain transaction)))
    
  
  

(defun add-transaction-to-blockchain (transaction)
  (require-blockchain)                  ; error checking
  (let* ((previous-block (last-block))
         (epoch (1+ (epoch previous-block)))
         (b (create-block epoch previous-block (list transaction))))
    (loop for tx-in in (transaction-inputs transaction)
          do (setf (%tx-in-epoch tx-in) epoch))
    (setf (last-block) b)
    (setf (last-transaction) transaction)
    (values transaction b)))





;;;; Operations to Support Epochs



;;; GET-CURRENT-EPOCH: (hands are waved temporarily)

(defun get-current-epoch ()
  (+                           ; 1+, since it's for the pending block,
   (epoch                      ; 1 greater than last block
    (last-block))
   1))





;;;; Wallet / Demo Stuff



(defun account-addresses= (address-1 address-2)
  (string-equal address-1 address-2))



;;; UTXO-P: search first the mempool and then blockchain exhaustively starting
;;; with the most-recently-created transaction returning true if the output of
;;; TRANSACTION at OUTPUT-INDEX is found to have been spent, and false (nil) if
;;; TRANSACTION itself or the beginning of the blockchain is reached.

(defun utxo-p (transaction output-index)
  (expect-transaction-context)
  (do-blockchain (block)
    (do-transactions (tx block)
      (return-from utxo-p
        (if (eq tx transaction)
            nil
            (loop for txi in (transaction-inputs tx)
                  as index = (tx-in-index txi)
                  when (eql index output-index)
                    return t))))))



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
  (let ((transaction (first (emotiq:transactions block))))
    (format t "TX: ~a~%" transaction)))



