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
    (write-string hex-string outstream :end 6)
    (write-string ".." outstream)
    (write-string hex-string outstream :start (- end 3))))



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
  transaction-id                        ; a hash of this transaction, which also
                                        ;   uniquely identifies it

  transaction-inputs                    ; sequence of 0 or more pairs,
                                        ;   each a sequence of a
                                        ;   unique identifier of a
                                        ;   previous transaction and
                                        ;   the 0-based index of one
                                        ;   of that transaction's
                                        ;   outputs (which gives the
                                        ;   unspent units from which
                                        ;   to spend); 0-indexed e.g.,
                                        ;   (("ec2..a2" 0) ("ec2..a2" 3))
  transaction-outputs                   ; sequence of 1 or more pairs,
                                        ;   each a sequence of account
                                        ;   and amount; 0-indexed
                                        ; e.g., (("2bd..90" 50100) ("90f..1c" 93410))
  )



(defun print-transaction-structure (transaction outstream)
  (print-unreadable-object (transaction outstream)
    (with-slots 
          (transaction-id transaction-inputs transaction-outputs)
        transaction
      (format outstream "~(~a~) " (type-of transaction))
      (format outstream "[Tx ID=")
      (abbrev-hash transaction-id outstream)
      (format outstream "] ")
      (loop for transaction-input in  transaction-inputs
            as first-time = t then nil
            when (not first-time)
              do (format outstream ", ")
            do (abbrev-hash (first transaction-input) outstream)
               (format 
                outstream "[~d]"
                (second transaction-input)))
      (if (null transaction-inputs)
          (format outstream "[] ")      ; COINBASE (no inputs)
          (format outstream " "))
      (format outstream "=> ")
      (loop for transaction-output in  transaction-outputs
            as first-time = t then nil
            when (not first-time)
              do (format outstream ", ")
            do (abbrev-hash (first transaction-output) outstream)
               (format outstream "/~d" (second transaction-output))))))

;; Note: to avoid describe output truncation, may need something like
;;
;;   (setq *print-right-margin* 1000)
;;
;; I found this to be true on SBCL.  -mhd, 2/25/18




(defun sha-256-string-for-transaction (transaction)
  (with-slots 
        (transaction-id transaction-inputs transaction-outputs)
      transaction
    (emotiq:sha-256-string
     (format
      nil "~a-~a-~a"
      transaction-id
      (with-output-to-string (out)
        (loop for input in transaction-inputs
              do (format out "~a/~a" (elt input 0) (elt input 1))))
      (with-output-to-string (out)
        (loop for output in transaction-outputs
              do (format out "~a/~a" (elt output 0) (elt output 1))))))))
  

(defun make-hash-pointer-for-transaction (transaction)
  (make-hash-pointer 
   :item transaction
   :digest-hex-string (sha-256-string-for-transaction transaction)))



(defparameter *current-coinbase-amount* 50000)
(defparameter *current-transaction-fee* 1000)



(defun get-coinbase-amount ()
  (+ *current-coinbase-amount*
     *current-transaction-fee*))




(defun get-timestamp ()
  (get-universal-time))



(defun hash-transaction (transaction)
  (with-slots 
        (transaction-inputs transaction-outputs)
      transaction
    (emotiq:sha-256-string
     (format
      nil "~a-~a"
      (with-output-to-string (out)
        (loop for input in transaction-inputs
              do (format out "~a/~a" (elt input 0) (elt input 1))))
      (with-output-to-string (out)
        (loop for output in transaction-outputs
              do (format out "~a/~a" (elt output 0) (elt output 1))))))))

(defun make-genesis-transaction ()
  (declare (special *alice-account*))   ; short-term kludge, declared later!
  (let* ((transaction
           (make-transaction
            :transaction-inputs '()
            ;; A coinbase transaction that mints new coins simply issues the
            ;; coin to Alice's account in this simple blockchain.
            :transaction-outputs 
              (list 
               (list *alice-account* (get-coinbase-amount))))))
    (setf (transaction-id transaction) (hash-transaction transaction))
    transaction))


(defun make-spend-transaction (input-specs output-specs)
  (let* ((transaction
           (make-transaction
            :transaction-inputs input-specs
            :transaction-outputs output-specs)))
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
      (loop for (input-id) in (transaction-inputs tx)
            when (tx-ids= input-id id)
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
        with outputs-of-input-transaction
        with n-outputs
        with indexed-output
        with subamount
        with total-output-amount
        for (id index) in input-specs
        do (unless (setq utxo (find-utxo id))
             (warn "Transaction failure: no UTXO found.")
             (return nil))
           (setq outputs-of-input-transaction (transaction-outputs utxo))
           (setq n-outputs (length outputs-of-input-transaction))
           (when (not (< index n-outputs))
             (warn "Transaction failure: out-of-range index in UTXO: ~d" 
                   index)
             (return nil))
           (setq indexed-output (elt outputs-of-input-transaction index))
           (setq subamount (elt indexed-output 1))
        sum subamount into total-input-amount
        ;; total-input-amount is the amount that had been in an unspent output,
        ;; which must now be consumed as "input", and then must be consumed as
        ;; output.

        ;; to do: check for validity!
        ;; to do: check signatures!
        finally
           (format t "~%Transaction total input amount: ~a~%" total-input-amount)
           (format t "~%Output specs: ~a~%" output-specs)
           (setq total-output-amount
                 (loop for (nil amount) in output-specs
                       ;; to do: more checks for validity!
                       sum amount))
           (format t "~%Transaction total output amount: ~a~%" 
                   total-output-amount)
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
           (let* ((new-transaction
                    (make-spend-transaction input-specs output-specs))
                  (hash-pointer-of-transaction
                    (make-hash-pointer-for-transaction new-transaction))
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
             (setq *last-transaction* new-transaction)
             (return (values new-transaction b)))))



(defun account-addresses= (address-1 address-2)
  (string-equal address-1 address-2))

(defun get-credits-per-account (account)
  (let ((credits '()))
    (do-blockchain (blk)
      (do-transactions (tx blk)
        (loop for (tx-output-account amount) in (transaction-outputs tx)
              as index from 0
              when (account-addresses= account tx-output-account)
                do (push (list (transaction-id tx) index amount 
                               (transaction-inputs tx)
                               (transaction-outputs tx))
                         credits))))
    credits))

(defun find-transaction-with-particular-input (input-tx-id input-index)
  (do-blockchain (blk)
    (do-transactions (tx blk)
      (loop for (tx-id index) in (transaction-inputs tx)
            when (and (tx-ids= tx-id input-tx-id)
                      (= index input-index))
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
                 (loop for (tx-id index) in inputs
                       do (format t "Tx ~a, index = ~a" tx-id index)))
             (format t "~%  Coin OUT")
             (loop for (acct amt) in outputs
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



(defparameter *alice-account* 
  (emotiq:sha-256-string "alice"))

(defparameter *bob-account* 
  (emotiq:sha-256-string "bob"))

(defparameter *david-account* 
  (emotiq:sha-256-string "david"))



(defun print-blockchain-info ()
  (do-blockchain (block)
    (print-block-info block)))

(defun print-block-info (block)
  (format t "Time: ~a~%" (get-block-timestamp-string block))
  (let ((transaction (hash-pointer-item (hash-pointer-of-transaction block))))
    (format t "TX: ~a~%" transaction)))



(defun tx-input-sequence (previous-tx sequence-number &rest additional)
  (list* (list (transaction-id previous-tx)
               sequence-number)
         (when additional
           (loop for (tx seq-no) on additional by 'cddr
                 collect `(,(transaction-id tx)
                           ,seq-no)))))


(defvar *blockchain-tx-attempt-counter* 0)
(defvar *blockchain-tx-success-counter* 0)


;;; The following sets of args are order-dependent. I.e., first set are
;;; fine. The second are double-spends coming after this first set.  The third
;;; refer to transactions never were known.

(defun blockchain-test-1 ()
  "Test by resetting the blockchain, which issues 51000 to Alice,
sending 40000 from Alice to Bob, 20000 from Bob to David, 5000 from
Bob to Alice, and finally 1000 from Bob to David."
  (let (prev-tx)
    (setq prev-tx (restart-blockchain))
    (format t "~%(GENESIS)~%  Tx-0: ~A~%" prev-tx)
    ;; Genesis Block Coinbase Tx gives 510000 to Alice initially.
    (setq prev-tx 
          (next-transaction 
           (tx-input-sequence prev-tx 0)
           `((,*bob-account* 40000) (,*alice-account* ,(- 51000 40000)))))
    (format t "~%  Tx-1: ~A~%" prev-tx)
    (setq prev-tx 
          (next-transaction
           (tx-input-sequence prev-tx 0)
           `((,*david-account* 20000) (,*bob-account* ,(- 40000 20000)))))
    (format t "~%  Tx-2: ~A~%" prev-tx)
    (setq prev-tx
          (next-transaction
           (tx-input-sequence prev-tx 0)
           `((,*alice-account* 5000) (,*bob-account* ,(- 20000 5000)))))
    (format t "~%  Tx-3: ~A~%" prev-tx)
    (setq prev-tx
          (next-transaction
           (tx-input-sequence prev-tx 1)
           `((,*david-account* 1000) (,*bob-account* ,(- 15000 1000)))))
    (format t "~%  Tx-4: ~A~%" prev-tx)))

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
    (let (prev-tx)
      (setq prev-tx
            (next-transaction
             (tx-input-sequence (genesis-tx) 0) ; <= bad - already used Tx!
             `((,*bob-account* 40000) (,*alice-account* ,(- 51000 40000)))))
      (format t "~%  Result: ~a (~a)~%" 
              prev-tx
              (if (eq prev-tx 'nil)
                  "as expected"
                  (format nil "Bad - unexpected value: ~a." prev-tx)))
      (setq prev-tx
            (next-transaction
             (tx-input-sequence (penultimate-tx) 1) ; <= bad - already used Tx!
             `((,*david-account* 1000) (,*bob-account* 14000))))
      (format t "~%  Result: ~a (~a)~%" 
              prev-tx
              (if (eq prev-tx 'nil)
                  "as expected"
                  (format nil "Bad - unexpected value: ~a."
                          prev-tx))))))

(defun blockchain-test-3 ()
  "Test by first invoking function blockchain-test-2, q.v., and then attempting
.... [See inline comments in code for full details.]"
  (blockchain-test-2)

  ;; Try to spend 15,000 from *last-transaction*'s output to *bob-account*
  ;; (sending it to *alice-account*), but we  account had only gotten 14,000
  ;; from this transaction:
  (let (prev-tx tx-8)
    (setq prev-tx
          (next-transaction
           (tx-input-sequence *last-transaction* 1) ; <= bad - overspend!
           `((,*alice-account* 15000))))
    (format t "~%  Result: ~a (~a)~%" 
            prev-tx
            (if (eq prev-tx 'nil)
                "as expected"
                (format nil "Bad - unexpected value: ~a."
                        prev-tx)))
    ;; Now try to spend exactly 14,000 from this transaction's output to
    ;; *bob-account* (sending it to *alice-account*). This account had gotten
    ;; exactly 14,000. So this will leave zero.  Note that this would leave zero
    ;; (0) as a transaction fee (!). This will be allowed (for now).  Maybe zero
    ;; transaction fees are kind of iffy, but review that issue later!
    (setq prev-tx
          (next-transaction
           (tx-input-sequence *last-transaction* 1) ; <= GOOD TRANSACTION HERE
           `((,*alice-account* 14000))))
    (setq tx-8 prev-tx)                 ; save it
    (format t "~%  Tx-8: ~A~%" prev-tx)
      
    ;; Now try spending zero (0) from Alice back to Bob. Should be rejected:
    ;; zero or negative amounts cannot be transferred.
    (setq prev-tx
          (next-transaction
           (tx-input-sequence tx-8 0) ; <= bad (attempt to transfer zero amount)
           `((,*bob-account* 0))))
    (format t "~%  Result: ~a (~a)~%" 
            prev-tx
            (if (eq prev-tx 'nil)
                "as expected"
                (format nil "Bad - unexpected value: ~a."
                        prev-tx)))

    ;; Now try spending negative (-1) million from Alice back to Bob. Should
    ;; be rejected: zero or negative amounts cannot be transferred.
    (setq prev-tx
          (next-transaction
           (tx-input-sequence tx-8 0) ; <= bad (attempt to transfer negative amount)
           `((,*bob-account* 0))))
    (format t "~%  Result: ~a (~a)~%" 
            prev-tx
            (if (eq prev-tx 'nil)
                "as expected"
                (format nil "Bad - unexpected value: ~a."
                        prev-tx)))))



(defvar *nonexistent-test-transaction-id*
  (sha-256-string "arbitrary-hash-doesnotexist")
  "A piece of 'fixture data' just be to be used in the next test
   as a dummy transaction ID, i.e., for a dummy reference to
   a transaction that does not exist, i.e., for testing a bad
   reference in a transaction.")

(defun blockchain-test-4 ()
  "Test by first invoking function blockchain-test-3, q.v., and then attempting
   a couple more bad transactions: one with an index out of range, and one that
   tries to use as input a nonexistent transaction."
  (blockchain-test-3)
  (let (prev-tx)
    (setq prev-tx
          (next-transaction
           (tx-input-sequence *last-transaction* 13) ; <= bad - index out of range!
           `((,*bob-account* 4000)
             (,*alice-account* (- 51000 4000)))))
    (format t "~%  Result: ~a (~a)~%" 
            prev-tx
            (if (eq prev-tx 'nil)
                "as expected"
                (format nil "Bad - unexpected value: ~a."
                        prev-tx)))
    (setq prev-tx
          (next-transaction
           (list (list *nonexistent-test-transaction-id* 0)) ; <= bad: nonexistent tx
           `((,*alice-account* 5000) 
             (,*bob-account* ,(- 20000 5000)))))
    (format t "~%  Result: ~a (~a)~%" 
            prev-tx
            (if (eq prev-tx 'nil)
                "as expected"
                (format nil "Bad - unexpected value: ~a."
                        prev-tx)))))


(defun test-blockchain ()
  (blockchain-test-4)
  (format t "~%---~%Transactions tests finished. Now printing blockchain...~%")
  (print-blockchain-info)
  (format t "~%DONE.~%"))
