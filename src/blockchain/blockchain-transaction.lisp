(in-package :emotiq/transaction)


(defun make-genesis-transaction (address)
  (make-transaction (list (make-coinbase-input))
                    (list (make-coinbase-output address +initial-total-coin-amount+))
                    :coinbase))

(defun make-collect-transaction (fee)
  "Make a collect transaction with amount equal to FEE, an amount of
   tokens to transfer. The fee is designated to be paid to the address
   (a function of the public key) of the leader node."
  (let* ((leader-public-key cosi-simgen:*leader*)
         (leader-address (wallet:address leader-public-key)))
    (make-transaction (list (make-coinbase-input))
                      (list (make-coinbase-output leader-address fee))
                      :collect)))

(defun make-spend-transaction (from-account ;; pbc:keying-triple
                               to-address     ;; target address
                               amount         ;; integer
                               &key
                                 (fee *minimum-transaction-fee*))
  (let* ((utxos (blockchain:get-utxos from-account)) 
         (from-skey (pbc:keying-triple-skey from-account))
         (from-pkey (pbc:keying-triple-pkey from-account))
         (from-address (emotiq/wallet:address from-pkey)))
    (multiple-value-bind (inputs outputs)
        (make-inputs/outputs from-address to-address amount fee utxos)
      (let ((transaction (make-transaction inputs outputs :spend)))
        (sign! transaction from-skey from-pkey)
        transaction))))


(defun retrieve (id &key also-search-mempool)
  "Search blockchain for a transaction with TXID matching ID. If
also-search-mempool-p is true, this also searches the current
mempool. This returns the transaction as a first value if found, and
false (nil) otherwise. In the found case, if found in the mempool,
this returns a second value of t; if found in the blockchain, this
returns the block the transaction was found in as a second value."
  (let ((result-from-mempool?
         (and also-search-mempool
              ;; TODO: make it a simple GETHASH lookup
              (loop :for tx :being :each :hash-value :of cosi-simgen:*mempool*
                 :when (id= (id tx) id)
                 :return tx))))
    (if result-from-mempool?
        (values result-from-mempool? t)
        (do-blockchain (block)
          (do-transactions (tx block)
            (when (id= (id tx) id)
              (return-from retrieve (values tx block))))))))


(defmethod validate ((transaction transaction))
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
  (when (null (inputs transaction))
    (emotiq:em-warn "Transaction with no inputs rejected")
    (return-from validate nil))
  (when (null (outputs transaction))
    (emotiq:em-warn "Transaction with no outputs rejected")
    (return-from validate nil))

  (let ((txid (id transaction)))
    (when (retrieve txid :also-search-mempool t)
      ;; This is sort of a subcase of a double-spend attempt: so give
      ;; a message to that effect. This corresponds to above rule
      ;; 'Reject if we have a matching transaction in the mempool or
      ;; in a block of the blockchain'
      (emotiq:em-warn "Double-spend attempt: a transaction with this ID (~a) is already in the mempool or on the blockchain. Rejected."
                      (id-string txid))
      (return-from validate nil))
    (loop
       :for tx-in :in (inputs transaction)
       :as id = (input-id tx-in)
       :as index = (input-index tx-in)
       :as input-tx = (retrieve id :also-search-mempool t)
       :as input-tx-outputs
       = (cond
           ((null input-tx)
            (emotiq:em-warn
             "Input transaction with presumed UTXO does not exist: no transaction with TxID ~a found in the mempool or on the blockchain." 
             (id-string id))
            (return nil))
           (t (outputs input-tx)))
       :as utxo
       = (let ((tx-out
                (or (nth index input-tx-outputs)
                    (unless (id= id (id input-tx))
                      (emotiq:em-warn "TX ~a output does not match input spec [~a/~d]."
                                      input-tx id index)
                      (return nil))
                    ;; debugging:
                    (error "TX ~a output [~a/~d] lost - likely programming emotiq:em-warn"
                           input-tx id index))))
           (when (output-double-spent-p id index :also-search-mempool t)
             (emotiq:em-warn "Double-spend attempt: TxID: ~a. Index: ~a. Rejected."
                    (id-string id) index)
             (return nil))
           tx-out)
       :when (coinbase-input-p tx-in)
       :do (emotiq:em-warn "Transaction with coinbase input - rejected") (return nil)
       :unless (witness-data-p tx-in)
       :do (emotiq:em-warn "Transaction missing witness data. Forgot to sign? Rejected.") (return nil)
       :unless (txn/script:run transaction tx-in utxo)
       :do (return nil)
       :sum (output-amount utxo) :into sum-of-inputs
       :finally (let ((sum-of-outputs (sum-outputs transaction)))
                  (when (< sum-of-inputs sum-of-outputs)
                    (emotiq:em-warn "TX sum of inputs values < sum of output values. Rejected.")
                    (return nil))
                  (let ((fee (- sum-of-inputs sum-of-outputs)))
                    (when (< fee *minimum-transaction-fee*)
                      (emotiq:em-warn "TX transaction fee ~d would be too low. Minimum = ~d. Rejected."
                                      fee
                                      *minimum-transaction-fee*)
                      (return nil)))
                  (emotiq:note "Node ~a declares successful transaction ~a TxID = ~a"
                               (cosi-simgen:current-node)
                               transaction
                               (id-string (id transaction)))
                  (mempool:add-transaction transaction)
                  (return t)))))


(defun output-double-spent-p (id index &key also-search-mempool)
  (when also-search-mempool
    (loop
       :with transaction-seen-p = nil
       :for tx :being :each :hash-value :of cosi-simgen:*mempool*
       :do (loop ;; TODO: make it a simple GETHASH lookup
              :for tx-in :in (inputs tx)
              :as tx-in-id = (input-id tx-in)
              :as tx-in-index = (input-index tx-in)
              :when (and (id= tx-in-id id) (= tx-in-index index))
              ;; earlier spend block found (double spend attempt)
              :do (return-from output-double-spent-p
                    (values t :found-in-mempool)))
       :when (and (not transaction-seen-p) (id= (id tx) id))
       :do (setq transaction-seen-p t)
       :finally
       ;; Can stop search (not a double-spend) if found
       ;; transaction in mempool, evidently a UTXO.  Must go
       ;; through whole loop, cannot exit -- mempool unordered,
       ;; unlike the blockchain (cf below)
       (when transaction-seen-p
         (return-from output-double-spent-p
           (values nil :utxo-in-mempool)))))
  (do-all-transactions (tx)
    (loop
       :for tx-in :in (inputs tx)
       :as tx-in-id = (input-id tx-in)
       :as tx-in-index = (input-index tx-in)
       :when (and (id= tx-in-id id) (= tx-in-index index))
       ;; earlier spend block found (double spend attempt)
       :do (return-from output-double-spent-p
             (values t :found-on-blockchain)) ; as opposed to mempool
       ;; Can stop search (not a double-spend) when reach ID,
       ;; indicating we have an usnpent transaction output
       ;; (UTXO)
       (when (id= (id tx) id)
         (emotiq:note "Hey look: the TxID  ~a is the same as the arg TxID  ~a Wow!"
                      (id-string (id tx))
                      (id-string id))
         (return-from output-double-spent-p
           (values nil :utxo-on-blockchain))))))


;;; UTXO-P: search the blockchain exhaustively starting with the
;;; most-recently-created transaction returning true if the output of
;;; TRANSACTION at OUTPUT-INDEX is found to have been spent, and false (nil) if
;;; TRANSACTION itself or the beginning of the blockchain is reached.
(defun utxo-p (transaction output-index)
  "Return whether TRANSACTION's output at index OUTPUT-INDEX has been spent. As
   a second value, returns whether the transaction was found at all."
  (let ((this-transaction-id (id transaction)))
    (do-blockchain (block)
      (do-transactions (tx block)
        (if (eq tx transaction)
            ;; We reached all the way from the latest in time to this
            ;; transaction, and have not found a transaction that spends this
            ;; transaction, so return now that this is unspent/found.
            (return-from utxo-p (values t t))
            ;; We are checking every transaction T-L that's later than the arg
            ;; transaction T-E to see if T-L spends T-E's output at index
            ;; OUTPUT-INDEX.  Check all T-E's
            (loop
               :for transaction-input :in (inputs tx)
               :as index = (input-index transaction-input)
               :when (and (eql index output-index)
                          (id= (input-id transaction-input) this-transaction-id))
               ;; spent/found
               :do (return-from utxo-p (values nil t))))))
    ;; degenerate case: got to end, but transaction not even found
    (values t nil)))


(defmethod compute-fee ((tx txn:transaction))
  "Compute the transaction fee on TRANSACTION, a transaction instance
   that has been fully validated and comes from the mempool, and this
   therefore makes all the assumptions as to validations, such as UTXO
   exists, no double spends, valid amounts, etc. The fee is computed
   as the sum of the inputs minus the sum of the outputs.  In our
   blockchain system the leader node creates a so-called collect
   transaction as the first transaction of every block, except the
   genesis, and directs that it be sent to an address of their
   choosing."
  (loop
     :for tx-in :in (inputs tx)
     :as id = (input-id tx-in)
     :as index = (input-index tx-in)
     :as input-tx = (retrieve id :also-search-mempool t)
     :as input-tx-outputs = (if (null input-tx)
                                (error "no match TxID = ~a" (id-string id))
                                (outputs input-tx))
     :as utxo = (or (nth index input-tx-outputs)
                    (error "no match index = ~a" index))
     :as input-subamount = (output-amount utxo)
     :sum input-subamount :into sum-of-inputs
     :finally (let* ((sum-of-outputs (sum-outputs tx))
                     (fee (- sum-of-inputs sum-of-outputs)))
                (when (< fee 0)       ; error if it happens here
                  (error "Negative fee: ~a! sum of inputs (~a) < sum of outputs (~a)."
                         fee sum-of-inputs sum-of-outputs))
                (return fee))))


;;;; DEBUG Waiting for Transactions

(defparameter *wait-for-tx-count-tick-print* ".")
(defparameter *wait-for-tx-count-tick-every* 10)
(defparameter *wait-for-tx-default-tx-types* '(:spend :spend-cloaked))

(defun spend-tx-types-p (tx-types)
  (equal *wait-for-tx-default-tx-types*
         (sort (copy-list tx-types) #'string<)))

(defun wait-for-tx-count (n &key interval timeout node tx-types
                                 tick-print tick-every)
  "Alternate sleeping for a small INTERVAL while waiting for there to
   have been a total of N spend transactions (or possibly other types
   if TX-TYPES is specified) on the blockchain. INTERVAL defaults to
   about 1/10th of a second, and a user-specified value is restricted
   to a reasonable range > 0 and <= 1 second.  The blockchain is with
   respect to NODE, which can be a specified node, and otherwise
   defaults to the current node, if non-nil, and otherwise the top
   node. A timeout occurs when TIMEOUT, which defaults to nil, has
   been supplied non-nil, in which case it should ben an interval in
   seconds. Then, when that amount of time has elapsed, a timeout has
   been reached, and the function returns.  If the count is reached,
   this returns true; if a timeout occurs, this returns nil;
   otherwise, this does not return, and simply continues alternating
   between checking the count of transactions and sleeping for brief
   intervals. If TX-TYPES is specified, it should be a list of
   transaction types to wait for; otherwise, it defaults to spend
   transactions, both cloaked and uncloaked."
  (when interval
    (setf interval (max 1 (min 1/1000 interval))))
  (setf tick-print (or tick-print *wait-for-tx-count-tick-print*)
        tick-every (or tick-every *wait-for-tx-count-tick-every*)
        tx-types (or tx-types *wait-for-tx-default-tx-types*))
  (let ((start-ut (get-universal-time))
        (interval (or interval 1/10))
        (cosi-simgen:*current-node* (or node 
                                        cosi-simgen:*current-node*
                                        cosi-simgen:*top-node*)))
    (emotiq:note "Awaiting ~d ~a txs."
                 n                   
                 (if (spend-tx-types-p tx-types) 
                     "spend"
                     (format nil "[transaction types = ~s]" tx-types)))
    (flet ((count-txs ()
             (blockchain:count-transactions)))
      (loop
         :when (>= (count-txs) n) :return (values t (- (get-universal-time) start-ut))
         :when (and timeout (>= (- (get-universal-time) start-ut) timeout)) :return nil
         :do (sleep interval)))))
         
;; The counting method is inefficient, since it counts all the way
;; from the beginning each time. Only OK for low-transaction-count
;; early blockchains, OK for now. Improve later! -mhd, 6/22/18
