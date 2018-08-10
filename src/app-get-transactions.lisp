(in-package :emotiq/app)

;; this is test code only - striving for obviousness, not efficiency (probably only needs a single pass throught blocks/txns)

#|

Transactions need:

For a given account, give a list of all transactions to or from the address of the account.

For each transaction

1.	ID of transaction                                                    ==> (cosi/proofs/newtx::transaction-id tx)
2.	type (always SPEND) (to be thorough, you could look at the COLLECT)  ==> (cosi/proofs/newtx::transaction-type tx)

3.	Epoch that transaction is part of                                    ==> not available
                                                                             ==> emotiq:note epoch is actually election N (0 < N < 1)
                                                                             ==> might calculate this by back-tracking through blockchain

4.	Fee paid for the transactions                                        ==> (cosi/proofs/newtx::compute-transaction-fee tx)

For all inputs of transactions

1.	Address that input came from
2.	amount in the input

For all outputs of transaction

1.	Address that was paid
2.	amount that was paid.
|#

(defmethod get-all-transactions-to-given-target-account ((a account))
  "return a list of transactions (CLOS in-memory, local to node) that SPEND (and COLLECT) to account 'a' ; N.B. this only returns transactions that have already been committed to the blockchain (e.g. transactions in MEMPOOL are not considered ; no UTXOs (unspent transactions) ; "
  (let ((account-address (emotiq/txn:address (a, ccount-pkey a)))
        (result nil))
    (cosi-simgen:with-current-node (cosi-simgen:current-node)
      (cosi/proofs/newtx::do-blockchain (block) ;; TODO: optimize this
        (cosi/proofs/newtx::do-transactions (tx block)
          (when (or (eq :COLLECT (cosi/proofs/newtx::transaction-type tx))
                    (eq :SPEND (cosi/proofs/newtx::transaction-type tx)))
            (dolist (out (cosi/proofs/newtx::transaction-outputs tx)) 
              #+nil-might-help-with-debug(emotiq:note "comparing ~a [~a] account-address ~a to transaction-output ~a -> ~a"
                           (account-name a)
                           (emotiq/txn:address (account-triple a))
                           account-address
                           (cosi/proofs/newtx::tx-out-public-key-hash out)
                           (cosi/proofs/newtx::account-addresses= account-address (cosi/proofs/newtx::tx-out-public-key-hash out)))
              (when (cosi/proofs/newtx::account-addresses= account-address (cosi/proofs/newtx::tx-out-public-key-hash out))
                (push tx result)))))))
    result))

(defmethod get-all-transactions-from-given-target-account ((a account))
  "return a list of transactions (CLOS in-memory, local to node) that SPEND (and COLLECT) from account 'a' ; N.B. this only returns transactions that have already been committed to the blockchain (e.g. transactions in MEMPOOL are not considered ; no UTXOs (unspent transactions)"
  (let ((account-address (emotiq/txn:address (account-pkey a)))
        (result nil))
    (cosi-simgen:with-current-node (cosi-simgen:current-node)
      (cosi/proofs/newtx::do-blockchain (block)
        (cosi/proofs/newtx::do-transactions (tx block)
          (when (or (eq :COLLECT (cosi/proofs/newtx::transaction-type tx))
                    (eq :SPEND (cosi/proofs/newtx::transaction-type tx)))
            (dolist (inputs (cosi/proofs/newtx::transaction-inputs tx)) 
              ;; assert (all inputs point to the same address)
              (when (cosi/proofs/newtx::account-addresses= account-address (cosi/proofs/newtx::tx-in-id
                (push tx result)))))))
    result))

(defmethod get-address-of-sender ((tx cosi/proofs/newtx::transaction))
  "return address of the wallet that created an input to this transaction"
  ;; src/Cosi-BLS/new-transactions.lisp/transaction-input has a field %tx-in-public-key, which might be what is needed, but w/o documentation
  ;; reach back to the transaction that created this input, return the address given in ITS input(s)
  ;; being careful about COINBASE (which has no inputs)
  ;; I'm not unwinding the input fully - it points back to a transaction output and index - index not needed, we
  ;; just need to know who sent this
  (let ((prev-transaction-id (tx-in-id (first (cosi/proofs/newtx::transaction-inputs tx)))))
    (let ((prev-tx (get-transaction-by-id prev-transaction-id)))
      (if (eq :coinbase (cosi/proofs/newtx::transaction-type prev-tx))
          (address-of-genesis)
        (input-address (first (cosi/proofs/newtx::transaction-inputs prev-tx)))))))
