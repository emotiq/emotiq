(in-package :emotiq/app)

;; this is test code only - striving for obviousness, not efficiency (probably only needs a single pass through blocks/txns)

#|

Transactions need:

For a given account, give a list of all transactions to or from the address of the account.

For each transaction

1.	ID of transaction                                                    ==> (cosi/proofs/newtx:transaction-id tx)
2.	type (always SPEND) (to be thorough, you could look at the COLLECT)  ==> (cosi/proofs/newtx:transaction-type tx)

3.	Epoch that transaction is part of                                    ==> not available
                                                                             ==> emotiq:note epoch is actually election N (0 < N < 1)
                                                                             ==> might calculate this by back-tracking through blockchain

4.	Fee paid for the transactions                                        ==> (cosi/proofs/newtx:compute-transaction-fee tx)

For all inputs of transactions

1.	Address that input came from                                         ==> (address-of-sender current-tx)
2.	amount in the input                                                  ==> (amount-in-input in)

For all outputs of transaction

1.	Address that was paid                                                ==> (cosi/proofs/newtx:tx-out-public-key-hash out)
2.	amount that was paid.                                                ==> (cosi/proofs/newtx:tx-out-amount out)
|#

(defmethod get-all-transactions-to-given-target-account ((a account))
  "return a list of transactions (CLOS in-memory, local to node) that SPEND (and COLLECT) to account 'a' ; N.B. this only returns transactions that have already been committed to the blockchain (e.g. transactions in MEMPOOL are not considered ; no UTXOs (unspent transactions) ; "
  (let ((account-address (emotiq/txn:address (account-pkey a)))
        (result nil))
    (emotiq/app::with-current-node
      (cosi/proofs/newtx:do-blockchain (block) ;; TODO: optimize this
        (cosi/proofs/newtx:do-transactions (tx block)
          (when (or (eq :COLLECT (cosi/proofs/newtx:transaction-type tx))
                    (eq :SPEND (cosi/proofs/newtx:transaction-type tx)))
            (dolist (out (cosi/proofs/newtx:transaction-outputs tx)) 
              #+nil-might-help-with-debug(emotiq:note "comparing ~a [~a] account-address ~a to transaction-output ~a -> ~a"
                           (account-name a)
                           (emotiq/txn:address (account-triple a))
                           account-address
                           (cosi/proofs/newtx:tx-out-public-key-hash out)
                           (cosi/proofs/newtx:account-addresses= account-address (cosi/proofs/newtx:tx-out-public-key-hash out)))
              (when (cosi/proofs/newtx:account-addresses= account-address (cosi/proofs/newtx:tx-out-public-key-hash out))
                (push tx result)))))))
    result))

(defmethod get-all-transactions-from-given-target-account ((a account))
  "return a list of transactions (CLOS in-memory, local to node) that SPEND (and COLLECT) from account 'a' ; N.B. this only returns transactions that have already been committed to the blockchain (e.g. transactions in MEMPOOL are not considered ; no UTXOs (unspent transactions)"
  (let ((account-address (emotiq/txn:address (account-pkey a)))
        (result nil))
    (emotiq/app::with-current-node
      (cosi/proofs/newtx:do-blockchain (block)
        (cosi/proofs/newtx:do-transactions (tx block)
          (when (or (eq :COLLECT (cosi/proofs/newtx:transaction-type tx))
                    (eq :SPEND (cosi/proofs/newtx:transaction-type tx)))
            (let ((inputs (cosi/proofs/newtx:transaction-inputs tx))) 
              ;; assert (all inputs point to the same address)
              (let ((any-single-input (first inputs)))
                (when (cosi/proofs/newtx:account-addresses= account-address (input-address any-single-input))
                  (push tx result))))))))
    result))

(defmethod get-address-of-sender ((tx cosi/proofs/newtx:transaction))
  "return address of the wallet that created an input to this transaction"
  ;; src/Cosi-BLS/new-transactions.lisp/transaction-input has a field %tx-in-public-key, which might be what is needed, but w/o documentation
  ;; reach back to the transaction that created this input, return the address given in ITS input(s)
  ;; being careful about COINBASE (which has no inputs)
  ;; I'm not unwinding the input fully - it points back to a transaction output and index - index not needed, we
  ;; just need to know who sent this
  (let ((prev-transaction-id (cosi/proofs/newtx:tx-in-id (first (cosi/proofs/newtx:transaction-inputs tx)))))
    (let ((prev-tx (find-transaction-per-id tx-in-id)))  ;; inefficient
      (if (eq :coinbase (cosi/proofs/newtx:transaction-type prev-tx))
          (address-of-genesis)
        (input-address (first (cosi/proofs/newtx:transaction-inputs prev-tx)))))))

(defmethod input-address ((in cosi/proofs/newtx:transaction-input))
  "return address of this input by reaching back to the output which is connected to it"
  (let ((prev-txn-id (cosi/proofs/newtx:tx-in-id in))
        (index (cosi/proofs/newtx:tx-in-index in)))
    (if (= -1 index)
        (address-of-genesis)
      (let ((prev-txn (find-transaction-per-id prev-txn-id)))  ;; inefficient
        (let ((outlist (cosi/proofs/newtx:transaction-outputs prev-txn)))  ;; sequence of outputs
          (let ((out (nth index outlist)))  ;; index and grab appropriate output
            (cosi/proofs/newtx:tx-out-public-key-hash out)))))))  ;; TODO: check if this is the correct use of this field

(defmethod amount-in-input ((in cosi/proofs/newtx:transaction-input))
  "return amount sent to this input by an output from a txn"
  (let ((prev-txn-id (cosi/proofs/newtx:tx-in-id in))
        (index (cosi/proofs/newtx:tx-in-index)))
    (let ((prev-txn (find-transaction-per-id prev-txn-id)))  ;; inefficient
      (let ((outlist (cosi/proofs/newtx:transaction-outputs)))  ;; sequence of outputs
        (let ((out (nth index outlist)))  ;; index and grab appropriate output
          (cosi/proofs/newtx:tx-out-amount out))))))  ;; return amount from that particular output

(defun find-transaction-per-id (txid)
  ;; inefficient - this calls do-blockchain and do-transactions
  (cosi/proofs/newtx:find-transaction-per-id txid))

(defmethod get-txid ((tx cosi/proofs/newtx:transaction))
  (cosi/proofs/newtx:transaction-id tx))

(defmethod get-tx-type ((tx cosi/proofs/newtx:transaction))
  (cosi/proofs/newtx:transaction-type tx))

(defmethod get-epoch ((eblock (eql nil)))
  0)

(defmethod get-epoch ((eblock cosi/proofs:eblock))
  ;; very inefficient
  ;; cosi-simgen::block-list produces a list of all the blocks in a node's blockchain
  ;; it takes an optional parameter which represents the block to start with (instead of
  ;; the full blockchain)J
  (let ((hblock (cosi/proofs:hash-block eblock)))
    (let ((blocks-from (cosi-simgen::block-list hblock)))
      (1- (length blocks-from)))))
     
(defun test-epoch ()
  "should return the epoch of every block in *blocks*"
  (mapcar #'emotiq/app::get-epoch emotiq/app:*blocks*))
    

(defmethod fee-paid-for-transaction ((tx cosi/proofs/newtx:transaction))
  (cosi/proofs/newtx:compute-transaction-fee tx))

(defmethod payee-address ((out cosi/proofs/newtx:transaction-output))
  (cosi/proofs/newtx:tx-out-public-key-hash out))

(defmethod amount-paid ((out cosi/proofs/newtx:transaction-output))
  (cosi/proofs/newtx:tx-out-amount out))

(defmethod get-transactions ((a account)) ;;TODO duplicates
  (append (get-all-transactions-to-given-target-account a)
          (get-all-transactions-from-given-target-account a)))

(defun address-of-genesis ()
  (emotiq/txn:address (emotiq/app::account-triple emotiq/app::*genesis*)))