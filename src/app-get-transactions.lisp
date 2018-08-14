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
  (let ((account-address (emotiq/txn:address (account-triple a))))
    (get-all-transactions-to-given-target-account account-address)))
        
(defmethod get-all-transactions-to-given-target-account ((a pbc:keying-triple))
  (get-all-transactions-to-given-target-account (emotiq/txn:address a)))
                                                       
(defmethod get-all-transactions-to-given-target-account ((account-address pbc:address))
  "return a list of transactions (CLOS in-memory, local to node) that SPEND (and COLLECT) to account 'a' ; N.B. this only returns transactions that have already been committed to the blockchain (e.g. transactions in MEMPOOL are not considered ; no UTXOs (unspent transactions) ; "
  ;; FYI - there is one transaction from alice to bob and one transaction from bob to mary - this second transaction 
  ;; produces "change" back to bob, hence, there are two transactions TO bob (one from alice and one from bob)
  (let ((result nil))
    (emotiq/app::with-current-node
     (cosi/proofs/newtx:do-blockchain (block) ;; TODO: optimize this
       (let ((timestamp (cosi/proofs::block-timestamp block))
             (epoch (get-epoch block)))
         (cosi/proofs/newtx:do-transactions (tx block)
           (let ((kind (cosi/proofs/newtx:transaction-type tx)))
             (when (or (eq :COLLECT kind)
                       (eq :SPEND kind))
               (let ((fee (if (eq :COLLECT kind)
                              0
                            (fee-paid-for-transaction tx))))
                 (dolist (out (cosi/proofs/newtx:transaction-outputs tx)) 
                   (when (cosi/proofs/newtx:account-addresses= account-address (cosi/proofs/newtx:tx-out-public-key-hash out))
                     (push (list tx timestamp epoch kind fee) result))))))))))
       result))


(defmethod get-all-transactions-from-given-target-account ((a account))
  (get-all-transactions-from-given-target-account (emotiq/txn:address (account-pkey a))))

(defmethod get-all-transactions-from-given-target-account ((a pbc:keying-triple))
  (get-all-transactions-to-given-target-account (emotiq/txn:address a)))
                                                       
(defmethod get-all-transactions-from-given-target-account ((account-address pbc:address))
  "return a list of transactions (CLOS in-memory, local to node) that SPEND (and COLLECT) from account 'a' ; N.B. this only returns transactions that have already been committed to the blockchain (e.g. transactions in MEMPOOL are not considered ; no UTXOs (unspent transactions)"
  ;; FYI - there should be ONE transaction FROM bob, with two outputs (one back to bob)
  (let ((result nil))
    (emotiq/app::with-current-node
      (cosi/proofs/newtx:do-blockchain (block)
       (let ((timestamp (cosi/proofs::block-timestamp block))
             (epoch (get-epoch block)))
         (cosi/proofs/newtx:do-transactions (tx block)
           (let ((kind (cosi/proofs/newtx:transaction-type tx)))
             (when (or (eq :COLLECT kind)
                       (eq :SPEND kind))
               (let ((fee (if (eq :COLLECT kind)
                              0
                            (fee-paid-for-transaction tx))))
                 (let ((inputs (cosi/proofs/newtx:transaction-inputs tx))) 
                   ;; assert (all inputs point to the same address)
                   (let ((any-single-input (first inputs)))
                     (when (cosi/proofs/newtx:account-addresses= account-address (input-address any-single-input))
                       (push (list tx timestamp epoch kind fee) result)))))))))))
    result))

(defmethod get-address-of-sender ((tx cosi/proofs/newtx:transaction))
  "return address of the wallet that created an input to this transaction"
  ;; src/Cosi-BLS/new-transactions.lisp/transaction-input has a field %tx-in-public-key, which might be what is needed, but w/o documentation
  ;; reach back to the transaction that created this input, return the address given in ITS input(s)
  ;; being careful about COINBASE (which has no inputs)
  ;; I'm not unwinding the input fully - it points back to a transaction output and index - index not needed, we
  ;; just need to know who sent this
  (if (eq :coinbase (cosi/proofs/newtx:transaction-type tx))
      (address-of-genesis) ;; special case for genesis
    (let ((prev-transaction-id (cosi/proofs/newtx:tx-in-id (first (cosi/proofs/newtx:transaction-inputs tx)))))
      (let ((prev-tx (find-transaction-per-id prev-transaction-id)))  ;; inefficient
        (if (eq :coinbase (cosi/proofs/newtx:transaction-type prev-tx))
            (address-of-genesis)
          (input-address (first (cosi/proofs/newtx:transaction-inputs prev-tx))))))))

(defmethod get-address-of-sender-to-input ((in cosi/proofs/newtx:transaction-input))
  "given an input, return address of who sent it"
  (let ((prev-txn-id (cosi/proofs/newtx:tx-in-id in))
        (index (cosi/proofs/newtx:tx-in-index in)))
    (if (= -1 index)
        (address-of-genesis)
      (let ((prev-txn (find-transaction-per-id prev-txn-id)))  ;; inefficient
        (get-address-of-sender prev-txn)))))

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
        (index (cosi/proofs/newtx:tx-in-index in)))
    (let ((prev-txn (find-transaction-per-id prev-txn-id)))  ;; inefficient
      (let ((outlist (cosi/proofs/newtx:transaction-outputs prev-txn)))  ;; sequence of outputs
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
  ;; FYI: *blockchain* contain a single hashed (hash-block) eblock
  ;; block-list returns a list of (not hashed) eblocks
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

(defmethod get-transactions ((a account))
  (append (get-all-transactions-to-given-target-account a)
          (get-all-transactions-from-given-target-account a)))

(defmethod get-transactions ((kt pbc:keying-triple))
  (append (get-all-transactions-to-given-target-account kt)
          (get-all-transactions-from-given-target-account kt)))
          

(defun address-of-genesis ()
  (emotiq/txn:address (emotiq/app::account-triple emotiq/app::*genesis*)))

(defun stubbed-out-get-transactions-from-chain (keyingtriple)
  (emotiq-rest:as-json (get-transactions keyingtriple)))

(defun get-transactions-from-chain ()
  (emotiq-rest:as-json (test-alist *james*)))

