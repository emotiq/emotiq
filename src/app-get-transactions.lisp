(in-package :emotiq/app)

(defmethod get-all-transactions-to-given-target-account ((a account))
  "return a list of transactions that SPEND (and COLLECT) to account 'a' ; N.B. this only returns transactions that have already been committed to the blockchain (e.g. transactions in MEMPOOL are not considered ; no UTXOs (unspent transactions)"
  (let ((account-address (emotiq/txn:address (account-pkey a)))
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