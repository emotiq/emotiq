(in-package :emotiq/block)


(defun new-transactions (&key max)
  "Selects transactions from mempool for new block."
  (let ((txns (sort (mempool:get-transactions 'vector) #'txn:precedes-p)))
    (when (and max (> max 0) (> (length txns) max))
      (setf txns (subseq txns 0 max)))
    (emotiq:note "~D Transactions for new block" (length txns))
    (let* ((total-fee (loop :for tx :across txns :sum (txn:compute-fee tx)))
           (collect-transaction (txn:make-collect-transaction total-fee)))
      (cons collect-transaction
            (coerce txns 'list)))))


(defun check-transactions-hash (blk)
  (hash:hash= (merkle-root-hash blk)
              (compute-merkle-root-hash blk)))

(defun check-transactions (blk)
  "Return nil if invalid block. This is run by a CoSi block 
   validator. Validate by recomputing the full merkle hash on all the 
   transactions and comparing with that with that saved in the 
   merkle-root-hash slot of the block." 
  (and (hash:hash=
        (compute-merkle-root-hash blk) 
        (merkle-root-hash blk))
       (hash:hash=  
        (compute-input-script-merkle-root-hash blk)
        (input-script-merkle-root-hash blk))
       (hash:hash=  
        (compute-witness-merkle-root-hash blk)
        (witness-merkle-root-hash blk))))
