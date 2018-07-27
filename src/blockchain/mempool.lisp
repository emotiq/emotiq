(in-package :emotiq/mempool)

(defun add-transaction (transaction)
  "Add TRANSACTION to the mempool. It is OK to call this when the
   transaction is already in the mempool.  This returns TRANSACTION as its value."
  (setf (gethash (txn:id transaction) cosi-simgen:*mempool*) transaction))

(defun remove-transaction (transaction &optional (mempool cosi-simgen:*mempool*))
  "Remove TRANSACTION, a transaction instance, from MEMPOOL, and return true if
   there was such an entry or false (nil) otherwise."
  (remhash (txn:id transaction) mempool))

(defun get-transactions (&optional (result-type 'list))
  (coerce (alexandria:hash-table-values cosi-simgen:*mempool*) result-type))

(defun clear-block-transactions (block)
  "Remove transactions that have just been added the block from the mempool that
   is globally bound to cosi-simgen:*mempool*. Note that these transactions may
   not be identical EQ Lisp instances, so even if you're holding a pointer to
   transaction that's in the mempool in your hand, to find the corresponding
   instance in the block, you must compare two instances using their hash keys, not EQ. 
   That also acts as a vefification that the contents are exactly the same, have not undergone 
   any changes after being encoded, transferred from one node to the other over the network, 
   and decoded, etc."
  (let ((mempool cosi-simgen:*mempool*))
    (loop
       :for transaction :in (block:transactions block)
       :count (remove-transaction transaction mempool) :into n-removed
       :finally
         (emotiq:note "Removed ~d transaction~p from mempool." n-removed n-removed)
         (blockchain:dump :mempool t))))
                                 


