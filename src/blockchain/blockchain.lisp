(in-package :emotiq/blockchain)


(defmethod get-utxos ((account pbc:keying-triple))
  (get-utxos (wallet:address account)))
(defmethod get-utxos ((address string))
  (let ((cosi-simgen:*current-node* cosi-simgen:*top-node*))
    (%get-utxos address)))

(defun %get-utxos (address)
  "Return the UTXOs for ADDRESS as a list of two elements of the form

     (TXO OUTPOINT AMT)

   Here, TXO is a transaction-output instance that pays ADDRESS, and
   OUTPOINT is a list of the form

     (TxID INDEX)

   where TxID is the transaction ID of the parent transaction, i.e.,
   the transaction containing TXO, and INDEX is the zero-based index
   of TXO in the sequence of the parent's transaction outputs. And
   where AMT is an integer amount of coin subunits.

   ADDRESS here is taken to mean the same thing as the public key
   hash."
  (let ((utxos-so-far '()))
    (do-blockchain (block)
      (do-transactions (tx block)
        (loop
           :for txo :in (txn:outputs tx)
           :as to-address = (txn:output-address txo)
           :as index :from 0
           :when (and (wallet:address= to-address address) (txn:utxo-p tx index))
           :collect `(,txo (,(txn:id tx) ,index) ,(txn:output-amount txo))
           :into result
           :finally (setq utxos-so-far
                          (nconc utxos-so-far result)))))
    utxos-so-far))


