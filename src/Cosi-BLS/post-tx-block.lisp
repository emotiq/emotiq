
(in-package :cosi/proofs)

(defmethod get-transaction-id ((transaction cosi/proofs/newtx:transaction))
  "Get the identifier of TRANSACTION, an octet vector of length 32, which can be
   used to hash the transactions leaves of the merkle tree to produce the
   block's merkle tree root hash. It represents H(PubKey, PedComm), or possibly
   a superset thereof."
  (if *newtx-p*
      (cosi/proofs/newtx:transaction-id transaction)
      (vec-repr:bev-vec (hash:hash-val (hash-transaction transaction)))))

;; In Bitcoin this is known as the TXID of the transaction.



(defmethod hash-transaction ((transaction cosi/proofs/newtx:transaction))
  "Produce a hash for TRANSACTION, including the pair (PubKey, PedComm).
   The resulting hash's octet vector is usable as a transaction ID."
  (hash-256 transaction))



