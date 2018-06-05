
(in-package :cosi-simgen)

(defun get-most-recent-block ()
  "Get the current head of the blockchain"
  (NYI "get-most-recent-block"))

(defun block-prev (blk)
  "Get hash ID of previous block, or nil if already in genesis block"
  (NYI "blk-prev"))

(defun block-transactions (blk)
  "Get list of transactions in blk"
  (NYI "blk-transactions"))

(defun block-witness-add-keys (blk)
  "Get list of witness keys added in this block epoch"
  (NYI "blk-add-keys"))

(defun block-witness-rem-keys (blk)
  "Get list of witness keys removed in this block epoch"
  (NYI "blk-rem-keys"))

(defun block-hash (blk)
  "Get block hash stored in blk"
  (NYI "blk-hash"))

(defun hash-block (blk)
  "Compute the hash code for this block"
  (NYI "hash-block"))

(defun get-block (blkid)
  "Fetch block for blkid"
  (NYI "get-block"))

;; ---------------------------------------------------

(defstruct utxo-record
  block trans)  ;; where created

(defvar *utxos*  (make-hash-table  ;; table of unspent TXOUT's
                  :test 'equalp))

(defstruct chain-block
  block       ;; ptr to block structure
  witnesses)  ;; list of witness pkeys in block's epoch

(defvar *blockchain* nil) ;; linear list of chain-block records
(defvar *witnesses*  nil) ;; linear list of witeness pkeys

#|
 ;; This version is for witness delta lists. See below for plain witness lists
(defun reconstruct-blockchain ()
  ;; reset state to beginning
  (setf *blockchain* nil
        *witnesses*  nil)
  ;; fetch entire chain of blocks
  (um:nlet-tail iter ((blk  (get-most-recent-block)))
    (when blk
      ;; check that blkid = Hash(blk)
      (assert (hash= (block-hash blk)
                     (hash-block blk)))
      (push (make-chain-block
             :block blk)
            *blockchain*)
      (iter (get-block (block-prev blk)))))
  ;; rebuild the witness list for each block
  ;; and the outstanding UTXO's
  (dolist (cblk *blockchain*)
    (let* ((blk     (chain-block-block cblk))
           (txs      (block-transactions blk))
           (addkeys  (block-witness-add-keys blk))
           (remkeys  (block-witness-rem-keys blk)))
      (dolist (pkey remkeys)
        (unless (find (int pkey) *witnesses*
                      :key 'int)
          (error "Removal of witness that wasn't present"))
        (setf *witnesses* (delete pkey *witnesses*)))
      (dolist (pkey addkeys)
        (when (find (int pkey) *witnesses*
                    :key 'int)
          (error "Addition of witness that was already present")))
      (setf *witnesses* (append addkeys *witnesses*)
            (chain-block-witnesses cblk) *witnesses*)
      ;; now examine UTXO's
      (dolist (tx txs)
        (dolist (txout (cosi/proofs:trans-txouts tx))
          (let ((key (cosi/proofs:txout-hashlock txout)))
            (setf (gethash key *utxos*)
                  (make-utxo-record
                   :block blk
                   :trans tx))
            ))
        (dolist (txin (cosi/proofs:trans-txins tx))
          (let ((key (cosi/proofs:txin-hashlock txin)))
            (if (gethash key *utxos*)
                (remhash key *utxos*)
              ;; else
              (error "Can't happen: Spend of nonexistent UTXO"))
            ))
        )))
  (setf *blockchain* (nreverse *blockchain*)))
|#
;; ---------------------------------------------------------------------------
;; This version is for plain witness lists in each block

(defun reconstruct-blockchain ()
  ;; reset state to beginning
  (setf *blockchain*
        (um:accum acc
          ;; fetch entire chain back to genesis block
          (um:nlet-tail iter ((blk  (get-most-recent-block)))
            (when blk
              ;; check that blkid = Hash(blk)
              (assert (vec= (block-hash blk)
                            (hash-block blk)))
              (acc blk)
              (iter (get-block (block-prev blk))))))
        *witnesses*  (and *blockchain*
                          (block-witnesses (car *blockchain*)))
        )
  ;; reconstruct the outstanding utxo's
  (clrhash *trans-cache*)
  (clrhash *utxo-table*)
  (dolist (blk (reverse *blockchain*))
    (dolist (tx (block-transactions blk))
      (dolist (txin (reverse (trans-txins tx)))
        (let ((key (txins-hashlock txin)))
          (assert (eql :spendable (gethash key *utxo-table*)))
          (remhash key *utxo-table*)))
      (dolist (txout (trans-txouts tx))
        (setf (gethash (txout-hashlock txout) *utxo-table*) :spendable))
      )))
