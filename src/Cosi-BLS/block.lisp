;;;; block.lisp

(in-package :cosi/proofs)




                                                                                ;
(defclass eblock ()
  ((protocol-version
    :initform 1
    :documentation 
      "Version of the protocol/software, an integer.")

   (epoch                               ; aka "height"
    :reader  block-epoch
    :initarg :epoch
    :initform 0
    :documentation
    "Increasing integer count of epoch this block is part of. We keep
     a single chain combining identity and ledger, unlike Byzcoin,
     which keeps an outside identity blockchain with its epoch count
     and an inside ledger blockchain with its height count. Thus,
     epoch subsumes height.")

   (prev-block-hash
    :reader block-prev-block-hash
    :initform nil
    :documentation "Hash of previous block (nil for genesis block).")

   (timestamp
    :reader block-timestamp
    :documentation 
      "Approximate creation time in seconds since Unix epoch. The time zone is UTC.")

   (leader-pkey
    :reader block-leader-pkey
    :documentation
    "Public key for the leader for this epoch.")

   (election-proof
    :reader block-election-proof)

   (signature
    :initform nil
    :reader block-signature
    :documentation
    "A signature over the whole block authorizing all transactions.")
   (signature-pkey
    :initform nil
    :reader block-signature-pkey
    :documentation
    "Public key for block-signature")
   (signature-bitmap
    :reader block-signature-bitmap
    :initform 0
    :documentation 
    "Use methods ith-witness-signed-p and set-ith-witness-signed-p OR,
     to access directly, supply a bitmap represented by an
     integer (potentially a bignum), where each position of the bitmap
     corresponds to a vector index, such that its state tells you
     whether that particular potential witness signed.")

   (witnesses
    :initform nil
    :reader block-witnesses
    :documentation
    "Sequence of public keys of validators 1:1 w/signature-bitmap slot.")

   (merkle-root-hash
    :reader block-merkle-root-hash
    :documentation "Merkle root hash of transactions.")

   ;; Transactions is generally what's considered the main contents of a block
   ;; whereas the rest of the above comprises what's known as the 'block header'
   ;; information.
   (transactions
    :reader block-transactions
    :documentation
    "For now a sequence of transactions. Later, this may be changed to
    be a merkle tree or simply allowed to be either a sequence or a
    merkle tree as an alternative representation "))
  (:documentation "A block on the Emotiq blockchain."))



;;; NB: in case of changes to eblock's slots, keep the following
;;; variable in sync.

(defparameter *names-of-block-slots-to-serialize*
  '(protocol-version 
    epoch
    prev-block-hash
    timestamp

    leader-pkey

    election-proof

    signature
    signature-pkey
    signature-bitmap

    witnesses
    
    merkle-root-hash)
  "These slots are serialized and then hashed. The hash is stored as
   the block-hash on the current block and the prev-block-hash on a
   newer block on the blockchain.")


(defun serialize-block-octets (block)
  "Return a serialization of BLOCK as a list of octet vectors for the slots in
   \*names-of-block-slots-to-serialize*. It is an error to call this before all
   those slots are bound. This is to be used to hash a previous block, i.e., on
   that has been fully formed and already been added to the blockchain."
  (loop for slot-name in *names-of-block-slots-to-serialize*
        collect (loenc:encode (slot-value block slot-name))))


(defparameter *names-of-block-header-slots-to-serialize*
  '(protocol-version 
    epoch
    prev-block-hash
    timestamp

    leader-pkey

    election-proof

    witnesses
    
    merkle-root-hash)
  "These slots are serialized and then hashed. The hash is stored as
   the block-hash on the current block and the prev-block-hash on a
   newer block on the blockchain. These slots represent the block header only.")

(defun serialize-block-header-octets (block)
  "Return a serialization of BLOCK header as a list of octet vectors
for the slots in *names-of-block-slots-to-serialize*. It is an error
to call this before all slots are bound. This is to be used to hash a
previous block, i.e., on that has been fully formed and already been
added to the blockchain."
  (loop for slot-name in *names-of-block-header-slots-to-serialize*
        collect (loenc:encode (slot-value block slot-name))))


(defvar *unix-epoch-ut* (encode-universal-time 0 0 0 1 1 1970 0)
  "The Unix epoch as a Common Lisp universal time. (Note: implied time
  zone is UTC.)")



;;; CREATE-BLOCK: TRANSACTIONS should be a sequence of TRANSACTION instances.
;;; The order of the elements of TRANSACTIONS is fixed in the block once the
;;; block is created and cannot be changed. The order is somewhat flexible
;;; except for the following partial ordering constraints:
;;;
;;;   (1) a coinbase transaction must come first; and
;;;
;;;   (2) for any two transactions Tx1, Tx2, if the input of Tx2 spends the
;;;   output of Tx1, Tx1 must come before Tx2.
;;;
;;; This function does not check the order. However, validators check the order,
;;; and they can require this order for a block to be considered valid.
;;; Software may also rely upon this order, e.g., as a search heuristic.

(defun create-block (prev-block? block-election-proof block-leader-pkey
                     block-witnesses block-transactions)
  (let ((blk (make-instance 'eblock)))
    (with-slots (merkle-root-hash timestamp election-proof 
                 leader-pkey witnesses transactions)
        blk
      (setf timestamp (- (get-universal-time) *unix-epoch-ut*))
      (setf election-proof block-election-proof)
      (setf leader-pkey block-leader-pkey)
      (setf witnesses block-witnesses)
      (setf transactions block-transactions)
      (setf merkle-root-hash (compute-merkle-root-hash block-transactions))
      (when prev-block? 
        (with-slots (epoch prev-block-hash)
            blk
          (setf epoch (1+ (slot-value prev-block? 'epoch)))
          (setf prev-block-hash (hash-block prev-block?))))
      blk)))


(defun hash-256 (&rest hashables)
  "This is the hashing used in our block in a merkle tree, linking
   transaction outputs and inputs, and hashing the block header. This
   uses SHA-3 for hashing; therefore, we do not use double hashing as
   in Bitcoin."
  (apply #'hash:hash/256 hashables))


(defun hash-block (block)
  "Applies hash-256 to result of serialize-block-octets, q.v."
  ;; Ends up doing the equivalent of
  ;;
  ;;   (hash-256  #<bv 1>  #<bv 2>  ...  #<bv N>)
  (apply #'hash-256 (serialize-block-octets block)))



;;; COMPUTE-MERKLE-ROOT-HASH: construct a merkle root for a sequence of
;;; TRANSACTIONS according to bitcoin.org Bitcoin developer reference doc, here:
;;; https://bitcoin.org/en/developer-reference#block-versions

(defun compute-merkle-root-hash (transactions)
  (compute-merkle
   ;; transactions is a sequence, i.e., list or vector
   (if (consp transactions)
       ;; (optimized for list case)
       (loop for tx in transactions
             as tx-out-id = (get-transaction-id tx)
             collect tx-out-id)
       (loop for i from 0 below (length transactions)
             as tx = (elt transactions i)
             as tx-out-id = (get-transaction-id tx)
             collect tx-out-id))))



(defun get-transaction-id (transaction)
  "Get the identifier of TRANSACTION, an octet vector of length 32, which can be
   used to hash the transactions leaves of the merkle tree to produce the
   block's merkle tree root hash. It represents H(PubKey, PedComm), or possibly
   a superset thereof."
  (vec-repr:bev-vec (hash:hash-val (hash-transaction transaction))))

;; In Bitcoin this is known as the TXID of the transaction.



(defun hash-transaction (transaction)
  "Produce a hash for TRANSACTION, including the pair (PubKey, PedComm).
   The resulting hash's octet vector is usable as a transaction ID."
  (hash-256 transaction))



(defun compute-merkle (nodes)
  "Compute merkle root hash on nonempty list of hashables NODES."
  (cond
    ((null nodes)                       ; empty blocks permitted
     (hash-256 '()))
    ((null (rest nodes))                ; just 1
     (hash-256 (first nodes)))
    (t (compute-merkle-pairs nodes))))



(defun compute-merkle-pairs (nodes)
  "Compute the merkle root on NODES, a list of two or more hash
   objects."
  (if (null (rest (rest nodes)))
      ;; 2 left
      (hash-256 (first nodes) (second nodes))
      ;; three or more:
      (loop for (a b . rest?) on nodes by #'cddr
            when (and (null rest?) (null b))
              do (setq b a) ; odd-length row case: duplicate last row item
            collect (hash-256 a b)
              into row
            finally (return (compute-merkle-pairs row)))))



(defmethod ith-witness-signed-p (block i)
  "Return true or false (nil) according to whether the ith witness has signed."
  (with-slots (witness-bitmap) block
    (logbitp i witness-bitmap)))

(defmethod set-ith-witness-signed-p (block i signed-p)
  "Set signed-p to either true or false (nil) for witness at position i."
  (with-slots (witness-bitmap) block
    (setf witness-bitmap
          (dpb (if signed-p 1 0)
               (byte 1 i)
               witness-bitmap))))



(defun update-block-signature (block sig bits)
  "Update BLOCK, an eblock instance, slots per SIG, an instance of
   pbc:signed-message, and BITS, a bitmap with format as documented
   for the signature-bitmap slot of eblock."
  (with-slots (signature signature-pkey signature-bitmap)
      block
    (setf signature (pbc:signed-message-sig sig))
    (setf signature-pkey (pbc:signed-message-pkey sig))
    (setf signature-bitmap bits)))
