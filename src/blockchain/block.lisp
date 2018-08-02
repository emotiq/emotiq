(in-package :emotiq/block)

(um:defconstant+ +unix-epoch-ut+ #.(encode-universal-time 0 0 0 1 1 1970 0)
                 "The Unix epoch as a Common Lisp universal time. (Note: implied time zone is UTC.)")

(defclass eblock ()
  ((protocol-version
    :initform 1
    :documentation "Version of the protocol/software, an integer.")
   (prev-block-hash
    :reader prev-block-hash
    :initform nil
    :documentation "Hash of previous block (nil for genesis block).")
   (timestamp
    :reader timestamp
    :documentation "Approx creation time in seconds since Unix epoch. The time zone is UTC.")
   (leader-pkey
    :reader leader-pkey
    :documentation "Public key for the leader for this epoch.")
   (election-proof
    :reader election-proof)
   (signature
    :initform nil
    :reader signature
    :documentation "A signature over the whole block authorizing all transactions.")
   (signature-bitmap
    :reader signature-bitmap
    :initform 0
    :documentation
    "Use methods ith-witness-signed-p and set-ith-witness-signed-p OR,
     to access directly, supply a bitmap represented by an
     integer (potentially a bignum), where each position of the bitmap
     corresponds to a vector index, such that its state tells you
     whether that particular potential witness signed.")
   (witnesses
    :initform nil
    :reader witnesses
    :documentation "Sequence of public keys of validators 1:1 w/signature-bitmap slot.")
   (witnesses-and-stakes
    :accessor witnesses-and-stakes
    :documentation
    "An a-list of the form

  ((<witness public-key> <stake amount in EMTQ coin/integer>)
  ...)

This is to be set only in the genesis block; otherwise, it should
remain unbound (and should not be referred to). The configuration data
comes from a config file.")             ; ---*** TODO: to be specified. -mhd, 6/28/18
   (merkle-root-hash
    :reader merkle-root-hash
    :documentation "Merkle root hash of transactions.")
   (input-script-merkle-root-hash
    :reader input-script-merkle-root-hash
    :documentation "Merkle root hash of input scripts.")
   (witness-merkle-root-hash
    :reader witness-merkle-root-hash
    :documentation "Merkle root hash of witness data.")
   ;; Transactions is generally what's considered the main contents of a block
   ;; whereas the rest of the above comprises what's known as the 'block header'
   ;; information.
   (transactions
    :reader transactions
    :documentation
    "For now a sequence of transactions. Later, this may be changed to
    be a merkle tree or simply allowed to be either a sequence or a
    merkle tree as an alternative representation "))
  (:documentation "A block on the Emotiq blockchain."))


;;; NB: in case of changes to eblock's slots, keep the following
;;; variable in sync.

(um:defconstant+ +slots-to-serialize+
    '(protocol-version 
      prev-block-hash
      timestamp
      leader-pkey
      election-proof
      signature
      signature-bitmap
      witnesses
      merkle-root-hash)
  "These slots are serialized and then hashed. The hash is stored as
   the block-hash on the current block and the prev-block-hash on a
   newer block on the blockchain.")


(defun serialize (block)
  "Return a serialization of BLOCK as a list of octet vectors for the slots in
   \*names-of-block-slots-to-serialize*. It is an error to call this before all
   those slots are bound. This is to be used to hash a previous block, i.e., on
   that has been fully formed and already been added to the blockchain."
  (loop :for slot-name :in +slots-to-serialize+
     :collect (loenc:encode (slot-value block slot-name))))


(um:defconstant+ +header-slots-to-serialize+
    '(protocol-version 
      prev-block-hash
      timestamp
      leader-pkey
      election-proof
      witnesses
      merkle-root-hash)
  "These slots are serialized and then hashed. The hash is stored as
   the block-hash on the current block and the prev-block-hash on a
   newer block on the blockchain. These slots represent the block header only.")

(defun serialize-header (block)
  "Return a serialization of BLOCK header as a list of octet vectors
for the slots in *names-of-block-slots-to-serialize*. It is an error
to call this before all slots are bound. This is to be used to hash a
previous block, i.e., on that has been fully formed and already been
added to the blockchain."
  (loop :for slot-name :in +header-slots-to-serialize+
     :collect (loenc:encode (slot-value block slot-name))))


;;; MAKE-BLOCK: TRANSACTIONS should be a sequence of TRANSACTION instances.
;;; The order of the elements of TRANSACTIONS is fixed in the block once the
;;; block is created and cannot be changed. The order is somewhat flexible
;;; except for the following partial ordering constraints:
;;;
;;;   (1) a collect transaction must come first; and
;;;
;;;   (2) for any two transactions Tx1, Tx2, if the input of Tx2 spends the
;;;   output of Tx1, Tx1 must come before Tx2.
;;;
;;; This function does not check the order. However, validators check the order,
;;; and they can require this order for a block to be considered valid.
;;; Software may also rely upon this order, e.g., as a search heuristic.

(defun make-block (prev-block? block-election-proof block-leader-pkey
                   block-witnesses block-transactions)
  (let ((blk (make-instance 'eblock)))
    (with-slots (merkle-root-hash 
                 input-script-merkle-root-hash
                 witness-merkle-root-hash
                 timestamp election-proof 
                 leader-pkey witnesses transactions)
        blk
      (setf timestamp (- (get-universal-time) +unix-epoch-ut+)
            election-proof block-election-proof
            leader-pkey block-leader-pkey
            witnesses block-witnesses
            transactions block-transactions
            merkle-root-hash (compute-merkle-root-hash blk)
            input-script-merkle-root-hash (compute-input-script-merkle-root-hash blk)
            witness-merkle-root-hash (compute-witness-merkle-root-hash blk))
      (when prev-block? 
        (with-slots (prev-block-hash)
            blk
          (setf prev-block-hash (hash prev-block?))))
      blk)))


(defun make-genesis-block (payout-address witnesses-and-stakes)
  "Create a genesis block paying to PUBLIC-KEY with WITNESSES-AND-STAKES
   as given. 

   WITNESSES-AND-STAKES is passed on to function
   normalize-witnesses-and-stakes, and the return value is stored in the
   block's block-witnesses-and-stakes slot.
   
   The initial transaction on the block is the coinbase type transaction that
   creates the entire monetary supply of the emtq currency. It  funds
   are sent to the hash of PUBLIC-KEY, i.e., as converted with
   cosi/proofs:public-key-to-address."
  (let* ((genesis-transaction (txn:make-genesis-transaction payout-address))
         (transactions (list genesis-transaction))
         (block (make-block nil nil nil nil transactions)))
    (flet ((ensure-pkey (key)
             ;; Currently, our config system gives us public keys as bignums
             ;; (function get-stakes), while the simulator
             ;; (function keys-and-stakes in package emotiq/sim) supplies them as
             ;; instances.
             (if (typep key 'pbc:public-key)
                 key
                 (make-instance 'pbc:public-key :val key))))
      (setf (witnesses-and-stakes block)
            (loop
               :for (public-key . stake-info) :in witnesses-and-stakes
               :collect `(,(ensure-pkey public-key) . ,stake-info)))
      block)))


(defmethod hash ((block eblock))
  "Applies hash-256 to result of serialize-block-octets, q.v."
  ;; Ends up doing the equivalent of (hash-256  #<bv 1>  #<bv 2>  ...  #<bv N>)
  (apply #'hash:hash/256 (serialize block)))


;;;; Merkle Root Hashes on Blocks

;;; We currently store three merkle root hashes on blocks that provide
;;; an overall merkle hash of the following items on each transaction
;;; and on the order of the transactions in the block:
;;;
;;;   (1) transaction ID;
;;;   (2) witness data (if any); and
;;;   (3) input scripts (if any)
;;;
;;; The most important is item (1) transaction ID.  This is a hash of
;;; most of the data of a transaction. It is arranged that it must be
;;; unique for every transaction.
;;;
;;; Items (2) and (3) are not used for coinbase and collect
;;; transactions, since they have no inputs. They are needed to verify
;;; input scripts and witness data, respectively, since those items
;;; are not part of the transaction ID.  The reason they are not part
;;; of the transaction ID is to prevent transaction malleability.
;;; Items (2) and (3) are not required to be unique.
;;;
;;; These merkle root hashes are put on the block when the block is
;;; created.  Non-leader nodes should rehash and recheck them in
;;; cosi/proofs/newtx:check-block-transactions, q.v., in
;;; new-transactions.lisp.



;;; COMPUTE-MERKLE-ROOT-HASH: construct a merkle root for a sequence of
;;; TRANSACTIONS according to bitcoin.org Bitcoin developer reference doc, here:
;;; https://bitcoin.org/en/developer-reference#block-versions
(defun compute-merkle-root-hash (block)
  (compute-merkle (transactions block) :key #'txn:id))


;;; COMPUTE-INPUT-SCRIPT-MERKLE-ROOT-HASH: construction a merkle root
;;; for a sequence of input scripts, one for each transaction of the
;;; block.
(defun compute-input-script-merkle-root-hash (block)
  (flet ((input-scripts-id-hash (tx)
           (hash:hash/256 (mapcar #'txn:input-unlock-script (txn:inputs tx)))))
    (compute-merkle (transactions block) :key #'input-scripts-id-hash)))


;;; COMPUTE-WITNESS-MERKLE-ROOT-HASH: construction a merkle root for a
;;; sequence of witness data, one for each transaction of the block,
;;; being a sequence of pairs of the combination a signature and a
;;; public key.
(defun compute-witness-merkle-root-hash (block)
  (labels ((input-witnesses (tx-in)
             (list (txn::%input-signature tx-in) (txn::%input-public-key tx-in)))
           (input-witnesses-hash (tx)
             (hash:hash/256 (mapcan #'input-witnesses (txn:inputs tx)))))
    (compute-merkle (transactions block) :key #'input-witnesses-hash)))


(defun compute-merkle (nodes &key (key #'identity))
  "Compute merkle root hash on nonempty list of hashables NODES."
  (labels ((hash-pair (a b)
             (hash:hash/256 (funcall key a) (funcall key b)))
           (compute-merkle-pairs (nodes)
             ;; Compute the merkle root on NODES, a list of two or more hash objects."
             (if (null (rest (rest nodes)))
                 (hash-pair (first nodes) (second nodes))      ;; 2 left
                 (loop                                         ;; three or more:
                    :for (a b . rest?) :on nodes :by #'cddr 
                    :when (and (null rest?) (null b))
                    :do (setq b a) ; odd-length row case: duplicate last row item
                    :collect (hash-pair a b) :into row
                    :finally (return (compute-merkle-pairs row))))))
    (cond ((null nodes)                     ; empty blocks permitted
           (hash:hash/256 '()))
          ((null (rest nodes))              ; just 1
           (hash:hash/256 (funcall key (first nodes))))
          (t
           (compute-merkle-pairs nodes)))))



(defun ith-witness-signed-p (block i)
  "Return true or false (nil) according to whether the ith witness has signed."
  (logbitp i (signature-bitmap block)))

(defun set-ith-witness-signed-p (block i signed-p)
  "Set signed-p to either true or false (nil) for witness at position i."
  (with-slots (signature-bitmap) block
    (setf signature-bitmap
          (dpb (if signed-p 1 0)
               (byte 1 i)
               signature-bitmap))))

(defun update-signature! (block sig bits)
  "Update BLOCK, an eblock instance, slots per SIG, an instance of
   pbc:signed-message, and BITS, a bitmap with format as documented
   for the signature-bitmap slot of eblock."
  (with-slots (signature signature-bitmap)
      block
    (setf signature sig
          signature-bitmap bits)))

