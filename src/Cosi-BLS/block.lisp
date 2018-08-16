;;;; block.lisp

(in-package :cosi/proofs)


(defclass eblock ()
  ((protocol-version
    :initform 1
    :documentation 
      "Version of the protocol/software, an integer.")

   (prev-block-hash
    :reader block-prev-block-hash
    :initform nil
    :documentation "Hash of previous block (nil for genesis block).")

   (timestamp
    :reader block-timestamp
    :initform nil
    :documentation 
      "Approximate creation time in seconds since Unix epoch. The time zone is UTC.")

   (leader-pkey
    :reader block-leader-pkey
    :initform nil
    :documentation
    "Public key for the leader for this epoch.")

   (election-proof
    :reader block-election-proof
    :initform nil)

   (signature
    :initform nil
    :reader block-signature
    :documentation
    "A signature over the whole block authorizing all transactions.")
   (signature-bitmap
    :reader block-signature-bitmap
    :initform 0
    :documentation 
    "Supply a bitmap represented by an
     integer (potentially a bignum), where each position of the bitmap
     corresponds to a vector index, such that its state tells you
     whether that particular potential witness signed.")

   (witnesses
    :initform nil
    :reader block-witnesses
    :documentation
    "Sequence of public keys of validators 1:1 w/signature-bitmap slot.")

   (witnesses-and-stakes
    :accessor block-witnesses-and-stakes
    :initform nil
    :documentation
    "An a-list of the form

  ((<witness public-key> <stake amount in EMTQ coin/integer>)
  ...)

This is to be set only in the genesis block; otherwise, it should
remain unbound (and should not be referred to). The configuration data
comes from a config file.")             ; ---*** TODO: to be
                                        ; specified. -mhd, 6/28/18

   (merkle-root-hash
    :reader block-merkle-root-hash
    :documentation "Merkle root hash of transactions.")
   (input-script-merkle-root-hash
    :reader block-input-script-merkle-root-hash
    :documentation "Merkle root hash of input scripts.")
   (witness-merkle-root-hash
    :reader block-witness-merkle-root-hash
    :documentation "Merkle root hash of witness data.")

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


(defun serialize-block-octets (block)
  "Return a serialization of BLOCK as a list of octet vectors for the slots in
   \*names-of-block-slots-to-serialize*. It is an error to call this before all
   those slots are bound. This is to be used to hash a previous block, i.e., on
   that has been fully formed and already been added to the blockchain."
  (loop for slot-name in *names-of-block-slots-to-serialize*
        collect (loenc:encode (slot-value block slot-name))))


(defparameter *names-of-block-header-slots-to-serialize*
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

(defmethod serialize-block-header-octets ((blk eblock))
  "Return a serialization of BLOCK header as a list of octet vectors
for the slots in *names-of-block-slots-to-serialize*. It is an error
to call this before all slots are bound. This is to be used to hash a
previous block, i.e., on that has been fully formed and already been
added to the blockchain."
  (loop for slot-name in *names-of-block-header-slots-to-serialize*
        collect (loenc:encode (slot-value blk slot-name))))


(defvar *unix-epoch-ut* (encode-universal-time 0 0 0 1 1 1970 0)
  "The Unix epoch as a Common Lisp universal time. (Note: implied time
  zone is UTC.)")



;;; CREATE-BLOCK: TRANSACTIONS should be a sequence of TRANSACTION instances.
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

(defmethod create-block (prev-block?
                         block-election-proof
                         (block-leader-pkey public-key)
                         (block-witnesses vector)
                         (block-transactions list))
  (let ((blk (make-instance 'eblock)))
    (with-slots (merkle-root-hash 
                 input-script-merkle-root-hash
                 witness-merkle-root-hash
                 timestamp election-proof 
                 leader-pkey witnesses transactions)
        blk
      (setf timestamp (- (get-universal-time) *unix-epoch-ut*))
      (setf election-proof block-election-proof)
      (setf leader-pkey block-leader-pkey)
      (setf witnesses block-witnesses)
      (setf transactions block-transactions)
      (setf merkle-root-hash (compute-merkle-root-hash blk))
      (setf input-script-merkle-root-hash (compute-input-script-merkle-root-hash blk))
      (setf witness-merkle-root-hash (compute-witness-merkle-root-hash blk))
      blk)))

(defmethod create-block :around ((prev-block? eblock)
                                 block-election-proof
                                 (block-leader-pkey public-key)
                                 (block-witnesses vector)
                                 (block-transactions list))
  (let ((blk (call-next-method)))
    (setf (slot-value blk 'prev-block-hash) (hash-block prev-block?))
    blk))


(defparameter *newtx-p* t) ; using new-transactions.lisp (package
                           ; cosi/proofs/newtx) - MHD development!
                           ; -mhd, 6/13/18



(defmethod create-genesis-block ((public-key pbc:public-key) (witnesses-and-stakes list))
  "Create a genesis block paying to PUBLIC-KEY with WITNESSES-AND-STAKES
   as given. 

   WITNESSES-AND-STAKES is passed on to function
   normalize-witnesses-and-stakes, and the return value is stored in the
   block's block-witnesses-and-stakes slot.
   
   The initial transaction on the block is the coinbase type transaction that
   creates the entire monetary supply of the emtq currency. It  funds
   are sent to the hash of PUBLIC-KEY, i.e., as converted with
   cosi/proofs:public-key-to-address."
  
  (when (not *newtx-p*)
    (error "Sorry, only know how to do this for new transactions (*newtx-p*)."))
  (let* ((genesis-transaction
          (let ((cosi/proofs/newtx::*%debug-public-key* public-key))
            (cosi/proofs/newtx:make-genesis-transaction
             (cosi/proofs:public-key-to-address public-key))))
         (transactions (list genesis-transaction))
         (g-d-keying   (make-key-pair :g-d))
         (block (create-block nil ;; no prior block
                              nil ;; no election proof
                              (keying-triple-pkey g-d-keying) ;; guess who...
                              #() ;; no witnesses
                              transactions))
         (normalized-block-witnesses-and-stakes
           (normalize-witnesses-and-stakes witnesses-and-stakes)))
    (setf (block-witnesses-and-stakes block)
          normalized-block-witnesses-and-stakes)
    block))



(defmethod normalize-witnesses-and-stakes ((witnesses-and-stakes list))
  "Given an a-list of the form

     ( (general-public-key . (emtq-amount-in-subunits))* )

   where a general-public-key is any form of public key that either is or can be
   converted to a pbc:public-key instance, and emtq-amount-in-subunits is an
   integer number of coin subunits, return an a-list of the same form except
   that each public key is always a pbc:public-key instance."
  (loop for (general-public-key . stake-info) in witnesses-and-stakes
        as public-key
          = (if (typep general-public-key 'pbc:public-key)
                general-public-key
                (make-instance 'pbc:public-key :val general-public-key))
        collect `(,public-key . ,stake-info)))

;; Currently, our config system gives us public keys as bignums
;; (function get-stakes), while the simulator
;; (function keys-and-stakes in package emotiq/sim) supplies them as
;; instances.
    
             


(defun hash-256 (&rest hashables)
  "This is the hashing used in our block in a merkle tree, linking
   transaction outputs and inputs, and hashing the block header. This
   uses SHA-3 for hashing; therefore, we do not use double hashing as
   in Bitcoin."
  (apply #'hash:hash/256 hashables))


(defmethod hash-block ((blk eblock))
  "Applies hash-256 to result of serialize-block-octets, q.v."
  ;; Ends up doing the equivalent of
  ;;
  ;;   (hash-256  #<bv 1>  #<bv 2>  ...  #<bv N>)
  (apply #'hash-256 (serialize-block-octets blk)))


(defmethod print-object ((blk eblock) out-stream)
  (print-unreadable-object (blk out-stream :type t)
    (princ (short-str (hex-str (hash-block blk))) out-stream)))

(defmethod listed-transactions-of-block ((blk eblock))
  "Return the transactions of BLOCK as a list. The caller must not
   make any assumption as to whether the result shares list structure
   with the block (or any other data structure) or whether it is
   freshly consed."
  (with-slots (transactions) 
      blk
    ;; transactions is a sequence, i.e., list or vector
    (if (listp transactions)
        ;; optimized for list case: no copying needed
        transactions
        ;; else, copy elements of transactions, known to be a vector:
        (loop for i from 0 below (length transactions)
              collect (aref transactions i)))))



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

(defmethod compute-merkle-root-hash ((blk eblock))
  (compute-merkle (list-transaction-ids-of-block blk)))

(defmethod list-transaction-ids-of-block ((blk eblock))
  (loop for tx in (listed-transactions-of-block blk)
        collect (get-transaction-id tx)))



;;; COMPUTE-INPUT-SCRIPT-MERKLE-ROOT-HASH: construction a merkle root
;;; for a sequence of input scripts, one for each transaction of the
;;; block.

(defmethod compute-input-script-merkle-root-hash ((blk eblock))
  (compute-merkle (list-transaction-input-script-ids-of-block blk)))

(defmethod list-transaction-input-script-ids-of-block ((blk eblock))
  (loop for tx in (listed-transactions-of-block blk)
        collect (cosi/proofs/newtx:hash-transaction-input-scripts-id tx)))



;;; COMPUTE-WITNESS-MERKLE-ROOT-HASH: construction a merkle root for a
;;; sequence of witness data, one for each transaction of the block,
;;; being a sequence of pairs of the combination a signature and a
;;; public key.

(defmethod compute-witness-merkle-root-hash ((blk eblock))
  (compute-merkle (list-transaction-witness-ids-of-block blk)))

(defmethod list-transaction-witness-ids-of-block ((blk eblock))
  (loop for tx in (listed-transactions-of-block blk)
        collect (cosi/proofs/newtx:hash-transaction-witness-id tx)))




(defmethod compute-merkle ((nodes list))
  "Compute merkle root hash on nonempty list of hashables NODES."
  (cond
    ((null nodes)                       ; empty blocks permitted
     (hash-256 '()))
    ((null (rest nodes))                ; just 1
     (hash-256 (first nodes)))
    (t (compute-merkle-pairs nodes))))



(defmethod compute-merkle-pairs ((nodes list))
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



(defmethod update-block-signature ((blk eblock) (sig pbc:signature) (bits integer))
  "Update BLOCK, an eblock instance, slots per SIG, an instance of
   pbc:signed-message, and BITS, a bitmap with format as documented
   for the signature-bitmap slot of eblock."
  (with-slots (signature signature-bitmap)
      blk
    (setf signature sig)
    (setf signature-bitmap bits)))
