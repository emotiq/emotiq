;;;; block.lisp

(in-package :cosi/proofs)




                                                                                ;
(defclass block ()
  ((protocol-version
    :accessor protocol-version
    :initform 1
    :documentation 
      "Version of the protocol/software, an integer")

   (epoch                               ; aka "height"
    :initarg :epoch :accessor epoch
    :initform nil 
    :documentation
      "The integer of the epoch that this block is a part of."
      ;; height: "Position of block in the chain, an integer."
      )

   (prev-block
    :accessor prev-block
    :documentation "Previous block (nil for genesis block).")
   (prev-block-hash
    :accessor prev-block-hash
    :documentation "Hash of previous block (nil for genesis block).")

   (merkle-root-hash
    :accessor merkle-root-hash
    :documentation "Merkle root hash of block-transactions.")

   (block-timestamp
    :accessor block-timestamp
    :documentation 
      "Approximate creation time in seconds since Unix epoch.")

   ;; Block-transactions is generally what's considered the main
   ;; contents of a block whereas the rest of the above comprises
   ;; what's known as the 'block header' information.
   (transactions
    :accessor transactions
    :documentation "Sequence of transactions")

   ;; Caller can set and maintain these slots. These could be set to
   ;; contain a treap or possibly for now just a simple list.
   (validator-keys-joining
    :accessor validator-keys-joining
    :documentation "The validators nodes joining in this epoch.")
   (validator-keys-leaving
    :accessor validator-keys-leaving
    :documentation "The validator nodes leaving in this epoch."))


  (:documentation "A block in the Emotiq chain."))



(defvar *unix-epoch-ut* (encode-universal-time 0 0 0 1 1 1970 0)
  "The Unix epoch as a Common Lisp universal time.")



;;; CREATE-BLOCK: TRANSACTIONS should be a list of TRANSACTION instances.  The
;;; order of the elements of TRANSACTIONS is fixed in the block once the block
;;; is created and cannot be changed. The order is somewhat flexible except for
;;; the following partial ordering constraints:
;;;
;;;   (1) a coinbase transaction must come first; and
;;;
;;;   (2) for any two transactions Tx1, Tx2, if the input of Tx2 spends the
;;;   output of Tx1, Tx1 must come before Tx2.
;;;
;;; This function does not check the order. However, validators check the order,
;;; and they can require this order for a block to be considered valid.
;;; Software may also rely upon this order, e.g., as a search heuristic.

(defun create-block (epoch prev-block transactions)
  (let ((block (make-instance 'block :epoch epoch)))
    (setf (prev-block block) prev-block)
    (setf (prev-block-hash block) 
          (if (null prev-block)
              nil
              (hash-block prev-block)))
    (setf (merkle-root-hash block)
          (compute-merkle-root-hash transactions))
    (setf (transactions block) transactions)
    (setf (block-timestamp block) 
          (- (get-universal-time) *unix-epoch-ut*))
    block))



(defun hash/256d (&rest hashables)
  "Double sha-256-hash HASHABLES, returning a hash:hash/256 hash value
   representing a 32 raw-byte vector. This is the hash function
   Bitcoin uses for hashing nodes of a merkle tree and for computing
   the hash of a block."
  (hash:hash/256 (apply #'hash:hash/256 hashables)))



(defun hash-block (block)
  (apply #'hash/256d (serialize-block-octets block)))



(defun list-integer-octets (integer &optional fixed-n?)
  "Return the octets of INTEGER as a list beginning with the least
  significant octet. FIXED-N?, if non-nil, should be a positive
  integer, in which case the result is a list of that many octets from
  lowest to highest."
  (loop with int-so-far of-type integer = integer
        with result = '()
        with counter = 0
        as octet = (logand #xFF int-so-far)
        do (push octet result)
           (setq int-so-far (ash int-so-far -8))
           (if fixed-n?
               (when (>= (incf counter) fixed-n?)
                 (loop-finish))
               (when (= int-so-far 0)
                 (loop-finish)))
        finally (return (nreverse result))))

;; General, so move elsewhere, or merge/replace with something already
;; in DBM's tool box! -mhd, 4/18/18



(defun serialize-block-octets (block)
  "Return a serialization of BLOCK as a list of octet vectors,
   comprised of these elements

 element:       # of octets:
   version           4
   epoch             4
   prev block hash  32
   merkle root hash 32
   timestamp         8"
  (let ((out '()))
    (flet ((emit (x)
             (push (if (consp x)
                       (make-array 
                        (length x)
                        :element-type 'ub8 :initial-contents x)
                       x)
                   out)))
      (emit (list-integer-octets (protocol-version block) 4))
      (emit (list-integer-octets (epoch block) 4))
      (emit (prev-block-hash block))
      (emit (merkle-root-hash block))
      (emit (list-integer-octets (block-timestamp block) 8)))
    (nreverse out)))

;; The use of 8 octets instead of 4 for timestamp is nonstandard but
;; is intended to avoid the "Year 2038 problem".



;;; COMPUTE-MERKLE-ROOT-HASH: construct a merkle root for a list of
;;; TRANSACTIONS according to bitcoin.org Bitcoin developer reference
;;; doc, here:
;;; https://bitcoin.org/en/developer-reference#block-versions

(defun compute-merkle-root-hash (transactions)
  (compute-merkle
   (loop for tx in transactions
         as tx-out-id = (get-txid-out-id tx)
         collect tx-out-id)))



(defun get-txid-out-id (transaction)
  "Get the identifier of TRANSACTION an octet vector of length 32, which can be
   used to hash the transactions leaves of the merkle tree to produce the
   block's merkle tree root hash. It represents H(PubKey, PedComm), or possibly
   a superset thereof."
  (vec-repr:bev-vec (hash:hash-val (hash-transaction transaction))))

;; In Bitcoin this is known as the TXID of the transaction.



(defun hash-transaction (transaction)
  "Produce a hash for TRANSACTION, including the pair (PubKey, PedComm).
   The resulting hash's octet vector is usable as a transaction ID."
  (hash/256d transaction))

;; Consider storing the ID in the transaction later.  Consider later
;; switching to Bitcoin's "Hash160", i.e., RIPEMD160(SHA256(Tx))).



(defun compute-merkle (nodes)
  "Compute merkle root hash on nonempty list of hashables NODES."
  (assert (not (null nodes)) () "NODES must not be null.")
  (cond
    ((null (rest nodes))                ; just 1
     (hash/256d (first nodes)))
    (t (compute-merkle-pairs nodes))))



(defun compute-merkle-pairs (nodes)
  "Compute the merkle root on NODES, a list of two or more hash
   objects."
  (if (null (rest (rest nodes)))
      ;; 2 left
      (hash/256d (first nodes) (second nodes))
      ;; three or more:
      (loop for (a b . rest?) on nodes by #'cddr
            when (and (null rest?) (null b))
              do (setq b a) ; odd-length row case: duplicate last row item
            collect (hash/256d a b)
              into row
            finally (return (compute-merkle-pairs row)))))
