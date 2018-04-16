;;;; blocks.lisp

(in-package :emotiq)




                                                                                ;
(defclass block ()
  ((block-sw-version :initform 0)
   (block-height
    :accessor block-height
    :documentation "Position of block in the chain, an integer.")

   (prev-block-hash
    :documentation "Hash of previous block (nil for genesis block).")
   (prev-block
    :accessor prev-block
    :documentation "Previous block (nil for genesis block).")

   (merkle-root-hash
    :documentation "Merkle root hash of block-transactions.")
   (block-transactions
    :documentation "Sequence of transactions"))
  (:documentation "A block in the Emotiq chain."))

;; Start with a something that resembles the basic Bitcoin-like block
;; header for now. Add advanced features, using the chain/block
;; prototype, later. -mhd, 4/16/18



(defun make-block (height prev-block transactions)
  (let ((block (make-block)))
    (setf (block-height block) height)
    (setf (prev-block block) prev-block)
    (setf (prev-block-hash block) (hash-block prev-block))
    (setf (merkle-root-hash block)
          (compute-merkle-root-hash transactions))
    (setf (block-transactions block) transactions)))



;;; COMPUTE-MERKLE-ROOT-HASH: construct a merkle root for a list of
;;; TRANSACTIONS according to bitcoin.org Bitcoin developer reference
;;; doc, here:
;;; https://bitcoin.org/en/developer-reference#block-versions

(defun compute-merkle-root-hash (transactions)
  (compute-merkle
   (loop for tx in transactions
        as txid = (transaction-id tx)
         collect txid)
   #'hash-256-string))

(defun compute-merkle (nodes hash-fn)
  (cond
    ((null nodes) nil)                  ; degenerate case => nil
    ((null (rest nodes))                ; just 1
     (funcall hash-fn (first nodes)))
    (t (compute-merkle-pairs nodes hash-fn))))


(defun compute-merkle-pairs (nodes hash-fn)
  (flet ((H (x) (funcall hash-fn x))
         (pair (a b) (concatenate 'string a b)))
    (if (null (cddr nodes))
        ;; 2 left
        (H (pair (first nodes) (second nodes)))
        ;; three or more:
        (loop for (a b . rest?) on nodes by #'cddr
              when (and (null rest?) (null b))
                do (setq b a) ; odd-length row case: duplicate last row item
              collect (H (pair a b))
                into row
              finally (return (compute-merkle row hash-fn))))))



#+omnichain-emotiq-future             ; from Mark E, get to this later
(defclass chain/block ()
  ((version :initarg :version :accessor version
            :initform "20180404a")
   (epoch ;; aka "height"
    :initarg :epoch :accessor epoch
    :initform nil 
    :documentation "The integer of the epoch that this block is a part of.")
   (block-hash
    :initarg :block-hash :accessor block-hash
    :initform nil)
   (previous-block
    :initarg :previous-block :accessor previous-block
    :initform nil)
   (genesis-block
    :initarg :genesis-block :accessor genesis-block
    :initform nil
    :documentation "The hash of the genesis block.")
   (transaction-blocks
    :initarg :transaction-blocks :accessor transaction-blocks
    :initform (ads-treap:make-authenticated-treap))
   (validator-keys-joining
    :initarg :validator-keys-joining :accessor validator-keys-joining
    :initform (ads-treap:make-authenticated-treap)
    :documentation "The validators nodes joining in this epoch.")
   (validator-keys-leaving
    :initarg :validator-keys-leaving :accessor validator-keys-leaving
    :initform (ads-treap:make-authenticated-treap)
    :documentation "The validator nodes leaving in this epoch."))
  (:documentation "A block in the Emotiq chain."))

