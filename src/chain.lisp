(in-package :emotiq/chain)

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

(defun make-genesis-block ()
  (make-instance 'chain/block
                 :epoch 0))
