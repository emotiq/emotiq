(in-package :emotiq/chains)

(defclass block ()
  ((epoch
    :initarg :epoch :initform nil :accessor epoch
    :documentation "The integer of the leader epoch that this block is a part of.")))

(defclass block/identity (block)
  ((block-hash
    :initarg :block-hash :accessor block-hash
    :initform nil)
   (previous-block
    :initarg :previous-block :accessor previous-block
    :initform nil)
   (genesis-block
    :initarg :genesis-block :accessor genesis-block
    :initform nil)
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
   (:documentation "The contents of an identity block"))

(defclass block/transaction (block)
   ((eon
    :initarg :eon :initform nil :accessor eon
    :documentation "The montonically increasing integer of when the transactions was added to the identity block.")
   (previous-transaction-block 
    :initarg :previous-transaction-block :accessor previous-transaction-block)
   (transactions 
    :initarg :transactions :accessor transactions
    :initform (ads-treap:make-authenticated-treap)
    :documentation "The transactions with their associated witnesses"))
  (:documentation "The contents of a transaction block, aka. microblock"))

   



    
   

  
