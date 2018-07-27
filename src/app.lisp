(in-package :emotiq/app)

(defun app2 ()

  (wait-for-node-startup)
 
  (let ((alice (make-account "alice"))
	(bob (make-account "bob"))
	(mary (make-account "mary"))
	(fee 10))

    ;; make various transactions
    (send-all-genesis-coin-to alice)
    
    (spend alice bob 490 fee)
    (spend bob mary 290 fee)
    (spend alice mary 190 fee)
    
    ;; get all transactions (in, out) for each person

    (let ((alice-tx (get-transactions alice))
	  (bob-tx (get-transactions bob))
	  (mary-tx (get-transactions mary))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; api's
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass account ()
  ((skey :accessor account-skey)  
   (pkey :accessor account-pkey)  
   (triple :accessor account-triple)))

(defun make-account (name)
  "return an account class"
  (let ((key-triple (pbc:make-key-pair name))
	(a (make-instance 'account)))
    (setf (account-triple a) key-triple
          (account-skey a) (pbc:keying-triple-skey key-triple)
          (account-pkey a) (pbc:keying-triple-pkey key-triple))))

(defun wait-for-node-startup ()
  "do whatever it takes to get this node's blockchain to be current"
  ;; currently - nothing - don't care
  )

(defmethod publish-transaction ((txn cosi/proofs/newtx::transaction))
  (gossip:broadcast (list :new-transaction-new :trn txn) :graphId :uber))

(defparameter *max-amount* 1000000000000) ;; all emtq coin

(defmethod send-all-genesis-coin-to ((dest account))
  "all coins are assigned to the first node in the config files - move those coins to an account used by this app"
  (multiple-value-bind (genesis-pkey genesis-skey)
      (emotiq/config:settings/read :address-for-coins)
    (let ((txn (emotiq/txn:make-spend-transaction (account-triple from)
                                                  (emotiq/txn:address (account-triple to))
                                                  *max-amount*)))
      (publish-transaction txn))))

(defmethod spend ((from account) (to account) amount fee)
  "make a transaction and publish it"
  (let ((txn (emotiq/txn:make-spend-transaction from (emotiq/txn:address (account-pkey to)) amount :fee fee)))
    (publish-transaction txn)))

(defmethod get-transactions ((a account))
  "return multiple value - (a) list of transactions that were inputs to this account and (b) list of transactions that this account was output to"
  (let ((input-txns nil)
        (outputs-to nil))
    :finish-me!))

(defmacro with-current-node (&rest body)
  `(cosi-simgen:with-current-node (cosi-simgen:current-node)
     ,@body))
  
(defmethod get-balance ((triple pbc:keying-triple))
  "return the balance for a given account"
  (with-current-node
   (cosi/proofs/newtx:get-balance (emotiq/txn:address triple))))

(defmethod get-balance ((a account))
  (get-balance (account-triple a)))

(defun this-tx-ouputs-to (tx)
  "return list of txns which use the given tx as input"
  (loop for tx-out in (cosi/proofs/newtx::transaction-outputs tx)
        collect tx-out into result
        finally (return result)))

(defun this-tx-inputs-from (tx)
  "return list of txns which this tx uses as inputs"
  (loop
     :for tx-in :in (cosi/proofs/newtx::transaction-inputs tx)
     :collecting tx-in :into result
     :finally (return result)))

;;; stolen from MHD new-transactions.lisp

;;; (defun dump-txs (&key file mempool block blockchain node)
;;;   (flet ((dump-loops ()
;;;            (when block
;;;              (pr "Dump txs in block = ~s:" block)
;;;              (do-transactions (tx block)
;;;                (dump-tx tx)))
;;;            (when mempool
;;;              (pr "Dump txs in mempool:")
;;;              (loop for tx being each hash-value of cosi-simgen:*mempool*
;;;                    do (dump-tx tx)))
;;;            (when blockchain
;;;              (pr "Dump txs on blockchain:")
;;;              (do-blockchain (block)
;;;                (pr " Dump txs in block = ~s:" block)
;;;                (do-transactions (tx block)
;;;                  (dump-tx tx))))))
;;;     (cosi-simgen:with-current-node
;;;         (or node 
;;;             (cosi-simgen:current-node)
;;;             cosi-simgen:*my-node*)
;;;       (if file
;;;           (with-open-file (*standard-output*
;;;                            file
;;;                            :direction :output :if-exists :supersede)
;;;             (dump-loops))
;;;           (dump-loops)))))



;;; (defun get-utxo-txns-per-account (address)
;;;   (let ((utxos-so-far '()))
;;;     (do-blockchain (block)
;;;       (do-transactions (tx block)
;;;         (loop for txo in (transaction-outputs tx)
;;;               as public-key-hash = (tx-out-public-key-hash txo)
;;;               as index from 0
;;;               when (and (account-addresses= public-key-hash address)
;;;                         (utxo-p tx index))
;;;                 collect `(,txo (,(transaction-id tx) ,index) 
;;;                                ,(tx-out-amount txo))
;;;                   into result
;;;               finally (setq utxos-so-far
;;;                             (nconc utxos-so-far result)))))
;;;     utxos-so-far))

