(in-package :emotiq/app)

;; for testin
(defparameter *tx1* nil)
(defparameter *tx2* nil)
(defparameter *tx3* nil)
(defparameter *tx4* nil)

(defparameter *alice* nil)
(defparameter *bob* nil)
(defparameter *mary* nil)
(defparameter *james* nil)

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
          (account-pkey a) (pbc:keying-triple-pkey key-triple))
    a))

(defun app2 ()
  (wait-for-node-startup)

  (setf *alice* (make-account "alice")
	*bob* (make-account "bob")
	*mary* (make-account "mary")
        *james* (make-account "james"))

  (let ((fee 10))

    ;; make various transactions
    (send-all-genesis-coin-to *alice*)
    
    (spend *alice* *bob* 490 fee)
    (spend *bob* *mary* 290 fee)
    (spend *alice* *mary* 190 fee)
    (spend *alice* *james* 90 fee)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; api's
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wait-for-node-startup ()
  "do whatever it takes to get this node's blockchain to be current"
  ;; currently - nothing - don't care
  )

(defmethod publish-transaction ((txn cosi/proofs/newtx::transaction))
  (gossip:broadcast (list :new-transaction-new :trn txn) :graphId :uber))

(defparameter *max-amount* (cosi/proofs/newtx::initial-coin-units))

(defmethod send-all-genesis-coin-to ((dest account))
  "all coins are assigned to the first node in the config files - move those coins to an account used by this app"
  (multiple-value-bind (s p sig)
    (let ((txn (emotiq/txn:make-spend-transaction (emotiq/config:settings/read :address-for-coins)
                                                  (emotiq/txn:address (account-triple dest))
                                                  *max-amount*)))
      (publish-transaction txn)))

(defmethod spend ((from account) (to account) amount fee)
  "make a transaction and publish it"
  (let ((txn (emotiq/txn:make-spend-transaction from (emotiq/txn:address (account-pkey to)) amount :fee fee)))
    (publish-transaction txn)))

(defmacro with-current-node (&rest body)
  `(cosi-simgen:with-current-node (cosi-simgen:current-node)
     ,@body))
  
(defmethod get-balance ((triple pbc:keying-triple))
  "return the balance for a given account"
  (with-current-node
   (cosi/proofs/newtx:get-balance (emotiq/txn:address triple))))

(defmethod get-balance ((a account))
  (get-balance (account-triple a)))

(defmethod get-all-transactions-to-given-target-account ((a account))
  "return a list of transactions that SPEND (and COLLET) to account 'a' ; N.B. this only returns transactions that have already been committed to the blockchain (e.g. transactions in MEMPOOL are not considered ; no UTXOs (unspent transactions)"
  (let ((account-address (emotiq/txn:address (account-pkey a)))
        (result nil))
    (cosi-simgen:with-current-node (cosi-simgen:current-node)
      (cosi/proofs/newtx::do-blockchain (block) ;; TODO: optimize this
        (cosi/proofs/newtx::do-transactions (tx block)
          (when (or (eq :COLLECT (cosi/proofs/newtx::transaction-type tx))
                    (eq :SPEND (cosi/proofs/newtx::transaction-type tx)))
            (dolist (out (cosi/proofs/newtx::transaction-outputs tx)) 
              (when (eq account-address (cosi/proofs/newtx::tx-out-public-key-hash out))
                (push tx result)))))))
    result))

(defun test-app ()
  (app2)
  (let ((bal-alice (get-balance *alice*))
        (bal-bob   (get-balance *bob*))
        (bal-mary  (get-balance *mary*))
        (bal-james (get-balance *james*)))
    (format t "balances alice(~a) bob(~a) mary(~a) james(~a)~%" bal-alice bal-bob bal-mary bal-james))
  (let ((txo-alice (get-all-transactions-to-given-target-account *alice*))
        (txo-bob (get-all-transactions-to-given-target-account *bob*))
        (txo-mary (get-all-transactions-to-given-target-account *mary*))
        (txo-james (get-all-transactions-to-given-target-account *james*)))
    (format t "transactions-to~%alice ~A~%bob ~A~%mary ~A~%james ~A~%"
            txo-alice
            txo-bob
            txo-mary
            txo-james)))

        