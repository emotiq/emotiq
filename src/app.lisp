(in-package :emotiq/app)

;; for testin
(defparameter *tx0* nil)
(defparameter *tx1* nil)
(defparameter *tx2* nil)
(defparameter *tx3* nil)
(defparameter *tx4* nil)

(defun get-nth-key (n)
  (let ((public-private (nth n (gossip/config:get-values))))
    (emotiq/config::make-keying-triple
     (first public-private)
     (second public-private))))

#|
(defun make-history-1 ()
    (let ((alice (get-nth-key 0))
          (bob (get-nth-key 1)))
      (let ((txn-1 (emotiq/txn:make-spend-transaction alice (emotiq/txn:address bob) (expt 10 5))))
        (gossip:broadcast (list :new-transaction-new :trn txn-1) :graphid :uber))))

(defun make-history-2 ()
    (let ((bob (get-nth-key 1))
          (carol (get-nth-key 2)))
      (let ((txn-1 (emotiq/txn:make-spend-transaction bob (emotiq/txn:address carol (expt 10 4)))))
        (gossip:broadcast (list :new-transaction-new :trn txn-1) :graphid :uber))))
|#

(defparameter *genesis* nil)
(defparameter *alice* nil)
(defparameter *bob* nil)
(defparameter *mary* nil)
(defparameter *james* nil)

(defclass account ()
  ((skey :accessor account-skey)  
   (pkey :accessor account-pkey)  
   (triple :accessor account-triple)
   (name :accessor account-name)))

(defun make-account (name)
  "return an account class"
  (let ((key-triple (pbc:make-key-pair name))
	(a (make-instance 'account)))
    (setf (account-triple a) key-triple
          (account-skey a) (pbc:keying-triple-skey key-triple)
          (account-pkey a) (pbc:keying-triple-pkey key-triple)
          (account-name a) name)
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
    
    #+nil(spend *alice* *bob* 490 fee)
    #+nil(spend *bob* *mary* 290 fee)
    #+nil(spend *alice* *mary* 190 fee)
    #+nil(spend *alice* *james* 90 fee)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; api's
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wait-for-node-startup ()
  "do whatever it takes to get this node's blockchain to be current"
  ;; currently - nothing - don't care
  (sleep 10)
  )

(defmethod publish-transaction ((txn cosi/proofs/newtx::transaction))
  (gossip:broadcast (list :new-transaction-new :trn txn) :graphId :uber))

(defparameter *max-amount* (cosi/proofs/newtx::initial-coin-units))

(defun get-genesis-key ()
  (get-nth-key 0))

(defmethod send-all-genesis-coin-to ((dest account))
  "all coins are assigned to the first node in the config files - move those coins to an account used by this app"
  (let ((txn (emotiq/txn:make-spend-transaction (get-genesis-key)
                                                (emotiq/txn:address (account-triple dest))
                                                *max-amount*)))
    (setf *tx0* txn)
    (publish-transaction txn)))

(defmacro with-current-node (&rest body)
  `(cosi-simgen:with-current-node (cosi-simgen:current-node)
     ,@body))

;(defmacro with-current-node (&rest body)
;  `(cosi-simgen:with-current-node cosi-simgen::*my-node*
;     ,@body))

(defmethod spend ((from account) (to account) amount fee)
  "make a transaction and publish it"
  (with-current-node
   (let ((txn (emotiq/txn:make-spend-transaction (account-triple from) (emotiq/txn:address (account-pkey to)) amount :fee fee)))
     (publish-transaction txn))))
  
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
  (emotiq:main)
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

(defun test-app1 ()
  (emotiq:main)
  (app2)
  (let ((bal-genesis (get-balance (get-genesis-key))))
    (format t "genesis balance(~a)~%" bal-genesis))
  #+nil(let ((bal-alice (get-balance *alice*)))
    #+nil(format t "balances alice(~a)~%" bal-alice )))


;; verify that transactions can refer to themselves in same block
;;
;; initial: initial-coin-units * subunits-per-coin
;;
;; gossip:send node :kill-node
;;
