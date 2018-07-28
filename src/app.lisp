;; currently trying to track down "insuffient funds" on a second transaction
;; using (emotiq/app::r2) to chip away at the problem
;;
;; test usage:
;; (emotiq/app::test-app)

(in-package :emotiq/app)

;; for testing
(defparameter *tx0* nil)
(defparameter *tx1* nil)
(defparameter *tx2* nil)
(defparameter *tx3* nil)
(defparameter *tx4* nil)

(defun get-nth-key (n)
  "return a keying triple from the config file for nth id"
  (let ((public-private (nth n (gossip/config:get-values))))
    (emotiq/config::make-keying-triple
     (first public-private)
     (second public-private))))

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

(defun get-genesis-key ()
  (get-nth-key 0))

(defun make-genesis-account ()
  (let ((r (make-instance 'account))
        (gk (get-genesis-key)))
    (setf (account-triple r) gk
          (account-skey r) (pbc:keying-triple-skey gk)
          (account-pkey r) (pbc:keying-triple-pkey gk)
          (account-name r) "genesis")
    (setf *genesis* r)
    r))

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

  (setf *genesis* (make-genesis-account))

  (setf *alice* (make-account "alice")
	*bob* (make-account "bob")
	*mary* (make-account "mary")
        *james* (make-account "james"))

  (let ((fee 10))

    ;; make various transactions
    (send-all-genesis-coin-to *alice*)

    (sleep 30)

    (spend *alice* *bob* 490 :fee fee)
    (spend *bob* *mary* 290 :fee fee)
    #+nil(spend *alice* *mary* 190 :fee fee)
    #+nil(spend *alice* *james* 90 :fee fee)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; api's
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wait-for-node-startup ()
  "do whatever it takes to get this node's blockchain to be current"
  ;; currently - nothing - don't care
  (emotiq:note "sleeping to let blockchain start up (is this necessary?)")
  (sleep 5)
  )

(defmethod publish-transaction ((txn cosi/proofs/newtx::transaction))
  (gossip:broadcast (list :new-transaction-new :trn txn) :graphId :uber))

(defparameter *max-amount* (* (cosi/proofs/newtx::initial-coin-units)
                              (cosi/proofs/newtx::subunits-per-coin))
"total amount of emtq's - is this correct?  or, does it need to multiplied by sub-units-per-coin?")

(defparameter *min-fee* 10 "new-transactions.lisp requires a minimum fee of 10")

(defmethod send-all-genesis-coin-to ((dest account))
  "all coins are assigned to the first node in the config files - move those coins to an account used by this app"
  (let ((txn (emotiq/txn:make-spend-transaction
              (get-genesis-key)
              (emotiq/txn:address (account-triple dest)) (- *max-amount* *min-fee*))))
    (publish-transaction txn)
    (setf *tx0* txn)
    *tx0*))

(defmacro with-current-node (&rest body)
  `(cosi-simgen:with-current-node (cosi-simgen:current-node)
     ,@body))

(defmethod spend ((from account) (to account) amount &key (fee 10))
  "make a transaction and publish it"
  ;; minimum fee 10 required by Cosi-BLS/new-transactions.lisp/validate-transaction
  (with-current-node
   (let ((txn (emotiq/txn:make-spend-transaction (account-triple from) (emotiq/txn:address (account-pkey to)) amount :fee fee)))
     (publish-transaction txn)
     (emotiq:note "sleeping to let txn propagate (is this necessary?)")
     (sleep 20)
     txn)))
  
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
  (let ((strm emotiq:*notestream*))
    (emotiq:main)
    (app2)
    (let ((bal-genesis (get-balance (get-genesis-key))))
      (emotiq:note"genesis balance(~a)~%" bal-genesis))
    (let ((bal-alice (get-balance *alice*))
          (bal-bob   (get-balance *bob*))
          (bal-mary  (get-balance *mary*))
          (bal-james (get-balance *james*)))
      (emotiq:note "balances alice(~a) bob(~a) mary(~a) james(~a)~%" bal-alice bal-bob bal-mary bal-james)
      #+nil(let ((txo-alice (get-all-transactions-to-given-target-account *alice*))
                 (txo-bob (get-all-transactions-to-given-target-account *bob*))
                 (txo-mary (get-all-transactions-to-given-target-account *mary*))
                 (txo-james (get-all-transactions-to-given-target-account *james*)))
             (emotiq:note "transactions-to~%alice ~A~%bob ~A~%mary ~A~%james ~A~%"
                     txo-alice
                     txo-bob
                     txo-mary
                     txo-james))
      (setf emotiq:*notestream* *standard-output*) ;;; ?? I tried to dynamically bind emotiq:*notestream* with LET, but that didn't work (???)
      (emotiq:note "sleeping again")
      (setf emotiq:*notestream* strm)
      (sleep 20)
      (with-current-node
       (setf emotiq:*notestream* *standard-output*)
       (cosi/proofs/newtx:dump-txs :blockchain t)
       (emotiq:note "")
       (emotiq:note "balances alice(~a) bob(~a) mary(~a) james(~a)~%" bal-alice bal-bob bal-mary bal-james)
       ;(ac:kill-executives)
       (setf emotiq:*notestream* strm)
       (values)))))

(defun dtx ()
  (with-current-node
   (let ((strm emotiq:*notestream*))
     (setf emotiq:*notestream* *standard-output*)
     (cosi/proofs/newtx:dump-txs :blockchain t)
     (setf emotiq:*notestream* strm)
     (values))))


;;; ;; verify that transactions can refer to themselves in same block - apparently not
;;
;; initial: initial-coin-units * subunits-per-coin
;;
;; gossip:send node :kill-node
;;

;; stuff that worked a few days ago
;; 
;; (R2) works and produces 4 blocks.  The first block contains one txn - COINBASE.  The other 3 blocks contain 2 txns - a COLLECT (leader's reward) and a SPEND txn.
;;
;; I'm going to leave the code below in, until I can get the code above to work...

(defun r2 ()
  (setf gossip::*debug-level* 0)
  (emotiq:main)
  (let ((from (emotiq/app::make-genesis-account))
        (paul (pbc:make-key-pair :paul)))
    (let ((txn (emotiq/txn:make-spend-transaction
                (emotiq/app::account-triple from)
                (emotiq/txn:address paul) 1000)))
      (gossip:broadcast (list :new-transaction-new :trn txn) :graphId :uber)
      
      ;; adding this transaction gives an "insufficient funds" message
      ;; obs - 3 blocks were created when I used the debugger - does this need more time
      ;; to settle?
      (sleep 30)
      (let ((mark (pbc:make-key-pair :mark)))
             (let ((txn2 (emotiq/txn:make-spend-transaction paul (emotiq/txn:address mark) 500)))
               (gossip:broadcast (list :new-transaction-new :trn txn2) :graphID :uber))
             (sleep 30)
             (let ((shannon (pbc:make-key-pair :shannon)))
               (let ((txn3 (emotiq/txn:make-spend-transaction mark (emotiq/txn:address shannon) 50)))
                 (gossip:broadcast (list :new-transaction-new :trn txn3) :graphID :uber)
                 (sleep 30))))))

  ;; inspect this node to see resulting blockchain
  cosi-simgen:*my-node*)

(defun r2-shutdown ()
  (emotiq-rest:stop-server)
  (websocket/wallet::stop-server))

;; ideas to try:
;; - see if we get two successive hold-elections w/o intervening :prepares (means that leader found
;;   no work)
;; - maybe also check our own mempool for emptiness?
