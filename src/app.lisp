;; test usage:
;; (emotiq/app::test-app)

(in-package :emotiq/app)

;; for testing
(defparameter *tx0* nil)
(defparameter *tx1* nil)
(defparameter *tx2* nil)
(defparameter *tx3* nil)
(defparameter *tx4* nil)

(defparameter *blocks* nil)
(defparameter *node* nil)

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
  (emotiq/config:get-nth-key 0))

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

(defun make-account-given-keypairs (name pkey skey)
  "return an account class"
  (let ((a (make-instance 'account)))
    (setf (account-triple a) (pbc:make-keying-triple pkey skey)
          (account-skey a) skey
          (account-pkey a) pkey
          (account-name a) name)
    a))

(defmacro with-current-node (&rest body)
  `(cosi-simgen:with-current-node cosi-simgen::*my-node*
     ,@body))

;; from test/geness.lisp
(defun verify-genesis-block (&key (root (emotiq/fs:etc/)))
  (let* ((genesis-block
          (emotiq/config:get-genesis-block
           :root root))
         (keypair
          (emotiq/config:get-nth-key 0 :root root)))
    (values
     (cosi-simgen:with-block-list ((list genesis-block))
       (cosi/proofs/newtx:get-balance (emotiq/txn:address keypair)))
     (emotiq/txn:address keypair)
     root)))

(defun test-app ()
  "entry point for tests"
  (let ((strm emotiq:*notestream*))
    (emotiq:main)
    (setf gossip::*debug-level* nil)
    (app)
    (setf emotiq/app:*node* cosi-simgen::*my-node*  ;; for debugging
          emotiq/app:*blocks* (cosi-simgen::block-list))
    (writebc)
    (verify-genesis-block)
    (dump-results strm)
    (generate-pseudo-random-transactions *alice*)))

(defun make-accounts ()
  (setf *genesis* (make-genesis-account))
  (setf *alice* (make-account "alice")
	*bob* (make-account "bob")
	*mary* (make-account "mary")
        *james* (make-account "james")))
  
(defun app ()
  "helper for tests"

  (wait-for-node-startup)

  (make-accounts)

  (let ((bal (get-balance *genesis*))
        (a (emotiq/txn:address (account-triple *genesis*))))
    (emotiq:note "balance for genesis is ~A" bal)
    (emotiq:note "address of genesis ~A" a))
  (let ((fee 10))
    
    ;; make various transactions
    (send-all-genesis-coin-to *alice*)

    (sleep 30)

    (spend *alice* *bob* 490 :fee fee)
    (spend *alice* *mary* 190 :fee fee)
    (spend *alice* *james* 90 :fee fee)

    (sleep 30)

    (spend *bob* *mary* 290 :fee fee)
    ;(spend *bob* *mary* 1000 :fee fee) ;; should raise insufficient funds error

    (sleep 120)))

;; alice should have 999...999,190
;; bob 190
;; mary 480
;; james 90

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; api's
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wait-for-node-startup ()
  "do whatever it takes to get this node's blockchain to be current"
  ;; currently - nothing - don't care
  ;; no-op
  )

(defmethod publish-transaction ((txn cosi/proofs/newtx::transaction))
  (gossip:broadcast (list :new-transaction-new :trn txn) :graphId :uber))

;(defparameter *max-amount* (* (cosi/proofs/newtx::initial-coin-units)
;                              (cosi/proofs/newtx::subunits-per-coin))
(defparameter *max-amount* (cosi/proofs/newtx:initial-total-coin-amount)
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

(defmethod spend ((from account) (to account) amount &key (fee 10))
  "make a transaction and publish it"
  ;; minimum fee 10 required by Cosi-BLS/new-transactions.lisp/validate-transaction
  (with-current-node
   (let ((txn (emotiq/txn:make-spend-transaction
               (account-triple from)
               (emotiq/txn:address (account-pkey to))
               amount
               :fee fee)))
     (publish-transaction txn)
     (emotiq:note "sleeping to let txn propagate (is this necessary?)")
     (sleep 30)
     txn)))
  
(defmethod get-balance ((triple pbc:keying-triple))
  "return the balance for a given account"
  (with-current-node
   (cosi/proofs/newtx:get-balance (emotiq/txn:address triple))))

(defmethod get-balance ((a account))
  (get-balance (account-triple a)))


(defun dump-results (strm)
  (declare (ignorable strm))

  (let ((bal-genesis (get-balance (get-genesis-key))))
    (emotiq:note"genesis balance(~a)~%" bal-genesis))

  #+nil(let ((bal-alice (get-balance *alice*))
        (bal-bob   (get-balance *bob*))
        (bal-mary  (get-balance *mary*))
        (bal-james (get-balance *james*)))
    (with-current-node
     (setf emotiq:*notestream* *standard-output*)
     (cosi/proofs/newtx:dump-txs :blockchain t)
     (emotiq:note "")
     (emotiq:note "balances alice(~a) bob(~a) mary(~a) james(~a)~%" bal-alice bal-bob bal-mary bal-james)
     (dumpamounts)
     
     ;; this gathers all txns to alice/bob/mary/james (incl. change txns)
     (let ((tx-alice (get-all-transactions-to-given-target-account *alice*))
           (tx-bob (get-all-transactions-to-given-target-account *bob*))
           (tx-mary (get-all-transactions-to-given-target-account *mary*))
           (tx-james (get-all-transactions-to-given-target-account *james*)))
       (emotiq:note "transactions-to~%alice ~A~%bob ~A~%mary ~A~%james ~A~%"
                    tx-alice
                    tx-bob
                    tx-mary
                    tx-james)
       
       (setf emotiq:*notestream* strm)
       (values)))))

(defun dumpamounts ()
  (let ((aliceamount (- (- *max-amount* 10) 30 490 190 90))
        (bobamount  (- 490 300))
        (maryamount (+ 290 190))
        (jamesamount 90))
    (emotiq:note "calculated balances alice(~a) bob(~a) mary(~a) james(~a)~%"
                 aliceamount bobamount maryamount jamesamount)))
    
(defun dtx ()
  (with-current-node
   (let ((strm emotiq:*notestream*))
     (setf emotiq:*notestream* *standard-output*)
     (cosi/proofs/newtx:dump-txs :blockchain t)
     (setf emotiq:*notestream* strm)
     (values))))


(defun shutdown ()
  (emotiq-rest:stop-server)
  (websocket/wallet::stop-server)
  (core-crypto:shutdown))




(defun writebc (&optional (filename "block-chain.loenc"))
  (let ((f (merge-pathnames (emotiq/filesystem:emotiq/user/root/) filename)))
    (with-open-file (o f
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-exists :supersede)
      (loenc:serialize emotiq/app::*blocks* o)
      (loenc:serialize emotiq/app::*genesis* o)
      (loenc:serialize emotiq/app::*alice* o)
      (loenc:serialize emotiq/app::*bob* o)
      (loenc:serialize emotiq/app::*mary* o)
      (loenc:serialize emotiq/app::*james* o)
  (values))))

(defun readbc (&optional (filename "block-chain.loenc"))
  (let ((f (merge-pathnames (emotiq/filesystem:emotiq/user/root/) filename)))
    (with-open-file (o f
                       :element-type '(unsigned-byte 8)
                       :direction :input)
      (setf emotiq/app::*blocks* (loenc:deserialize o))
      (setf emotiq/app:*genesis* (loenc:deserialize o))
      (setf emotiq/app:*alice* (loenc:deserialize o))
      (setf emotiq/app:*bob* (loenc:deserialize o))
      (setf emotiq/app:*mary* (loenc:deserialize o))
      (setf emotiq/app:*james* (loenc:deserialize o))))
  (format t "~%blockchain now available in emotiq/app:*blocks*~%~%")
  emotiq/app::*blocks*)


  