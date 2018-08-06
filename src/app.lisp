;; test usage:
;; (emotiq/app::test-app)

(in-package :emotiq/app)

;; for testing
(defparameter *tx0* nil)
(defparameter *tx1* nil)
(defparameter *tx2* nil)
(defparameter *tx3* nil)
(defparameter *tx4* nil)

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

(defun test-app ()
  "entry point for tests"
  (let ((strm emotiq:*notestream*))
    (emotiq:main)
    (setf gossip::*debug-level* nil)
    (app)
    (dump-results strm)
    (generate-pseudo-random-transactions)
    #+nil(block-explorer)));; not tested

(defun app ()
  "helper for tests"

  (wait-for-node-startup)

  (setf *genesis* (make-genesis-account))

(let ((bal (get-balance *genesis*)))
  (emotiq:note "balance for genesis is ~A" bal))

  (setf *alice* (make-account "alice")
	*bob* (make-account "bob")
	*mary* (make-account "mary")
        *james* (make-account "james"))

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
     (dumpamounts)
     (setf emotiq:*notestream* strm)
     (values))))

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
;; 
;; I'm keeping this code around until everything works.

#+nil
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


;; 
;; potentially useful functions in new-transactions.lisp
;;
; find-transaction-per-id
; make-transaction-inputs
; verify-transaction
; check-public-keys-for-transaction-inputs
; utxo-p
; find-tx


(defun block-explorer ()
  (let ((b-list (cosi-simgen:block-list)))
    (mapc #'(lambda (b) 
              (cosi/proofs/newtx::do-transactions (tx b)
                (explore-transaction tx)))
          b-list)))

(defun explore-transaction (tx)
  (let ((txid (cosi/proofs/newtx:hash-transaction-id tx)))  ;; TODO: use with-block-list macro
    (let ((my-address (get-transaction-address tx)))
      (let ((out-list (get-transaction-outs tx)))
        (let ((in-list (get-transaction-ins tx)))
          (let ((out-txid-list (mapcar #'cosi/proofs/newtx::tx-out-public-key-hash out-list)))
            (let ((in-txid-list (mapcar #'(lambda (in)
                                            (emotiq/txn:address (cosi/proofs/newtx::%tx-in-public-key in)))
                                        in-list)))
              (let ((in-address-list (mapcar #'get-transaction-address in-list)))
                (let ((out-address-list (mapcar #'get-transaction-address out-list)))
                  ;; at this point, 
                  ;; tx == the given transaction CLOS object
                  ;; txid == string txid for this transaction
                  ;; in-list == list of CLOS objects this tx gets inputs from
                  ;; in-txid-list == list of string id's this tx gets inputs from
                  ;; in-address-list == list of string address id's this tx gets inputs from
                  ;; out-list == list of tx CLOS objects that this tx outputs to
                  ;; out-txid-list == list of string id's that this tx outputs to
                  ;; out-address-list == list of string addrsss id's that this tx outputs to
                  ;;
                  ;; N.B. txid (string) is different from address (string) - txid refers to the transaction, address
                  ;; refers to the hashed-pkey of a Node
                  (emotiq:note "[txids] tx=~A in=~A out=~A, [addresses] tx=~A from=~A to=~A"
                               txid in-txid-list out-txid-list
                               my-address in-address-list out-address-list))))))))))
  
(defun get-transaction-outs (tx)
  "return a list of CLOS transactions that are outputs of this transaction"
  (cosi/proofs/newtx:transaction-outputs tx))

(defun get-transaction-ins (tx)
  "return a list of CLOS transactions that are inputs of this transaction"
  (cosi/proofs/newtx:transaction-inputs tx))

(defmethod get-transaction-address ((tx cosi/proofs/newtx::transaction))
  "return the address of tx"
  ;; we can get the address of tx by looking at its inputs and pulling out the pkey that the
  ;; inputs are aimed at

  ;; this turns out to be extra easy at the moment, because the current tx input data
  ;; structure has a field that contains the pkey - will this change in the future?

  ;; the actual blockchain transactions are optimized so that redundant information is not
  ;; stored - target pkeys appear in the txouts of transactions that feed into this one - using only that information,
  ;; this code would have to reach back to the input transactions to find the account pkey associated
  ;; with this transaction (in the txout of the input transaction(s)).

  (cosi/proofs/newtx::%tx-in-public-key (first (cosi/proofs/newtx::transaction-inputs tx))))


;;;;;;;;;;;;;;;;;;
; random transaction generator
;;;;;;;;;;;;;;;;;;

;; for lack of better graphics, let's used pipe syntax to show what I am building
;;
;; ticker | timer (randomizer) | transaction-creator
;; 
;; the pipe syntax cannot show that "ticker" is sending itself a message, endlessly
;;
;; maybe (loop ticker | ticker |2) | timer | transaction-creator
;;
;; these are honest message sends between concurrent pieces of code
;; the loopback to ticker is not recursion, it is a message send
;; 


(defparameter *all-accounts* nil
  "list of accounts which will be re-used whenever one of the from/to lists dry up, monitored")

(defparameter *from* nil
  "list of accounts which will send tokens from to *to*, monitored")

(defparameter *to* nil
  "list of accounts which will accept tokens, monitored")

;; for debug - hooks to the actors
(defparameter *ticker-actor* nil
  "wakes up periodically and sends 'random' events to create-transaction actor")

(defparameter *randomizer-actor* nil
  "wakes up periodically and sends 'random' events to create-transaction actor")

(defparameter *transaction-creator-actor* nil
  "creates a transaction using *from* and *to* lists, whenever pinged")

(useful-macros:defmonitor
    ((ensure-lists ()
       ;; helper that checks and resets (if necessary) the from and to lists
       ;; should be done atomically, hence in a (Hoare) monitor
       (when (or (null *from*)
                 (null *to*))
         (setf *from* *all-accounts*
               *to* (reverse *all-accounts*))))
  
     (get-from ()
       ;; return an account from the *from* list and pop
       (ensure-lists)
       (pop *from*))

     (get-to ()
       ;; return an account from the *to* list and pop
       (ensure-lists)
       (pop *to*))

     ;; end monitor
     ))

(defun generate-pseudo-random-transactions ()
  "spawn an actor that wakes up and spends tokens from some account in *from* to another account in *to*"
  (let ((keypairs (emotiq/config:get-keypairs))
        (nameindex -1))
    (let ((account-list (mapcar #'(lambda (pair)
                                    (make-account-given-keypairs
                                     (format nil "account~A" (incf nameindex))
                                     (first pair)
                                     (second pair)))
                                keypairs)))
      ;; some (any) kind of randomization of accounts
      ;; for now, run *front* from the beginning of *all-accounts* 
      ;; and *to* backwards
      ;; when one of them peters out, reset the lists
      (setf *all-accounts* account-list)
      (ensure-lists)

      ;; in general, using actors here is over-kill
      ;; actors are just an efficient way to invoke the process paradigm
      ;; processes/threads would be suffiecient here, since transactions only need
      ;; to be created in "human time" ; I'm using actors only because they are easily
      ;; at hand, for me

      (let ((tick-period 1) ;; bald guess at something that resembles "human time" and will allow a test set of transactions to propagate
            (node cosi-simgen::*my-node*))
        (setf *transaction-creator-actor*
              (actors:make-actor
               #'(lambda (&rest msg)
                   (transaction-creator
                    (first msg)
                    node
                    msg))))
        (setf *randomizer-actor*
              (actors:make-actor
               #'(lambda (&rest msg)
                   (random-timer-actor
                    *transaction-creator-actor*
                    (first msg)
                    node
                    msg))))
        (setf *ticker-actor* (actors:make-actor #'forever-ticker))
        (actors:send *transaction-creator-actor* :create-transaction node)
        #+nil(actors:send *ticker-actor* :again *ticker-actor* *rnadomizer-actor* tick-period node)))))

;; actor body
(defun forever-ticker (&rest msg)
  (destructuring-bind (msg-symbol self randomizer sleep-amount node)
      msg
    (emotiq:note "ticker ~s ~s ~s ~s ~s" msg-symbol self randomizer sleep-amount node)
    
    ;; timers and errors (catch/throw stuff) are nothing special, they are just "events"
    
    ;; this actor gets a periodic tick (using existing actor timer code)
    ;; then sends a message to the randomizer, forever ;; this is over-kill, but, I hope
    ;; it is very obvious
    
    ;; ignore the message, just send a tick to the randomizer
    
    (sleep sleep-amount)
    (emotiq:note "sending to randomizer")
    (actors:send randomizer :tick node)
    (emotiq:note "sending to self/forever-ticker")
    (actors:send self :again self randomizer sleep-amount node)))

(defun get-fire-p (n)
  "return T if RANDOM(1/n) is 25% of n or less"
  ;; repeatable random numbers are created with RANDOM, after calling emotiq/random:init-random
  (multiple-value-bind (num dem)
      (random n)
    (declare (ignore num))
    (truncate n 4)
    (zerop dem)))

;; actor body
(defun random-timer-actor (transaction-maker &rest msg)
  "after a pseudo-random number of ticks, wake the transaction-creator up"

  (let ((msg-symbol (first msg))
        (node (second msg)))
    
    ;; should not receive anything but :tick messages,
    ;; messages are always lists, and, by convention, the first token in a message is a keyword
    ;; dlambda is just a convenience
    (assert (eq :tick msg-symbol))
    
    (let ((fire-p t)) ; (get-fire-p 50)))
      (emotiq:note "timer fire-p ~s" fire-p)
      (when fire-p
        (actors:send transaction-maker :create-transaction node)))))
  
(defun create-an-amount-lower-or-equal-to (n)
  "apply some heuristic to generate an amount which we can use in a randomly-created transaction"
  n)

;; actor body
(defun transaction-creator (&rest msg)

  (let ((msg-symbol (first msg))
        (node (second msg)))
    
    (emotiq:note "transaction-creator ~A ~A" msg-symbol node)

    (assert (eq msg-symbol :create-transaction))

    (let ((to-account (get-to))
          (from-account (get-from))
          (from-bal 0)
          (max-froms 16) ;; a bald guess as to the number of from-accounts we will search before giving up
          (some-lower-limit 100) ;; bald guess at what kind of (random) amount we want to use
          (fee 10))

      ;; search for an account with sufficient balance to handle the amount + fees
      (dotimes (i max-froms)
        (let ((bal (get-balance from-account)))
          (cond ((>= bal (+ fee some-lower-limit))
                 (setf from-bal bal)
                 (return))
                ((< bal some-lower-limit)
                 (emotiq:note "account ~A balance is ~A" from-account bal)
                 (setf from-account (get-from))))))
      ;; loop ends with bal set to something >= (+ fee some-lower-limit), or,
      ;; it terminates with bal still = 0
      
      (if (< from-bal some-lower-limit)
          (emotiq:note "can't create a transaction, since ~A accounts do not have suffient funds (~A)"
                       max-froms (+ fee some-lower-limit))
        (let ((new-amount (create-an-amount-lower-or-equal-to some-lower-limit)))
          (emotiq:note "transaction creator making a transaction of ~A from ~A to ~A with fee ~A"
                       new-amount
                       from-account
                       to-account
                       fee)
          (spend from-account (account-pkey to-account) new-amount :fee fee))))))

(defun shutdown ()
  (r2-shutdown)
  (gossip::graceful-shutdown))