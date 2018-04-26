;; hacks of lisp-unit tests from test/blockchain-test.lisp
;; usage:
;; (emotiq:enode)
;; > test ;; sets up the blockchain
;; > bob  ;; switches to Bob wallet
;; > bal  ;; shows Bob's balance



(in-package #:emotiq)

;;; Documentation for LISP-UNIT testing framework is here:
;;;
;;;   https://github.com/OdonataResearchLLC/lisp-unit/wiki/Reference
;;;
;;; There's also good documentation here, written by the original author of the
;;; software, although it seems to be a bit out of date:
;;;
;;;   https://www.cs.northwestern.edu/academics/courses/325/readings/lisp-unit.php#usage



;;;; Blockchain Unit Tests





;;;; Blockchain Demo/Test



;; To generate keys for these tests, for now, use:
;;
;;   (multiple-value-setq (skey pkey pkey-hash) (keygen))

;;(progn 

  (defparameter *alice-pkey-hash* "3bab17cf1c0d97d88e28ef7af96dbe60b8daef26")
  (defparameter *alice-pkey* "7ac0728d574b2269628e2ec57d9520a74f7f03877d5b38c9c94221556679c7fa")
  (defparameter *alice-skey* "0e765439d985ff2c70bbe42f1f84b6418afc3fc56dc85872a028025503b49516")

  (defparameter *bob-pkey-hash* "220a3dd905912f74d004bf83989f2b76f41b7d85")
  (defparameter *bob-pkey* "0762e9167c4f22c7e537a95128ca5eedc3a0b4a7e9fdaf300647ac8fa2555113")
  (defparameter *bob-skey* "95b5fce84d948dcc2120ca9e93028e21e806a4ed2edb55a006492479c77cc67a")

  (defparameter *cindy-pkey-hash* "2a8ef419267c2700e32f9941ca8f37a62b80faf5")
  (defparameter *cindy-pkey* "de942256ecd8c7b717b06322d233a0782c520987214f7dcb6e826d1f41a9741d")
  (defparameter *cindy-skey* "1c46a159d771c5a1f89451fea875742424811447296268745386869758843aeb")

  (defparameter *david-pkey-hash* "8a5aa89686f7e546fdb4dd0b8f2bf119f72528bb")
  (defparameter *david-pkey* "0e27079c886b963a946878696f3682070609e504753a53360fe6137c5bd78d20")
  (defparameter *david-skey* "0f48e3479e134278ccd55a841cfe952a4c54d9dc87482ad98a1836952469b694")


  ;; Set up "minter-a" and "minter-b" and keys, for testing stake
  ;; transactions. (Note: values key values vary each new Lisp session, which
  ;; should be ok.)
  (defvar *minter-a-pkey-hash*)
  (defvar *minter-a-pkey*)
  (defvar *minter-a-skey*)

  (defvar *minter-b-pkey-hash*)
  (defvar *minter-b-pkey*)
  (defvar *minter-b-skey*)

  (multiple-value-setq (*minter-a-skey* *minter-a-pkey* *minter-a-pkey-hash*)
    (keygen))
  (multiple-value-setq (*minter-b-skey* *minter-b-pkey* *minter-b-pkey-hash*)
    (keygen))

;;  )



(defun tx-input-sequence (previous-tx index public-key private-key
                          &rest additional)
  (list* (list (emotiq:transaction-id previous-tx) index public-key private-key)
         (when additional
           (loop for (tx idx pk sk) on additional by #'cddr
                 collect `(list ,(emotiq:transaction-id tx) ,idx ,pk ,sk)))))
  


(defvar *tx-test-attempt-counter* 0)
(defvar *tx-test-success-counter* 0)
(defvar *tx-test-prev-result* nil)
(defvar *tx-test-prev-tx* nil)          ; once it's good, it stays good
(defvar *test-blockchain-context* nil)

(defun next-tx-test (input-specs output-specs &key restart)
  (when restart
    (setq *tx-test-attempt-counter* 0)
    (setq *tx-test-success-counter* 0)
    (setq *tx-test-prev-result* nil)
    (setq *tx-test-prev-tx* nil)
    (setq *test-blockchain-context* (emotiq:start-blockchain-context)))
  (let ((tx (emotiq:with-blockchain-context (*test-blockchain-context*)
              (if restart
                  (emotiq:last-transaction)
                  (next-transaction input-specs output-specs)))))
    (setq *tx-test-prev-result* tx)
    (incf *tx-test-attempt-counter*)
    (cond
      (tx
       (setq *tx-test-prev-tx* tx)
       (incf *tx-test-success-counter*)
       (if restart
           (format t "~%(GENESIS)~%  Tx-~d: ~A~%" *tx-test-success-counter* tx)
           (format t "~%  Tx-~d: ~A~%" *tx-test-success-counter* tx))))))




;;; The following sets of args are order-dependent. I.e., first set are
;;; fine. The second are double-spends coming after this first set.  The third
;;; refer to transactions never were known.

(defun blockchain-test-1 ()
  "Test by resetting the blockchain, which issues everything to Minter 0. They
send 51000 to Alice, who sends 40000 to Bob, who sends 20000 to David, 5000 back
to Alice, and 1000 to David."    

  (next-tx-test nil nil :restart t)
  ;; Genesis Block Coinbase Tx gives everything to Minter 0 initially.

  ;; Then they send 51000 to Alice:
  (next-tx-test
   (tx-input-sequence *tx-test-prev-tx* 0 emotiq:*minter-0-pkey* emotiq:*minter-0-skey*)
   `((,*alice-pkey-hash* 51000)        ; Minter 0 pays 51,000 to Alice
     (,emotiq:*minter-0-pkey-hash* ,(- (emotiq:initial-total-coin-amount) 51000))))

  (next-tx-test
   (tx-input-sequence *tx-test-prev-tx* 0 *alice-pkey* *alice-skey*)
   `((,*bob-pkey-hash* 40000)           ; Alice pays 40,000 to Bob
     (,*alice-pkey-hash* ,(- 51000 40000)))) ; change back: 11,000 

  (next-tx-test
   (tx-input-sequence *tx-test-prev-tx* 0 *bob-pkey* *bob-skey*)
   `((,*david-pkey-hash* 20000)         ; Bob pays 20,000 to David
     (,*bob-pkey-hash* ,(- 40000 20000)))) ; change back: 20,000

  (next-tx-test
   (tx-input-sequence *tx-test-prev-tx* 1 *bob-pkey* *bob-skey*)
   `((,*alice-pkey-hash* 5000)              ; Bob pays 5,000 to Alice
     (,*bob-pkey-hash* ,(- 20000 5000))))   ; change back: 15,000

  (next-tx-test
   (tx-input-sequence *tx-test-prev-tx* 1 *bob-pkey* *bob-skey*)
   `((,*david-pkey-hash* 1000)               ; Bob pays 1,000 to David
     (,*bob-pkey-hash* ,(- 15000 1000)))))   ; change back: 14,000



(defun get-test-blockchain-genesis-tx ()
  (emotiq:with-blockchain-context (*test-blockchain-context*)
    (emotiq:hash-pointer-item
     (emotiq:hash-pointer-of-transaction
      (emotiq:genesis-block)))))

(defun get-test-blockchain-penultimate-tx ()
  (emotiq:with-blockchain-context (*test-blockchain-context*)
    (emotiq:hash-pointer-item
     (emotiq:hash-pointer-of-transaction
      (emotiq:hash-pointer-item
       (emotiq:hash-pointer-of-previous-block
        (emotiq:last-block)))))))

(defun blockchain-test-2 ()
  "Test by first invoking function blockchain-test-1, q.v., and then attempting
two double-spends: trying to send 40000 again from Alice to Bob (spending the
output of the first transaction, which had already been spent), and then trying
to send 1000 again from Bob to Alice, from the second-to-last transaction, which
had just been used in the last transaction."
  (blockchain-test-1)
  (let ((genesis-tx (get-test-blockchain-genesis-tx))
        (penultimate-tx (get-test-blockchain-penultimate-tx)))
    (next-tx-test
     (tx-input-sequence genesis-tx 0    ; <= bad - already used Tx!
                        *alice-pkey* *alice-skey*)
     `((,*bob-pkey-hash* 40000)
       (,*alice-pkey-hash* ,(- 51000 40000))))
    (next-tx-test
     (tx-input-sequence penultimate-tx 1 ; <= bad - already used Tx!
                        *david-pkey* *david-skey*)
     `((,*david-pkey-hash* 1000) (,*bob-pkey-hash* 14000)))))



(defun get-test-blockchain-last-tx ()
  (emotiq:with-blockchain-context (*test-blockchain-context*)
    (emotiq:last-transaction)))

(defun blockchain-test-3 ()
  "Test by first invoking function blockchain-test-2, q.v., and then attempting
.... [See inline comments in code for full details.]"
  (blockchain-test-2)

  ;; Now the last transaction (stored in variable (emotiq:last-transaction)) sent
  ;; 14,000 to Bob as the 2nd output (as change). Try to spend 15,000 from
  ;; (emotiq:last-transaction)'s, sending it to Alice, but it should fail, since
  ;; there's only 14,000 available:
  (next-tx-test
   (tx-input-sequence (get-test-blockchain-last-tx) 1 ; <= bad - overspend!
                      *bob-pkey* *bob-skey*)
   `((,*alice-pkey-hash*
      15000)))                          ; <= too many units
  ;; Now try to have Bob send 14,000 to Alice. Since that's exactly what was
  ;; there, this will leave zero (0) extra, meaning zero as a transaction fee
  ;; (!). This will be allowed (for now).  Maybe zero transaction fees are
  ;; kind of iffy, but review that issue later!
  (next-tx-test
   (tx-input-sequence (get-test-blockchain-last-tx) 1 ; <= GOOD TRANSACTION HERE
                      *bob-pkey* *bob-skey*)
   `((,*alice-pkey-hash* 
      14000)))                       ; <= leaves 0 change, 0 for transaction fee
  ;; Now try spending zero (0) from Alice back to Bob. Should be rejected:
  ;; zero or negative amounts cannot be transferred.

  (next-tx-test
   (tx-input-sequence *tx-test-prev-tx* 0
                      *alice-pkey* *alice-skey*)
   `((,*bob-pkey-hash* 0)))           ; <= bad (attempt to transfer zero amount)
  ;; Now try spending negative (-1) million from Alice back to Bob. Should
  ;; be rejected: zero or negative amounts cannot be transferred.

  (next-tx-test
   (tx-input-sequence *tx-test-prev-tx* 0
                      *alice-pkey* *alice-skey*)
   `((,*bob-pkey-hash* -1000000))) ; <= bad (attempt to transfer negative amount)
  )



(defvar *nonexistent-test-transaction-id*
  (emotiq:hash-256-string "arbitrary-hash-doesnotexist")
  "A piece of 'fixture data' just be to be used in the next test
   as a dummy transaction ID, i.e., for a dummy reference to
   a transaction that does not exist, i.e., for testing a bad
   reference in a transaction.")

(defun blockchain-test-4 ()
  "Test by first invoking function blockchain-test-3, q.v., and then attempting
   a couple more bad transactions: one with an index out of range, and one that
   tries to use as input a nonexistent transaction."
  (blockchain-test-3)
  (next-tx-test
   (tx-input-sequence (get-test-blockchain-last-tx) 13 ; <= bad - index out of range!
                      *alice-pkey* *alice-skey*)
   `((,*bob-pkey-hash* 4000)
     (,*alice-pkey-hash* (- 51000 4000))))
    
    
  (next-tx-test
   (list (list *nonexistent-test-transaction-id* ; <= bad: nonexistent TX ID
               0 *bob-pkey* *bob-skey*))
   `((,*alice-pkey-hash* 5000) 
     (,*bob-pkey-hash* ,(- 20000 5000)))))




(defun print-test-blockchain ()  
  (emotiq:with-blockchain-context (*test-blockchain-context*)
    (emotiq:print-blockchain-info)))

(defun print-test-balance-summaries (list-of-pkey-hash-parameter-names)
  (emotiq:with-blockchain-context (*test-blockchain-context*)
    (loop for parameter-name in list-of-pkey-hash-parameter-names
          as address
            = (or (symbol-value parameter-name)
                  (cerror
                   "Continue"
                   "What is ~a? It evaluated to nil, not a public key hash."
                   parameter-name))
          initially
             (format t "~%  ~(~A~) ~25T~(~A~) ~70T~D"
                     "parameter-name" "address" "balance amount")
          when address
            do (let ((balance-amount (emotiq:get-balance address)))
                 (format t "~%  ~(~A~) ~25T~(~A~) ~70T~D"
                         parameter-name address balance-amount)))))



(defparameter *test-pkey-hash-parameter-names*
  '(emotiq:*minter-0-pkey-hash*         ; initial coinbase recipient, in BLOCKCHAIN
    *alice-pkey-hash* *bob-pkey-hash*
    *cindy-pkey-hash* *david-pkey-hash*
    *minter-a-pkey-hash* *minter-b-pkey-hash*)
  "A list of parameter-name symbols each of whose symbol-value is a
  public key hash used in these tests, such as *ALICE-PKEY-HASH*, et
  al.")




;;; TEST-BLOCKCHAIN: run the set of tests (above) logging results on the
;;; terminal and side affecting various globals. This is exported so it can be
;;; run from top level for quick interactive demoing and testing out.

(defun test-blockchain ()
  "Run extremely basic transaction functionality demo/tests."
  (blockchain-test-4)
  (format t "~%---~%Transactions tests finished. Now printing blockchain...~%")
  (print-test-blockchain)
  (format t "~%---~%Balance Summaries...~%")
  (print-test-balance-summaries *test-pkey-hash-parameter-names*)
  (format t "~%DONE.~%"))



(defun stake-test ()
  "Test a stake transaction."

  ;; Now make a coinbase transaction to a minter.  Its output is a
  ;; regular locking script as for a spend transaction.
  ;; Next-coinbase-transaction puts in a STAKE locking script.
  (let (stake-tx
        first-tx)

    ;; Start anew. We do not do anything with the Genesis block right
    ;; now, but we could, maybe should.  Just ignore this for now, use a
    ;; coinbase transaction to a "minter" next...
    (next-tx-test nil nil :restart t)
    (setq first-tx (emotiq:last-transaction))

    (setq stake-tx
          (emotiq:next-coinbase-transaction *minter-a-pkey*))
    (loop with coin = 51000
          with draw = 1000
          with times-before-unstake = 3
          for times from 0
          as index = 0 then 1
          until (> times times-before-unstake)
          do ;; Now magically have > 3 transactions go by...
             (decf coin draw)
             (next-tx-test
              ;; Alice pays 1,000 to Bob from 1st Tx/index 0:
              (tx-input-sequence 
               (if (= index 0)
                   first-tx
                   (emotiq:last-transaction))
               index *alice-pkey* *alice-skey*)
              `((,*bob-pkey-hash* ,draw) (,*alice-pkey-hash* ,coin)))
          finally 
             (format t "~%Try to unstake now...")
             (when (y-or-n-p "Ready?")
               ;; Unstake here just means minter pays self back from
               ;; stake-tx.
               (next-tx-test 
                (tx-input-sequence 
                 stake-tx 0 *minter-a-pkey* *minter-a-skey*)
                `((,*minter-a-pkey-hash* 51000)))))))


;; testing context only - keys are out in the open

(defparameter *pkey-hash* nil)
(defparameter *pkey* nil)
(defparameter *skey* nil)

(defun set-owner (str)
  (if (string= "alice" str)
      (setf *pkey-hash* *alice-pkey-hash*
            *pkey* *alice-pkey*
            *skey* *alice-skey*)
    (if (string= "bob" str)
      (setf *pkey-hash* *bob-pkey-hash*
            *pkey* *bob-pkey*
            *skey* *bob-skey*)
      (format t "bad name ~a~%"))))

(defun print-balance ()
  (emotiq:with-blockchain-context (*test-blockchain-context*)
    (let ((balance-amount (emotiq:get-balance *pkey-hash*)))
      (format t "~a~%"
              balance-amount))))