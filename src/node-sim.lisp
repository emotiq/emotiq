;; use (run) to run all-cloaked transactions
;; use (run :cloaked nil) to run all-uncloaked transactions
;; use (emotiq/sim:run-new-tx) to run new-style transactions
;; pretty cheesy at the moment -  cloaked was working, copy/paste to make uncloaked

;; top-level glue code for a simulation node
 
(in-package :emotiq/sim)

;; remove for production
(defun eassert (bool-condition)
  (assert bool-condition))

(defun initialize (&key
                     (cosi-prepare-timeout 40)
                     (cosi-commit-timeout 10)
                     (executive-threads nil)
                     (nodes 8)
                     (new-configuration-p nil)
                     (run-cli-p nil))
  "Initialize a new local simulation of the Emotiq chain

The simulation can be configured to run across the number of EXECUTIVE-THREADS 

COSI-PREPARE-TIMEOUT specifies how many seconds that cosi leaders wait
for responses during prepare phase.  The default value is 40.  For
simulations involving computational expensive cloaked transactions
across many nodes, this value may need to be larger to allow the
blocks to be sealed.

COSI-COMMIT-TIMEOUT specifies how many seconds that cosi leaders wait
for responses during commit phase.

Prepare can possibly take longer than commit, because the block may contain txns that a witness has not
yet seen (and, therefore, needs to validate the unseen txn(s))

If either NEW-CONFIGURATION-P is true or the simulator has never been
run on this node, a new simulation network will be generated.  The
configured simulation will have the integer number of NODES as
witnesses.  

N.B. :nodes has no effect unless a new configuration has been triggered (see above)."

  #+OPENMCL
  (when (find-package :gui)
    (setf emotiq:*notestream* (funcall (intern "MAKE-LOG-WINDOW" :gui) "Emotiq Log")))
  (setf cosi-simgen::*use-real-gossip* nil)
  (setf actors::*maximum-age* 120)
  (when executive-threads
    (actors:set-executive-pool executive-threads))
  (when (or new-configuration-p
              (not (and (probe-file cosi-simgen:*default-data-file*)
                        (probe-file cosi-simgen:*default-key-file*))))
    (cosi-simgen:generate-tree :nodes nodes))
  (gossip:gossip-startup)
  (setf cosi-simgen:*cosi-prepare-timeout* cosi-prepare-timeout)
  (setf cosi-simgen:*cosi-commit-timeout* cosi-commit-timeout)
  (cosi-simgen:init-sim)

  ;; should this following code be executed every time or only when a new configuration is created?
  (phony-up-nodes)
  (emotiq/elections:set-nodes (keys-and-stakes))
  (emotiq/tracker:start-tracker)
  (if run-cli-p
      (emotiq/cli:main)
      ;;; FIXME:  return nil when initialization is not successful
      t))

(defvar *genesis-account*
  nil
  "Genesis account.")

(defvar *genesis-output*
  nil
  "Genesis UTXO.")

#+OBSOLETE
(defun broadcast-message (&rest message)
  (loop
     :for node :across cosi-simgen:*node-bit-tbl*
     :doing (apply 'cosi-simgen:send (cosi-simgen:node-pkey node) message)))

(defun send-genesis-utxo (&key (monetary-supply 1000) (cloaked t))
  (when *genesis-output*
    (error "Can't create more than one genesis UTXO."))
  (print "Construct Genesis UTXO")
  (multiple-value-bind (utxog secrg)
      (if cloaked
          (cosi/proofs:make-cloaked-txout monetary-supply (pbc:keying-triple-pkey *genesis-account*))
        (cosi/proofs:make-uncloaked-txout monetary-supply (pbc:keying-triple-pkey *genesis-account*)))
    (declare (ignore secrg))
    (eassert (cosi/proofs:validate-txout utxog))
    (setf *genesis-output* utxog)
    (cosi-simgen:gossip-neighborcast nil
                                     :genesis-utxo
                                     :utxo utxog)
    utxog))

(defun create-transaction (from-account from-utxo amount-list to-pkey-list fee &key (cloaked t))
  (let ((from-skey (pbc:keying-triple-skey from-account))
	(from-pkey (pbc:keying-triple-pkey from-account)))
    (let* ((to-info (when cloaked (cosi/proofs:decrypt-txout-info from-utxo from-skey)))
           (amt (if cloaked
                    (cosi/proofs:txout-secr-amt to-info)
                  (cosi/proofs:uncloaked-txout-amt from-utxo)))
           (gamma (if cloaked
                      (cosi/proofs:txout-secr-gamma to-info)
                    (cosi/proofs:uncloaked-txout-gamma from-utxo)))
           (kind (if cloaked :cloaked :uncloaked))
	   (out-list (mapcar #'(lambda (amt to-pkey)
				 `(:kind ,kind
				   :amount ,amt
				   :pkey ,to-pkey))
			     amount-list to-pkey-list)))
      (cosi/proofs:make-transaction :ins `((:kind ,kind
                                            :amount ,amt
                                            :gamma  ,gamma
                                            :pkey   ,from-pkey
                                            :skey   ,from-skey))
                                    :outs out-list
                                    :fee fee))))

(defun publish-transaction (trans name)
  (unless (cosi/proofs:validate-transaction trans)
    (error (format nil "transaction ~A did not validate" name)))
  (format *error-output* "Broadcasting transaction ~a to all simulated nodes" name)
  (cosi-simgen:gossip-neighborcast nil
                                   :new-transaction
                                   :trn trans))

(defun force-epoch-end ()
  (ac:pr "force-epoch-end")
  (cosi-simgen:send cosi-simgen:*top-node* :make-block))

(defparameter *user-1* nil)
(defparameter *user-2* nil)
(defparameter *user-3* nil)
(defparameter *tx-1* nil)
(defparameter *tx-2* nil)

(defun ensure-simulation-keys ()
  (unless (and *genesis-account* *user-1* *user-2* *user-3*)
    (setf *genesis-account*
          (pbc:make-key-pair :genesis)
          
          *user-1*
          (pbc:make-key-pair :user-1)

          *user-2*
          (pbc:make-key-pair :user-2)
          
          *user-3*
          (pbc:make-key-pair :user-3))))

(defun run (&key (monetary-supply 1000) (cloaked t))
  "Run the block chain simulation entirely within the current process

This will spawn an actor which will asynchronously do the following:

  1.  Create a genesis transaction with MONETARY-SUPPLY coins.
      Transact 1000 coins to *USER-1*.  The resulting transaction
      can be referenced via *tx-1*.

  2.  In the transaction references as *TX-2*, *USER-1* sends 500
      coins to *USER-2*, 490 coins to *USER-3*, with a fee of 10 coins.
"
  (ensure-simulation-keys)

  (setf *genesis-output* nil
        *tx-1*           nil
        *tx-2*           nil)

  (cosi-simgen:reset-nodes)
 
  (emotiq/elections:make-election-beacon)
                                         
  (let ((fee 10)
        (user-1-pkey (pbc:keying-triple-pkey *user-1*))
        (user-2-pkey (pbc:keying-triple-pkey *user-2*))
        (user-3-pkey (pbc:keying-triple-pkey *user-3*)))
      
      (ac:pr "Construct Genesis transaction")
      (let ((genesis-utxo (send-genesis-utxo :monetary-supply monetary-supply :cloaked cloaked)))
        (let ((trans (create-transaction
                      *genesis-account* genesis-utxo
                                        ; user1 gets 1000 from genesis (fee = 0)
                      '(1000) (list user-1-pkey) 0 :cloaked cloaked)))
                                        ; force genesis block (leader-exec breaks if blockchain is nil)
          (publish-transaction (setf *tx-1* trans) "tx-1") ; 
          (ac:pr "Find UTX for user-1")
          (let ((from-utxo (cosi/proofs:find-txout-for-pkey-hash
                            (hash:hash/256 user-1-pkey)
                            trans)))
            (ac:pr "Construct 2nd transaction")
            (let ((trans (create-transaction
                          *user-1* from-utxo 
                                        ; user1 spends 500 to user2, 490 to user3, 10 for fee
                          '(500 490)
                          (list user-2-pkey user-3-pkey)
                          fee :cloaked cloaked)))
              ;; allow leader elections to create this block
              (publish-transaction (setf *tx-2* trans) "tx-2"))))))
  (sleep 60)
  (let ((result (emotiq/tracker:query-current-state)))
    (emotiq:note "current state = ~A" result)
    result))   ;;; return non-nil if we are able to exit cleanly

(defun run-new-tx ()
  "Using new tx feature, run the block chain simulation entirely within the current process.
  This will spawn an actor which will asynchronously do the following:
  
  1.  Create a genesis block (amount of coin is parameterized in the
  code).  Transact AMOUNT coins to *USER-1*.  The resulting
  transaction can be referenced via *tx-1*.
  
  2.  Transfer the some amount of coins from *user-1* to *user-2* as *tx2*.
  
  3.  Transfer some other amount of coins from *user-2* to *user-3* as *tx3*."
  
  (ensure-simulation-keys)
  (setf *genesis-output* nil *tx-1* nil *tx-2* nil)
  (cosi-simgen:reset-nodes)
  (let ((fee 10))    
    (ac:pr "Construct Genesis Block")
    (let* ((genesis-block
            (let ((cosi-simgen:*current-node* cosi-simgen:*top-node*))
              ;; Establish current-node binding of genesis node
              ;; around call to create genesis block.
              (cosi/proofs:create-genesis-block
               (pbc:keying-triple-pkey *genesis-account*)
               (keys-and-stakes))))
           (genesis-transaction         ; kludgey handling here
            (first (cosi/proofs:block-transactions genesis-block)))
           (genesis-public-key-hash
            (cosi/proofs:public-key-to-address (pbc:keying-triple-pkey *genesis-account*))))
      
      (emotiq:note "Tx 0 created/genesis, now broadcasting.")
      (cosi/proofs/newtx:dump-tx genesis-transaction)      
      (cosi-simgen:gossip-neighborcast nil :genesis-block :blk genesis-block)
      
      (let* ((txid
              (cosi/proofs/newtx:transaction-id genesis-transaction))
             (index 0)
             (transaction-inputs
              (cosi/proofs/newtx:make-transaction-inputs `((,txid ,index))))
             
             (user-1-public-key-hash    ; a/k/a address
              (cosi/proofs:public-key-to-address (pbc:keying-triple-pkey *user-1*)))
             (amount-1 890)             ; user gets 890 (fee = 10)
             (change-amount-1
              (- (cosi/proofs/newtx:initial-total-coin-amount) 
                 (+ amount-1 fee)))
             (transaction-outputs
              (cosi/proofs/newtx:make-transaction-outputs
               `((,user-1-public-key-hash ,amount-1)
                 (,genesis-public-key-hash ,change-amount-1))))
             (signed-transaction        ; signed by genesis account
              (cosi/proofs/newtx:make-and-maybe-sign-transaction
               transaction-inputs transaction-outputs
               :skeys (pbc:keying-triple-skey *genesis-account*)
               :pkeys (pbc:keying-triple-pkey *genesis-account*)
               :type :spend)))
        (setq *tx-1* signed-transaction)
        (ac:pr (format nil "Broadcasting 1st TX."))
        (emotiq:note "Tx 1 created/signed by genesis (~a), now broadcasting."
                     genesis-public-key-hash)
        (cosi/proofs/newtx:dump-tx signed-transaction)
        (cosi-simgen:gossip-neighborcast nil :new-transaction-new :trn signed-transaction)
        
        (let* ((user-2-public-key-hash
                (cosi/proofs:public-key-to-address (pbc:keying-triple-pkey *user-2*)))
               (amount-2 500)
               (change-amount-2 (- amount-1 (+ amount-2 fee))))
          
          (setq txid (cosi/proofs/newtx:transaction-id signed-transaction))
          (setq index 0)
          (setq transaction-inputs
                (cosi/proofs/newtx:make-transaction-inputs `((,txid ,index))))
          
          (setq transaction-outputs
                (cosi/proofs/newtx:make-transaction-outputs
                 `((,user-2-public-key-hash ,amount-2)
                   (,user-1-public-key-hash ,change-amount-2))))
          
          (setq signed-transaction      ; signed by user 1
                (cosi/proofs/newtx:make-and-maybe-sign-transaction
                 transaction-inputs transaction-outputs
                 :skeys (pbc:keying-triple-skey *user-1*)
                 :pkeys (pbc:keying-triple-pkey *user-1*)))
          (emotiq:note "Tx 2 created/signed by user-1 (~a), now broadcasting."
                       user-1-public-key-hash)
          (setq *tx-2* signed-transaction)
          (ac:pr (format nil "Broadcasting 2nd TX."))
          (cosi/proofs/newtx:dump-tx signed-transaction)
          (cosi-simgen:gossip-neighborcast nil :new-transaction-new :trn signed-transaction)
          
          (let* ((user-3-public-key-hash
                  (cosi/proofs:public-key-to-address (pbc:keying-triple-pkey *user-3*)))
                 (amount-3 350)
                 (change-amount-3 (- amount-2 (+ amount-3 fee))))
            
            (setq txid (cosi/proofs/newtx:transaction-id signed-transaction))
            (setq index 0)
            (setq transaction-inputs
                  (cosi/proofs/newtx:make-transaction-inputs `((,txid ,index))))
            
            (setq transaction-outputs
                  (cosi/proofs/newtx:make-transaction-outputs
                   `((,user-3-public-key-hash ,amount-3)
                     (,user-2-public-key-hash ,change-amount-3))))
            
            (setq signed-transaction    ; signed by user 2
                  (cosi/proofs/newtx:make-and-maybe-sign-transaction
                   transaction-inputs transaction-outputs
                   :skeys (pbc:keying-triple-skey *user-2*)
                   :pkeys (pbc:keying-triple-pkey *user-2*)))          
            (ac:pr (format nil "Broadcasting 3rd TX."))
            (emotiq:note "Tx 3 created/signed by user-2 (~a), now broadcasting."
                         user-2-public-key-hash)
            (cosi/proofs/newtx:dump-tx signed-transaction)
            (cosi-simgen:gossip-neighborcast nil :new-transaction-new :trn signed-transaction)
            
            ;; here: attempt a double-spend: (with same TxID)
            (setq signed-transaction
                  (cosi/proofs/newtx:make-and-maybe-sign-transaction
                   transaction-inputs transaction-outputs
                   :skeys (pbc:keying-triple-skey *user-2*)
                   :pkeys (pbc:keying-triple-pkey *user-2*)))
            
            (ac:pr (format nil "Broadcasting 4th TX [attempt to double-spend (same TxID)]."))
            (emotiq:note "Tx 4 created/signed by user-2 (~a) [attempt to double-spend (same TxID)], now broadcasting."
                         user-2-public-key-hash)
            (cosi-simgen:gossip-neighborcast nil :new-transaction-new :trn signed-transaction)
            
            ;; here: attempt a double-spend: (with different TxID)
            (setq transaction-outputs
                  (cosi/proofs/newtx:make-transaction-outputs
                   `((,user-3-public-key-hash 123)
                     (,user-2-public-key-hash 321))))
            (setq signed-transaction
                  (cosi/proofs/newtx:make-and-maybe-sign-transaction
                   transaction-inputs transaction-outputs
                   :skeys (pbc:keying-triple-skey *user-2*)
                   :pkeys (pbc:keying-triple-pkey *user-2*)
                   :type ':spend))
            
            (ac:pr (format nil "Broadcasting 5th TX [attempt to double-spend (diff TxID)]."))
            (emotiq:note "Tx 5 created/signed by user-2 (~a) [attempt to double-spend (diff TxID)], now broadcasting."
                         user-2-public-key-hash)
            (cosi-simgen:gossip-neighborcast nil :new-transaction-new :trn signed-transaction)
            
            
            (emotiq/elections:fire-election)
            
            ;; Dump the whole blockchain now after about a minute,
            ;; just before exiting:
            (multiple-value-bind (all-done-p elapsed-seconds-if-done)
                                 (cosi/proofs/newtx:wait-for-tx-count 3 :timeout 60)
              (cond
               (all-done-p
                (emotiq:note "Finished ~d spend transactions in ~d second~p"
                             3 elapsed-seconds-if-done elapsed-seconds-if-done))
               (t
                (cerror
                 "Continue regardless."
                 "Timed out, waited 60 sec for 4 transactions on blockchain."))))
            ;; previous:
            ;; (sleep 60)
            (emotiq:note "~2%Here's a dump of the whole blockchain currently:")
            (cosi/proofs/newtx:dump-txs :blockchain t)
            (emotiq:note "Good-bye and good luck!"))))))
  (let ((result (emotiq/tracker:query-current-state)))
    (emotiq:note "current state = ~A" result)
    result))  ;;; return non-nil if we are able to exit cleanly


(defun run-new-tx-1 ()
  "Using new tx feature, run the block chain simulation entirely within the current process.
This will spawn an actor which will asynchronously do the following:

  1.  Create a genesis block (amount of coin is parameterized in the
      code).  Transact AMOUNT coins to *USER-1*.  The resulting
      transaction can be referenced via *tx-1*.

  2.  Transfer the some amount of coins from *user-1* to *user-2* as *tx2*.

  3.  Transfer some other amount of coins from *user-2* to *user-3* as *tx3*."

  (ensure-simulation-keys)
  (setf *genesis-output* nil *tx-1* nil *tx-2* nil)
  (cosi-simgen:reset-nodes)
  (let ((fee 10))    
    (ac:pr "Construct Genesis Block")
    (let* ((genesis-block
            (let ((cosi-simgen:*current-node* cosi-simgen:*top-node*))
              ;; Establish current-node binding of genesis node
              ;; around call to create genesis block.
              (cosi/proofs:create-genesis-block
               (pbc:keying-triple-pkey *genesis-account*)
               (keys-and-stakes))))
           (genesis-transaction (first (cosi/proofs:block-transactions genesis-block)))
           (genesis-address (emotiq/txn:address *genesis-account*))
           (user-1-address (emotiq/txn:address *user-1*))
           (user-2-address (emotiq/txn:address *user-2*))
           (user-3-address (emotiq/txn:address *user-3*))
           (last-transaction nil))

      (emotiq:note "Tx 0 created/genesis, now broadcasting.")
      (cosi/proofs/newtx:dump-tx genesis-transaction)      

      (cosi-simgen:gossip-neighborcast nil :genesis-block :blk genesis-block)
      (sleep 10.0)
      
      (setf last-transaction 
            (emotiq/txn:make-spend-transaction *genesis-account*
                                               user-1-address
                                               890
                                               :fee fee))
        
      (setf *tx-1* last-transaction)
      (emotiq:note "Tx 1 [GENESIS -> USER-1] created/signed by genesis (~a), now broadcasting."
                   genesis-address)
      (cosi/proofs/newtx:dump-tx last-transaction)

      (cosi-simgen:gossip-neighborcast nil :new-transaction-new :trn last-transaction)
      (emotiq/elections:fire-election)
      (assert (cosi/proofs/newtx:wait-for-tx-count 1 :timeout 120.0))
      
      (setf last-transaction
            (emotiq/txn:make-spend-transaction *user-1*
                                               user-2-address
                                               500
                                               :fee fee))
      (setf *tx-2* last-transaction)
      (emotiq:note "Tx 2 [USER-1 -> USER-2] created/signed by user-1 (~a), now broadcasting."
                   user-1-address)
      (cosi/proofs/newtx:dump-tx last-transaction)
          
      (cosi-simgen:gossip-neighborcast nil :new-transaction-new :trn last-transaction)
      (emotiq/elections:fire-election)
      (assert (cosi/proofs/newtx:wait-for-tx-count 2 :timeout 120.0))
          
      (setf last-transaction
            (emotiq/txn:make-spend-transaction *user-2*
                                               user-3-address
                                               350
                                               :fee fee))
            
      (emotiq:note "Tx 3 [USER-2 -> USER-3] created/signed by user-2 (~a), now broadcasting."
                   user-2-address)
      (cosi/proofs/newtx:dump-tx last-transaction)

      (cosi-simgen:gossip-neighborcast nil :new-transaction-new :trn last-transaction)
      (emotiq/elections:fire-election)
      (assert (cosi/proofs/newtx:wait-for-tx-count 3 :timeout 120.0))
          
      ;; here: attempt a double-spend: (with same TxID)
      (emotiq:note "Tx 4 [USER-2 -> USER-3] created/signed by user-2 (~a) [attempt to double-spend (same TxID)], now broadcasting." user-2-address)
            
      (cosi-simgen:gossip-neighborcast nil :new-transaction-new :trn last-transaction)

      ;; this sleep is here so we can manually see in a log the double spend attemp
      ;; better (testable) approach would be to have a way to subscribe to a tracker messages
      ;; and block until we receive a message of particular type
      (sleep 20.0)
      
      ;; NOTE:
      ;; Previously there was attempt to double-spend with different TxID here.
      ;; Since we look for UTXOs in blockchain now and don't craft input transactions manually,
      ;; it's not possible to create such call using the API function make-spend-transaction.
                 
      (emotiq:note "~2%Here's a dump of the whole blockchain currently:")
      (cosi/proofs/newtx:dump-txs :blockchain t)
      (format t "~2%Good-bye and good luck!~%")
      (emotiq/tracker:query-current-state))))

(defun blocks ()
  "Return the blocks in the chain currently under local simulation

The blocks constituting the chain are from the view of the `top-node` of
the cosi-simgen implementation of the simulator."
  (cosi-simgen:node-blockchain cosi-simgen:*top-node*))

(defun kill-beacon ()
  (emotiq/elections:kill-beacon))

;; ----------------------------------------------------------------
;; These disappear once Gossip is installed...
;; 

(defun phony-up-nodes ()
  (cosi-simgen:set-nodes
   (um:accum acc
     (maphash (lambda (k node)
                (declare (ignore k))
                (let ((pkey  (cosi-simgen:node-pkey node))
                      (stake (random 100000)))
                  (setf (cosi-simgen:node-stake node) stake)
                  (ac:pr (list pkey stake))
                  (acc (list pkey stake))))
              cosi-simgen:*ip-node-tbl*))))

(defun keys-and-stakes ()
  "Return a list of lists of public key and stake for nodes"
  (cosi-simgen:get-witness-list))

;; END? of "those which disappear once Gossip in installed…"
;; ----------------------------------------------------------------

;;;; Single Node REPL

(defun node-repl (&key node message)
  "This runs a read-eval-print loop (REPL) for a single node. If NODE
   is nil, this uses cosi-simgen:*top-node*. If initialization has not
   been done (cosi-simgen:*top-node* is nil), this does the
   initialization."
  (when (null cosi-simgen:*top-node*)
    (emotiq:note "Initialization needed....")
    (emotiq/sim:initialize)
    (emotiq:note "Initialization DONE."))
  (let ((cosi-simgen:*current-node*
          (or node cosi-simgen:*top-node*)))
    (node-repl-loop
     "~a (node = ~s)"
     (or message "Node REPL")
     cosi-simgen:*current-node*)))


(defparameter *node-repl-prompt* "Node Repl> ")
(defparameter *node-repl-quitters* '(:quit :q :a))

(defun node-repl-loop (format-string &rest format-args)
  (let ((banner (apply #'format nil format-string format-args)))
    (emotiq:note "~a" banner)
    (emotiq:note "~a" (make-string (length banner) :initial-element #\-))
    (loop for form 
            = (progn (emotiq:note "~a" *node-repl-prompt*) (read))
          when (member form *node-repl-quitters*)
            do (emotiq:note "Quitting")
               (return)
          do (print (eval form)))))

;; To do: spend a few minutes seeing if there's already a CL-REPL
;; someone did (open source), especially one integrated with SLIME?

;; To do: wrap some error handling to catch simple read errors, catch
;; if they really want to exit to top level, etc.  For example, if you
;; type in a variable name that's unbound, and it pops you into a
;; debugger, and you abort from debugger, it should return to this
;; REPL, not to the Lisp REPL.

;; To do: should have Lisp's REPL variables set (*, **, ***, +, ++,
;; +++).

;; To do/sad: in SLIME when this prompts, you lose SLIME
;; completion. How to get that back?
