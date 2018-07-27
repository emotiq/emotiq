(in-package :blockchain-test)

(defun run-1 ()
  (sim::initialize :nodes 3 :new-configuration-p t)
  (sim::ensure-simulation-keys)
  (setf sim::*genesis-output* nil
        sim::*tx-1* nil
        sim::*tx-2* nil)
  (cosi-simgen:reset-nodes)
  (ac:pr "Construct Genesis Block")
  
  (let* ((genesis-block
          (let ((cosi-simgen:*current-node* cosi-simgen:*top-node*))
            ;; Establish current-node binding of genesis node
            ;; around call to create genesis block.
            (block:make-genesis-block (wallet:address sim::*genesis-account*)
                                      (sim:keys-and-stakes))))
         (genesis-transaction (first (block:transactions genesis-block))))

    (emotiq:note "Tx 0 created/genesis, now broadcasting.")
    (txn:dump genesis-transaction)      

    (cosi-simgen:gossip-neighborcast nil :genesis-block :blk genesis-block)
    (sleep 10.0)))

  

(defun run ()
  "Using new tx feature, run the block chain simulation entirely within the current process.
This will spawn an actor which will asynchronously do the following:

  1.  Create a genesis block (amount of coin is parameterized in the
      code).  Transact AMOUNT coins to *USER-1*.  The resulting
      transaction can be referenced via *tx-1*.

  2.  Transfer the some amount of coins from *user-1* to *user-2* as *tx2*.

  3.  Transfer some other amount of coins from *user-2* to *user-3* as *tx3*."

  (sim::initialize :nodes 3 :new-configuration-p t)
  (sim::ensure-simulation-keys)
  (setf sim::*genesis-output* nil
        sim::*tx-1* nil
        sim::*tx-2* nil)
  (cosi-simgen:reset-nodes)
  (let ((fee 10))    
    (ac:pr "Construct Genesis Block")
    (let* ((genesis-block
            (let ((cosi-simgen:*current-node* cosi-simgen:*top-node*))
              ;; Establish current-node binding of genesis node
              ;; around call to create genesis block.
              (block:make-genesis-block (wallet:address sim::*genesis-account*)
                                        (sim:keys-and-stakes))))
           (genesis-transaction (first (block:transactions genesis-block)))
           (genesis-address (wallet:address sim::*genesis-account*))
           (user-1-address (wallet:address sim::*user-1*))
           (user-2-address (wallet:address sim::*user-2*))
           (user-3-address (wallet:address sim::*user-3*))
           (last-transaction nil))

      (emotiq:note "Tx 0 created/genesis, now broadcasting | ID = ~A"
                   (txn:id genesis-transaction))
      (txn:dump genesis-transaction)      

      (cosi-simgen:gossip-neighborcast nil :genesis-block :blk genesis-block)
      (sleep 10.0)
      
      (setf last-transaction 
            (txn:make-spend-transaction sim::*genesis-account*
                                        user-1-address
                                        890
                                        :fee fee))
        
      (setf sim::*tx-1* last-transaction)
      (emotiq:note "Tx 1 [GENESIS -> USER-1] created/signed by genesis (~a), now broadcasting. | ID = ~A"
                   genesis-address (txn:id last-transaction))
      (txn:dump last-transaction)

      (cosi-simgen:gossip-neighborcast nil :new-transaction-new :trn last-transaction)
      (emotiq/elections:fire-election)
      (assert (txn::wait-for-tx-count 1 :timeout 120.0))
      
      (setf last-transaction
            (txn:make-spend-transaction sim::*user-1*
                                        user-2-address
                                        500
                                        :fee fee))
      (setf sim::*tx-2* last-transaction)
      (emotiq:note "Tx 2 [USER-1 -> USER-2] created/signed by user-1 (~a), now broadcasting."
                   user-1-address)
      (txn:dump last-transaction)
          
      (cosi-simgen:gossip-neighborcast nil :new-transaction-new :trn last-transaction)
      (emotiq/elections:fire-election)
      (assert (txn::wait-for-tx-count 2 :timeout 120.0))
          
      (setf last-transaction
            (txn:make-spend-transaction sim::*user-2*
                                        user-3-address
                                        350
                                        :fee fee))
            
      (emotiq:note "Tx 3 [USER-2 -> USER-3] created/signed by user-2 (~a), now broadcasting."
                   user-2-address)
      (txn:dump last-transaction)

      (cosi-simgen:gossip-neighborcast nil :new-transaction-new :trn last-transaction)
      (emotiq/elections:fire-election)
      (assert (txn::wait-for-tx-count 3 :timeout 120.0))
          
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
      (blockchain:dump :blockchain t)
      (format t "~2%Good-bye and good luck!~%")
      (emotiq/tracker:query-current-state))))
