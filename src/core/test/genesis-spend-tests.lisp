(in-package :blockchain-test)

;; (defun run-1 ()
;;   (sim::initialize :nodes 3 :new-configuration-p t)
;;   (sim::ensure-simulation-keys)
;;   (setf sim::*genesis-output* nil
;;         sim::*tx-1* nil
;;         sim::*tx-2* nil)
;;   (node:reset-nodes)
;;   (ac:pr "Construct Genesis Block")
  
;;   (let* ((genesis-block
;;           (let ((node:*current-node* node:*top-node*))
;;             ;; Establish current-node binding of genesis node
;;             ;; around call to create genesis block.
;;             (block:make-genesis-block (wallet:address sim::*genesis-account*)
;;                                       (sim:keys-and-stakes))))
;;          (genesis-transaction (first (block:transactions genesis-block))))

;;     (emotiq:note "Tx 0 created/genesis, now broadcasting.")
;;     (txn:dump genesis-transaction)      

;;     (node:gossip-neighborcast nil :genesis-block :blk genesis-block)
;;     (sleep 10.0)))


(defun init (&key
               (cosi-prepare-timeout 40)
               (cosi-commit-timeout 10)
               (executive-threads nil)
               (nodes 3)
               (new-configuration-p t))
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
            (not (and (probe-file node:*default-data-file*)
                      (probe-file node:*default-key-file*))))
    (cosi-simgen:generate-tree :nodes nodes))
  (setf cosi-simgen::*cosi-prepare-timeout* cosi-prepare-timeout)
  (setf cosi-simgen::*cosi-commit-timeout* cosi-commit-timeout)

  (cosi-simgen:reconstruct-tree)

  (flet ((send-real-nodes (&rest msg)
           (dolist (ip node::*real-nodes*)
             (apply #'actors:send (node:pkey (gethash ip node:*ip->node*)) msg))))

    (send-real-nodes :reset))
  
  (cosi-simgen::set-nodes
   (um:accum acc
     (maphash (lambda (k node)
                (declare (ignore k))
                (let ((pkey (node:pkey node))
                      (stake (random 100000)))
                  (setf (node:stake node) stake)
                  (ac:pr (list pkey stake))
                  (acc (list pkey stake))))
              node:*ip->node*)))
  

  (setf cosi-simgen::*all-nodes* (cosi-simgen::get-witness-list))
  
  (emotiq/tracker:start-tracker))


(defparameter *genesis-account* nil)
(defparameter *genesis-output* nil)

(defparameter *user-1* nil)
(defparameter *user-2* nil)
(defparameter *user-3* nil)
(defparameter *tx-1* nil)
(defparameter *tx-2* nil)

(defun ensure-keys ()
  (unless (and *genesis-account* *user-1* *user-2* *user-3*)
    (setf *genesis-account* (pbc:make-key-pair :genesis)
          *user-1* (pbc:make-key-pair :user-1)
          *user-2* (pbc:make-key-pair :user-2)
          *user-3* (pbc:make-key-pair :user-3))))

(defun run ()
  "Using new tx feature, run the block chain simulation entirely within the current process.
This will spawn an actor which will asynchronously do the following:

  1.  Create a genesis block (amount of coin is parameterized in the
      code).  Transact AMOUNT coins to *USER-1*.  The resulting
      transaction can be referenced via *tx-1*.

  2.  Transfer the some amount of coins from *user-1* to *user-2* as *tx2*.

  3.  Transfer some other amount of coins from *user-2* to *user-3* as *tx3*."

  (init)
  (ensure-keys)
  (setf *genesis-output* nil
        *tx-1* nil
        *tx-2* nil)
  (cosi-simgen::reset-nodes)
  (let ((fee 10))    
    (ac:pr "Construct Genesis Block")
    (let* ((genesis-block
            (let ((node:*current-node* node:*top-node*))
              ;; Establish current-node binding of genesis node
              ;; around call to create genesis block.
              (block:make-genesis-block (wallet:address *genesis-account*)
                                        (cosi-simgen::get-witness-list))))
           (genesis-transaction (first (block:transactions genesis-block)))
           (genesis-address (wallet:address *genesis-account*))
           (user-1-address (wallet:address *user-1*))
           (user-2-address (wallet:address *user-2*))
           (user-3-address (wallet:address *user-3*))
           (last-transaction nil))

      (emotiq:note "Tx 0 created/genesis, now broadcasting | ID = ~A"
                   (txn:id genesis-transaction))
      (txn:dump genesis-transaction)      

      (cosi-simgen:gossip-neighborcast nil :genesis-block :blk genesis-block)
      
      (sleep 10.0)
      
      (setf last-transaction 
            (txn:make-spend-transaction *genesis-account*
                                        user-1-address
                                        890
                                        :fee fee))
        
      (setf *tx-1* last-transaction)
      (emotiq:note "Tx 1 [GENESIS -> USER-1] created/signed by genesis (~a), now broadcasting. | ID = ~A"
                   genesis-address (txn:id last-transaction))
      (txn:dump last-transaction)

      (cosi-simgen:gossip-neighborcast nil :new-transaction :txn last-transaction)
      
      (fire-election)
      (assert (txn::wait-for-tx-count 1 :timeout 120.0))
      
      (setf last-transaction
            (txn:make-spend-transaction *user-1*
                                        user-2-address
                                        500
                                        :fee fee))
      (setf *tx-2* last-transaction)
      (emotiq:note "Tx 2 [USER-1 -> USER-2] created/signed by user-1 (~a), now broadcasting."
                   user-1-address)
      (txn:dump last-transaction)
          
      (cosi-simgen:gossip-neighborcast nil :new-transaction :txn last-transaction)

      (fire-election)
      (assert (txn::wait-for-tx-count 2 :timeout 120.0))
          
      (setf last-transaction
            (txn:make-spend-transaction *user-2*
                                        user-3-address
                                        350
                                        :fee fee))
            
      (emotiq:note "Tx 3 [USER-2 -> USER-3] created/signed by user-2 (~a), now broadcasting."
                   user-2-address)
      (txn:dump last-transaction)
      
      (cosi-simgen:gossip-neighborcast nil :new-transaction :txn last-transaction)
      (fire-election)
      (assert (txn::wait-for-tx-count 3 :timeout 120.0))
          
      ;; here: attempt a double-spend: (with same TxID)
      (emotiq:note "Tx 4 [USER-2 -> USER-3] created/signed by user-2 (~a) [attempt to double-spend (same TxID)], now broadcasting." user-2-address)
            
      (cosi-simgen:gossip-neighborcast nil :new-transaction :txn last-transaction)
      
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
