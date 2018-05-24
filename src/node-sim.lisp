;; use (run) to run all-cloaked transactions
;; use (urun) to run all-uncloaked transactions
;; pretty cheesy at the moment -  cloaked was working, copy/paste to make uncloaked

;; top-level glue code for a simulation node
 
(in-package :emotiq/sim)

;; remove for production
(defun eassert (bool-condition)
  (assert bool-condition))

(defun initialize (&key
                     (cosi-prepare-timeout 10)
                     (cosi-commit-timeout 10)
                     (executive-threads nil)
                     (nodes 8)
                     (new-configuration-p nil)
                     (run-cli-p nil))
  "Initialize a new local simulation of the Emotiq chain

The simulation can be configured to run across the number of EXECUTIVE-THREADS 

COSI-PREPARE-TIMEOUT specifies how many seconds that cosi leaders wait for responses during prepare phase. 
COSI-COMMIT-TIMEOUT specifies how many seconds that cosi leaders wait for responses during commit phase. 

Prepare can possibly take longer than commit, because the block may contain txns that a witness has not
yet seen (and, therefore, needs to validate the unseen txn(s))

If either NEW-CONFIGURATION-P is true or the simulator has never been
run on this node, a new simulation network will be generated.  The
configured simulation will have the integer number of NODES as
witnesses."
  (setf actors::*maximum-age* 120)
  (when executive-threads
    (setf actors::*nbr-execs*
          executive-threads))
  (when (or new-configuration-p
              (not (and (probe-file cosi-simgen:*default-data-file*)
                        (probe-file cosi-simgen:*default-key-file*))))
    (cosi-simgen:generate-tree :nodes nodes))
  (setf cosi-simgen:*cosi-prepare-timeout* cosi-prepare-timeout)
  (setf cosi-simgen:*cosi-commit-timeout* cosi-commit-timeout)
  (cosi-simgen:init-sim)

  ;; should this following code be executed every time or only when a new configuration is created?
  (let ((node-list (list-of-nodes)))
    (assert node-list)
    (assign-phony-stake-to-nodes node-list)
    (emotiq/elections:set-nodes (sort-nodes-by-stake node-list)))

  (when run-cli-p
    (emotiq/cli:main)))

(defvar *genesis-account*
  (pbc:make-key-pair :genesis)
  "Genesis account.")

(defvar *genesis-output*
  nil
  "Genesis UTXO.")

(defun broadcast-message (&rest message)
  (loop
     :for node :across cosi-simgen:*node-bit-tbl*
     :doing (apply 'cosi-simgen:send node message)))

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
    (broadcast-message :genesis-utxo
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
  (print "Validate transaction")
  (unless (cosi/proofs:validate-transaction trans)
    (error(format nil "transaction ~A did not validate" name)))
  (broadcast-message :new-transaction
                     :trn trans)
  ;; (force-epoch-end)
  )

(defun force-epoch-end ()
  (ac:pr "force-epoch-end")
  (cosi-simgen:send cosi-simgen:*leader* :make-block))


(defparameter *user-1* (pbc:make-key-pair :user-1))
(defparameter *user-2* (pbc:make-key-pair :user-2))
(defparameter *user-3* (pbc:make-key-pair :user-3))
(defparameter *tx-1* nil)
(defparameter *tx-1-hash* nil)
(defparameter *tx-2* nil)
(defparameter *tx-3* nil)

; test helper - in real life, we would already know the pkey of the destination,
; here we have special variables holding the various test users
(defun pkey-of (user)
  (pbc:keying-triple-pkey user))

(defun skey-of (user)
  (pbc:keying-triple-skey user))

(defun run (&key (amount 100) (monetary-supply 1000) (cloaked t))
  "Run the block chain simulation entirely within the current process

This will spawn an actor which will asynchronously do the following:

  1.  Create a genesis transaction with AMOUNT coins.  Transact AMOUNT
      coins to *USER-1*.  The resulting transaction can be referenced
      via *tx-1*.

  2.  Transfer the AMOUNT of coins from *user-1* to *user-2* as *tx2*.

  3.  Transfer (- amount (floor (/ amount 2))) coins from *user-2* to *user-3* as *tx3*
"

  (declare (ignore amount))

  (setf *genesis-output* nil
        *tx-1*           nil
        *tx-1-hash*      nil
        *tx-2*           nil
        *tx-3*           nil)


  (cosi-simgen:reset-nodes)

  (if cloaked
      (progn
        (setf cosi-simgen::*cosi-prepare-timeout* 40
              ;; emotiq/elections::*beacon-interval* 60
              ))
    (progn
        (setf cosi-simgen::*cosi-prepare-timeout* 10
              emotiq/elections::*beacon-interval* 20)))
      
  (emotiq/elections:make-election-beacon)
                                         
  ;(ac:spawn  ;; cosi-handlers assumes that there is one block in the blockchain, if you spawn here,
  ; you might get errors in cosi-handlers if leader-exec runs before this code...
  ; (lambda ()
  (let ((fee 10))
    (let* ( ;(genesis-pkey  (pbc:keying-triple-pkey *genesis-account*))
           (user-1-pkey (pbc:keying-triple-pkey *user-1*))
           (user-2-pkey (pbc:keying-triple-pkey *user-2*))
           (user-3-pkey (pbc:keying-triple-pkey *user-3*)))
      
      (ac:pr "Construct Genesis transaction")
      (let ((genesis-utxo (send-genesis-utxo :monetary-supply monetary-supply :cloaked cloaked)))
        ;; secrg (see tst-blk) is ignored and not even returned
        (let ((trans (create-transaction *genesis-account* genesis-utxo
                                            ; user1 gets 1000 from genesis (fee = 0)
                                         '(1000) (list user-1-pkey) 0 :cloaked cloaked)))
          (setf *tx-1* trans)
          (setf *tx-1-hash* (cosi/proofs::trans-idhash trans))
          (checktr1)
          (publish-transaction trans "tx-1")  ;; force genesis block (leader-exec breaks if blockchain is nil)
          (checktr1)
          (ac:pr "Find UTX for user-1")
          (let* ((from-utxo (cosi/proofs:find-txout-for-pkey-hash (hash:hash/256 user-1-pkey) trans)))
            (ac:pr "Construct 2nd transaction")
            (let ((trans (create-transaction *user-1* from-utxo 
                                                ; user1 spends 500 to user2, 490 to user3, 10 for fee
                                             '(500 490) (list user-2-pkey user-3-pkey) fee :cloaked cloaked)))
              ;; allow leader elections to create this block
              (publish-transaction (setf *tx-2* trans) "tx-2")
              )))))))

(defun checktr1 ()
  (let ((tx1h (cosi/proofs::trans-idhash emotiq/sim:*tx-1*)))
    (unless (vec-repr:int= tx1h emotiq/sim::*tx-1-hash*)
      (error "tx-1 has changed")))
  t)

(defun blocks ()
  "Return the blocks in the chain currently under local simulation."
  (cosi-simgen:node-blockchain cosi-simgen:*top-node*))

;; hacked copy of cosi-simgen::assign-bits()
(defun list-of-nodes ()
  (let ((collected
         (um:accum acc
           (maphash (lambda (k node)
                      (declare (ignore k))
                      (acc node))
                    cosi-simgen:*ip-node-tbl*))))
    collected))

(defun assign-phony-stake-to-nodes (node-list)
  ;; set the node-stake slot of every node to a random number <= 100,000
  (dolist (node node-list)
        (let ((phony-stake (random 100000)))
          (setf (cosi-simgen:node-stake node) phony-stake))))

(defun sort-nodes-by-stake (node-list)
    (sort node-list '< :key #'cosi-simgen:node-stake))


(defun prdebug ()
  (let ((blks (emotiq/sim:blocks)))
    (assert (>= (length blks) 1))
    (let ((txs (cosi/proofs::block-transactions (first blks))))
      (assert (>= (length txs) 1))
      (let ((firsttx (first txs)))
        (format t "blocks: ~A~%" blks)
        (format t "first txn: ~A~%" firsttx)
        (format t "*tx-1*:    ~A~%" emotiq/sim:*tx-1*)
        (format t "tx1 eq emotiq/sim:*tx-1* ~A~%" 
                (vec-repr:int= (hash:hash/256 firsttx) (hash:hash/256 emotiq/sim:*tx-1*)))
        (let ((h1 (cosi/proofs::trans-idhash firsttx))
              (h2 (cosi/proofs::trans-idhash *tx-1*)))
          (format t "firsttx.idhash = ~a~%" h1)
          (format t "*tx-1*.idhash  = ~a~%" h2)
          (format t "firsttx.idhash =? *tx-1*.idhash ~A~%" (vec-repr:int= h1 h2))))))
  )

(defun fire-election ()
  (emotiq/elections::fire-election))

(defun kill-beacon ()
  (emotiq/elections::kill-beacon))