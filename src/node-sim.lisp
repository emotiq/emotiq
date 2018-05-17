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
              (not (and (probe-file cosi-simgen::*default-data-file*)
                        (probe-file cosi-simgen::*default-key-file*))))
    (cosi-simgen::generate-tree :nodes nodes))
  (setf cosi-simgen::*cosi-prepare-timeout* cosi-prepare-timeout)
  (setf cosi-simgen::*cosi-commit-timeout* cosi-commit-timeout)
  (cosi-simgen::init-sim)
  (when run-cli-p
    (emotiq/cli:main)))

(defvar *genesis-account*
  (pbc:make-key-pair :genesis)
  "Genesis account.")

(defvar *genesis-output*
  nil
  "Genesis UTXO.")

(defun broadcast-message (message arg)
  (loop
     :for node :across cosi-simgen::*node-bit-tbl*
     :doing (cosi-simgen::send (cosi-simgen::node-self node)
                               message arg)))

(defun send-cloaked-genesis-utxo (&key (monetary-supply 1000))
  (when *genesis-output*
    (error "Can't create more than one genesis UTXO."))
  (print "Construct Genesis UTXO")
  (multiple-value-bind (utxog secrg)
      (cosi/proofs::make-cloaked-txout monetary-supply (pbc:keying-triple-pkey *genesis-account*))
    (declare (ignore secrg))
    (eassert (cosi/proofs::validate-txout utxog))
    (setf *genesis-output* utxog)
    (broadcast-message :genesis-utxo utxog)
    utxog))

(defun publish-transaction (trans)
  (print "Validate transaction")
  (eassert (cosi/proofs::validate-transaction trans)) ;; 7.6s MacBook Pro
  (broadcast-message :new-transaction trans)
  (force-epoch-end))

(defun force-epoch-end ()
  (ac:pr "force-epoch-end")
  (cosi-simgen::send cosi-simgen::*leader* :make-block))


(defparameter *user-1* (pbc:make-key-pair :user-1))
(defparameter *user-2* (pbc:make-key-pair :user-2))
(defparameter *user-3* (pbc:make-key-pair :user-3))
(defparameter *tx-1* nil)
(defparameter *tx-2* nil)
(defparameter *tx-3* nil)

(defparameter *genesis* nil)
(defparameter *trans1* nil)
(defparameter *trans2* nil)


; test helper - in real life, we would already know the pkey of the destination,
; here we have special variables holding the various test users
(defun pkey-of (user)
  (pbc:keying-triple-pkey user))

(defun skey-of (user)
  (pbc:keying-triple-skey user))

(defun create-cloaked-transaction (from-account from-utxo amount fee to-account)
  (declare (ignore amount))
  (let ((from-skey (pbc:keying-triple-skey from-account))
	(from-pkey (pbc:keying-triple-pkey from-account))
	(to-pkey (pbc:keying-triple-pkey to-account)))
    (let* ((to-info (cosi/proofs::decrypt-txout-info from-utxo from-skey))
           (secr-amt (cosi/proofs::txout-secr-amt to-info))
           (secr-gamma (cosi/proofs::txout-secr-gamma to-info))
	   (trans (cosi/proofs::make-transaction :ins `((:kind :cloaked
							       :amount ,secr-amt
							       :gamma  ,secr-gamma
							       :pkey   ,from-pkey
							       :skey   ,from-skey))
						 :outs `((:kind :cloaked
								:amount 750
								:pkey   ,to-pkey)
							 (:kind :cloaked
								:amount 240
								:pkey   ,from-pkey))
						 :fee fee)))
      trans)))
               
(defun create-cloaked-transaction-with-multiple-outs
    (from-account from-utxo amount-list to-pkey-list fee)
  (declare (ignore amount))
  (let ((from-skey (pbc:keying-triple-skey from-account))
	(from-pkey (pbc:keying-triple-pkey from-account)))
    (let* ((to-info (cosi/proofs::decrypt-txout-info from-utxo from-skey))
           (secr-amt (cosi/proofs::txout-secr-amt to-info))
           (secr-gamma (cosi/proofs::txout-secr-gamma to-info))
	   (out-list (mapcar #'(lambda (amt to-pkey)
				 `(:kind :cloaked
				   :amount ,amt
				   :pkey ,to-pkey))
			     amount-list to-pkey-list)))
      (cosi/proofs::make-transaction :ins `((:kind :cloaked
					     :amount ,secr-amt
					     :gamma  ,secr-gamma
					     :pkey   ,from-pkey
					     :skey   ,from-skey))
				     :outs out-list
				     :fee fee))))

(defun run (&key (amount 100) (monetary-supply 1000))
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
        *tx-2*           nil
        *tx-3*           nil)


  (cosi-simgen::reset-nodes) 
  (ac:spawn
   (lambda ()
     (let ((fee 10))
       (let* ((genesis-pkey  (pbc:keying-triple-pkey *genesis-account*))
              (user-1-pkey (pbc:keying-triple-pkey *user-1*)))
         
         (ac:pr "Construct Genesis transaction")
         (let ((genesis-utxo (send-cloaked-genesis-utxo :monetary-supply monetary-supply)))
           ;; secrg (see tst-blk) is ignored and not even returned
           (let ((trans (create-cloaked-transaction *genesis-account*
                                                    genesis-utxo 990 10
                                                    *user-1*)))
             (publish-transaction (setf *trans1* trans))
             (ac:pr "Find UTX for user-1")
             (let* ((from-utxo (cosi/proofs::find-txout-for-pkey-hash (hash:hash/256 user-1-pkey) trans)))
               (ac:pr "Construct 2nd transaction")
               (let ((trans (create-cloaked-transaction-with-multiple-outs
                             *user-1* from-utxo '(250 490) (list user-1-pkey genesis-pkey) fee)))
                 (publish-transaction (setf *trans2* trans))
                 ))))))
     (force-epoch-end))))

(defun blocks ()
  "Return the blocks in the chain currently under local simulation."
  (cosi-simgen::node-blockchain cosi-simgen::*top-node*))

#|
create a Genesis UTXO: (special case for initialization) AMT0

1. Make a keyset for Genesis {k, pkey, skey} 
   where ("k" a pbc:keying-triple { pubkey, secret-key, signature over :dave (arbitrary)})
2. make txout-secrets (pkey, AMNT0, GAMMA0) ->txout-secrets0
3. make encrypted-amount -> (txout-secrets0, pkey, id (":spend-amount")) -> encr0
4. make txout -> (int, pubkey) -> (AMT0, pkey) -> txout0{hashpkey, hashlock, proof, encr0} -> { UTX0, txout-secrets0 }
5. discard txout-secrets0 (special case for initialization)
6. send UTX0 to blockchain (as a genesis UTXO, not as a block)


----- scenario 0a -------
send all of AMT0 to Mary: (special case for initialization)

N.B. We must do all this in one fell-swoop, while knowing the secrets of, both, Genesis and 
     Mary.  This does not happen in the typical case.  Normally, we can only unlock UTXOs that
     are meant for a particular Node (/Wallet).  If the UTXOs are not meant for us, the best
     we can do is to simply hold onto them without knowing the amounts.

1. Make a keyset for Mary
2. Grab UTX0 (from Genesis), knowing that it is the only UTXO1 we need to consider.
3. make txout-secrets (pkeym, AMNT1, GAMMA1) ->txout-secrets1
4. make encrypted-amount -> (txout-secrets1, pkey, id (":spend-amount")) -> encr1
5. make txout -> (int, pubkey) -> (AMT0, pkey) -> txout1{hashpkey, hashlock, proof, encr1} -> { UTX1, txout-secrets1 }
5. discard txout-secrets0 (special case for initialization)
3. Convert the Genesis UTXO into a TXIN for Mary TX, use Mary's credentials to 
   decrypt UTX0 into a TXIN1
4. create a transaction from Genesis to Mary 
   txn1 -> 


N.B  A verifier builds up a mempool and a UTXO table.  The verifier can verify cloaked
     transactions, as they come in, without knowing the amounts using a "level 1" 
     (my terminology) algorithm, confirms that the txn (transaction) is mathematically 
     sound.  It then sticks such an incoming txn into the mempool.  A CoSi Leader
     runs a "level 2" algorithm to check for double spends before releasing a
     block to the prepare phase.  Every witness (a CoSi co-signer) checks the incoming
     txns for level-1 correctness and creates their own version of mempool.  A witness
     should, also, perform a level-2 check on a proposed block before signing off on it
     during :prepare (and :commit?).




send an AMT1 from Mary to Alice

1. check all UTXO's assigned to Mary, ensure she has enough to cover the AMT1
2. collect up enough UTXO's to cover AMT1
3. uncloak the chosen UTXO's
4. commit (Pederson) the amount(s) to Alice, commit (Pederson) any leftovers back to Mary
3. recloak Alice's TXIN1s -> {AMT1, GAMMA1}, recloak Mary's leftover TXIN2s -> {AMT2, GAMMA2}
4. create UTXO1 to Alice, create UTXO2 to Mary
5. create a transaction to Alice {TXIN1, TXOUT1, GAMMA-ADJ1} and
   create a transaction to Mary {TXIN2, TXOUT2, GAMMA-ADJ2}
6. Send both txns to the blockchain

|#


#|
create a Genesis UTXO: (special case for initialization) AMT0

1. Make a keyset for Genesis {k, pkey, skey} 
   where ("k" a pbc:keying-triple { pubkey, secret-key, signature over :dave (arbitrary)})


(let* ((k     (pbc:make-key-pair :dave)) ;; genesis keying
       (pkey  (pbc:keying-triple-pkey k))
       (skey  (pbc:keying-triple-skey k))
       

2. make txout-secrets (pkey, AMNT0, GAMMA0) ->txout-secrets0
3. make encrypted-amount -> (txout-secrets0, pkey, id (":spend-amount")) -> encr0
4. make txout -> (int, pubkey) -> (AMT0, pkey) -> txout0{hashpkey, hashlock, proof, encr0} -> { UTX0, txout-secrets0 }
5. discard txout-secrets0 (special case for initialization)
6. send UTX0 to blockchain (as a genesis UTXO, not as a block)


----- scenario 0a -------
send all of AMT0 to Mary: (special case for initialization)

N.B. We must do all this in one fell-swoop, while knowing the secrets of, both, Genesis and 
     Mary.  This does not happen in the typical case.  Normally, we can only unlock UTXOs that
     are meant for a particular Node (/Wallet).  If the UTXOs are not meant for us, the best
     we can do is to simply hold onto them without knowing the amounts.

1. Make a keyset for Mary

       (km    (pbc:make-key-pair :mary)) ;; Mary keying
       (pkeym (pbc:keying-triple-pkey km))
       (skeym (pbc:keying-triple-skey km)))




N.B  A verifier builds up a mempool and a UTXO table.  The verifier can verify cloaked
     transactions, as they come in, without knowing the amounts using a "level 1" 
     (my terminology) algorithm, confirms that the txn (transaction) is mathematically 
     sound.  It then sticks such an incoming txn into the mempool.  A CoSi Leader
     runs a "level 2" algorithm to check for double spends before releasing a
     block to the prepare phase.  Every witness (a CoSi co-signer) checks the incoming
     txns for level-1 correctness and creates their own version of mempool.  A witness
     should, also, perform a level-2 check on a proposed block before signing off on it
     during :prepare (and :commit?).




send an AMT1 from Mary to Alice

1. check all UTXO's assigned to Mary, ensure she has enough to cover the AMT1
2. collect up enough UTXO's to cover AMT1
3. uncloak the chosen UTXO's
4. commit (Pederson) the amount(s) to Alice, commit (Pederson) any leftovers back to Mary
3. recloak Alice's TXIN1s -> {AMT1, GAMMA1}, recloak Mary's leftover TXIN2s -> {AMT2, GAMMA2}
4. create UTXO1 to Alice, create UTXO2 to Mary
5. create a transaction to Alice {TXIN1, TXOUT1, GAMMA-ADJ1} and
   create a transaction to Mary {TXIN2, TXOUT2, GAMMA-ADJ2}
6. Send both txns to the blockchain




make-key-pair: seed -> {signature, pkey, skey}
make-txout-secrets: pubkey-of-receiver -> amt -> txout-secrets
txout-combo: { txout, txout-secrets }
make-tx-out: amt -> pubkey -> txout-combo

make-txin: hashlock -> prf -> pkey -> sig -> encr
txin-combo: { txin, gamma }

pedersen-commitment (range-proof)
pedersen-commitment (txin)
pedersen-commitment (txout)
pedersen-commitment (unlock-txout)

make-hashlock: range-proof -> public-key -> hashlock


to validate txout == validate-txout
to validate txout == validate-txin
to validate txn  == validate-transaction

to recover spend info
- decrypt-txin-info (txin skey)
- decrypt-txout-info (txout skey)

lookups
- find-txin-for-pkey-hash (pkey-hash txn)
- find-txout-for-pkey-hash (pkey-hash txn)

|#
