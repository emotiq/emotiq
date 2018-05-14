;; top-level glue code for a simulation node
 
(in-package :emotiq/sim)

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

(defun send-genesis-utxo (&key (monetary-supply 1000) (cloaked t))
  (when *genesis-output*
    (error "Can't create more than one genesis UTXO."))
  (let ((pkey  (pbc:keying-triple-pkey *genesis-account*)))
    (print "Construct Genesis transaction")
    (multiple-value-bind (utxog secrg)
        (cosi/proofs:make-txout monetary-supply (if cloaked pkey nil))
      (declare (ignore secrg))

      (setf *genesis-output* utxog)
      (broadcast-message :genesis-utxo utxog))))

(defun broadcast-message (message arg)
  (loop
     :for node :across cosi-simgen::*node-bit-tbl*
     :doing (cosi-simgen::send (cosi-simgen::node-self node)
                               message arg)))
  
(defun spend-from-genesis (receiver amount &key (cloaked t))
  (unless *genesis-output*
    (error "No genesis account to spend from."))
  (spend *genesis-account* *genesis-output*
         (pbc:keying-triple-pkey receiver)
         amount
         :cloaked cloaked))

(defun spend (from from-utxo to-pubkey amount &key (cloaked t))
  "from = from-account(key-pair), from-utxo = specific utxo to be spent here, to-pubkey = public key of receiver
    amount = number, bit-tbl = node's bit table"
  (with-accessors ((from-public-key pbc:keying-triple-pkey)
                   (from-private-key pbc:keying-triple-skey))
      from
    (let ((decrypted-from-utxo
           (cosi/proofs:decrypt-txout-info from-utxo from-private-key)))
      ;; decrypt "from" txout, then make a txin for current txn
      (multiple-value-bind (txin txin-gamma)  
            (cosi/proofs:make-txin (cosi/proofs:txout-secr-amt decrypted-from-utxo) 
                                   (cosi/proofs:txout-secr-gamma decrypted-from-utxo)
                                   from-public-key from-private-key)
          (multiple-value-bind (new-utxo new-utxo-secrets) ;; sends
              (cosi/proofs:make-txout amount (if cloaked to-pubkey nil))
            (let ((transaction (cosi/proofs:make-transaction (list txin)
                                                             (list txin-gamma)
                                                             (list new-utxo)
                                                             (list new-utxo-secrets))))
                (let ((valid (cosi/proofs::validate-transaction transaction)))
                  (ac:pr (format nil "~%in SPEND-CLOAKED valid txn ~A~%" valid)))
              (broadcast-message :new-transaction transaction)
              transaction))))))

(defun spend-list (from from-utxo to-pubkey-list amount-list &key (cloaked t))
  " like spend, but allows multiple receivers, returns one transaction with multiple txouts"
  (with-accessors ((from-public-key pbc:keying-triple-pkey)
                   (from-private-key pbc:keying-triple-skey))
      from
    (let ((decrypted-from-utxo
           (cosi/proofs:decrypt-txout-info from-utxo from-private-key)))
      (multiple-value-bind (txin txin-gamma)  
          (cosi/proofs:make-txin (cosi/proofs:txout-secr-amt decrypted-from-utxo) 
                                 (cosi/proofs:txout-secr-gamma decrypted-from-utxo)
                                 from-public-key from-private-key)
        (let ((new-utxo-list nil)
              (new-utxo-secrets-list nil))
          (mapc #'(lambda (to-pubkey amount)
                    (multiple-value-bind (new-utxo new-utxo-secrets)
                        (cosi/proofs:make-txout amount (if cloaked to-pubkey nil))
                      (push new-utxo new-utxo-list)
                      (push new-utxo-secrets new-utxo-secrets-list)))
                to-pubkey-list
                amount-list)
          (let ((transaction (cosi/proofs:make-transaction (list txin)
                                                           (list txin-gamma)
                                                           new-utxo-list
                                                           new-utxo-secrets-list)))
            (broadcast-message :new-transaction transaction)
            transaction))))))

(defun force-epoch-end ()
  (cosi-simgen::send cosi-simgen::*leader* :make-block))

;;; tests of uncloaked ONLY

(defun send-uncloaked-genesis-utxo (&key (monetary-supply 1000))
  (when *genesis-output*
    (error "Can't create more than one genesis UTXO."))
  (print "Construct Genesis transaction")
  (multiple-value-bind (utxog secrg)
      (cosi/proofs:make-txout monetary-supply nil)  ;; pkey=nil -> uncloaked
    (declare (ignore secrg))
    
    (setf *genesis-output* utxog)
    (broadcast-message :genesis-utxo utxog)))

(defun spend-uncloaked-from-genesis (receiver)
  (unless *genesis-output*
    (error "No genesis account to spend from."))
  (spend-uncloaked *genesis-account* *genesis-output*
         (pbc:keying-triple-pkey receiver)  ;; pkey of receiver
         (pbc:keying-triple-skey receiver)))

(defun spend-uncloaked (from-account from-utxo to-public-key to-private-key)
  "from = from-account(key-pair), from-utxo = specific utxo to be spent here, to-pubkey = public key of receiver
    amount = number, bit-tbl = node's bit table"
  (let ((amount (cosi/proofs::uncloaked-txout-amt from-utxo)))
    (with-accessors ((from-private-key pbc:keying-triple-skey)
                     (from-public-key pbc:keying-triple-pkey))
        from-account
      (multiple-value-bind (uncloaked-txin uncloaked-txin-gamma-dont-care)  
          (cosi/proofs:make-uncloaked-txin amount from-public-key from-private-key)
        (declare (ignore uncloaked-txin-gamma-dont-care))
        (multiple-value-bind (uncloaked-txin2 uncloaked-txin-gamma-dont-care2)  
            (cosi/proofs:make-uncloaked-txin amount to-public-key to-private-key)
          (declare (ignore uncloaked-txin-gamma-dont-care2))
          (multiple-value-bind (uncloaked-txin3 uncloaked-txin-gamma-dont-care3)  
              (cosi/proofs:make-uncloaked-txin amount from-public-key to-private-key)
            (declare (ignore uncloaked-txin-gamma-dont-care3))
            (multiple-value-bind (new-uncloaked-utxo new-uncloaked-secrets)
                (cosi/proofs:make-txout amount nil) ;; pkey=nil -> uncloaked
              (let ((transaction (cosi/proofs:make-transaction (list uncloaked-txin)
                                                               (list (cosi/proofs:gam-txin uncloaked-txin)) ;; 0
                                                               (list new-uncloaked-utxo)
                                                               (list new-uncloaked-secrets)))
                    (transaction2 (cosi/proofs:make-transaction (list uncloaked-txin2)
                                                                (list (cosi/proofs:gam-txin uncloaked-txin)) ;; 0
                                                                (list new-uncloaked-utxo)
                                                                (list new-uncloaked-secrets)))
                    (transaction3 (cosi/proofs:make-transaction (list uncloaked-txin3)
                                                                (list (cosi/proofs:gam-txin uncloaked-txin)) ;; 0
                                                                (list new-uncloaked-utxo)
                                                                (list new-uncloaked-secrets))))
                (let ((valid (cosi/proofs::validate-transaction transaction)))
                  (ac:pr (format nil "~%in SPEND-UNCLOAKED valid txn ~A~%" valid)))
                (let ((valid (cosi/proofs::validate-transaction transaction2)))
                  (ac:pr (format nil "~%in SPEND-UNCLOAKED valid txn2 ~A~%" valid)))
                (let ((valid (cosi/proofs::validate-transaction transaction3)))
                  (ac:pr (format nil "~%in SPEND-UNCLOAKED valid txn2 ~A~%" valid)))
                (broadcast-message :new-transaction transaction)
                transaction))))))))

(defun spend-uncloaked-list (from-account from-utxo amount-list)
  "like spend-uncloaked, but allows multiple receivers, returns one transaction with multiple txouts"
  (let ((amount (cosi/proofs::uncloaked-txout-amt from-utxo))
        (in-amount (reduce #'+ amount-list)))
    (when (< amount in-amount)
      (error (format nil "trying to spend more than input, ins=%a outs=%a" amount in-amount)))
    (with-accessors ((from-private-key pbc:keying-triple-skey)
                     (from-public-key pbc:keying-triple-pkey))
        from-account
        (let ((new-uncloaked-utxo-list nil)            ;; we will create a list of utxos in the mapc below
              (new-uncloaked-utxo-secrets-list nil))
          (multiple-value-bind (uncloaked-txin uncloaked-txin-gamma-dont-care)  
              (cosi/proofs:make-uncloaked-txin amount from-public-key from-private-key)
            (declare (ignore uncloaked-txin-gamma-dont-care))
            (mapc #'(lambda (amount-out)
                      ;; we would expect only one txin, but the txin is used
                      ;; multiple times, forming one hashlock for each "to"
                      (multiple-value-bind (new-uncloaked-utxo new-uncloaked-utxo-secrets)
                          (cosi/proofs:make-txout amount-out nil) ;; nil -> uncloaked
                        (push new-uncloaked-utxo new-uncloaked-utxo-list)
                        (push new-uncloaked-utxo-secrets new-uncloaked-utxo-secrets-list)))
                  amount-list)
          (let ((transaction (cosi/proofs:make-transaction (list uncloaked-txin)
                                                           (list (cosi/proofs:gam-txin uncloaked-txin)) ;; 0
                                                           new-uncloaked-utxo-list
                                                           new-uncloaked-utxo-secrets-list)))
            (broadcast-message :new-transaction transaction)
            transaction))))))

(defparameter *user-1* (pbc:make-key-pair :user-1))
(defparameter *user-2* (pbc:make-key-pair :user-2))
(defparameter *user-3* (pbc:make-key-pair :user-3))
(defparameter *tx-1* nil)
(defparameter *tx-2* nil)
(defparameter *tx-3* nil)

(defun run (&key (amount 1000))
  "Run the block chain simulation entirely within the current process

This will spawn an actor which will asynchronously do the following:

  1.  Create a genesis transaction with AMOUNT coins.  Transact AMOUNT
      coins to *USER-1*.  The resulting transaction can be referenced
      via *tx-1*.

  2.  Transfer the AMOUNT of coins from *user-1* to *user-2* as *tx2*.

  3.  Transfer (- amount (floor (/ amount 2))) coins from *user-2* to *user-3* as *tx3*
"

  (setf *genesis-output* nil
        *tx-1*           nil
        *tx-2*           nil
        *tx-3*           nil)
  (cosi-simgen::reset-nodes) 
  (ac:spawn
   (lambda ()
       (send-genesis-utxo :monetary-supply amount)
       (let ((txn-genesis (spend-from-genesis *user-1* amount)))
         (let ((txout2 (cosi/proofs::trans-txouts txn-genesis)))
           (assert (= 1 (length txout2)))
           (let ((txn2 (spend *user-1*
                              (first txout2)
                              (pbc:keying-triple-pkey *user-2*)
                              amount)))
             (let ((txout2 (cosi/proofs::trans-txouts txn2))
                   (change (floor (/ amount 2))))
               (let ((txn3 (spend-list *user-2*
                                       (first txout2)
                                       (list (pbc:keying-triple-pkey *user-3*)
                                             (pbc:keying-triple-pkey *user-2*))
                                       (list (- amount change) change))))
                 (force-epoch-end)
                 (setf *tx-1* txn-genesis
                       *tx-2* txn2
                       *tx-3* txn3)))))))))

(defun urun (&key (amount 1000))
  "Run the block chain simulation entirely within the current process
Using only uncloaked transactions

This will spawn an actor which will asynchronously do the following:

  1.  Create a genesis transaction with AMOUNT coins.  Transact AMOUNT
      coins to *USER-1*.  The resulting transaction can be referenced
      via *tx-1*.

  2.  Transfer the AMOUNT of coins from *user-1* to *user-2* as *tx2*.

  3.  Transfer (- amount (floor (/ amount 2))) coins from *user-2* to *user-3* as *tx3*
"

  (setf *genesis-output* nil
        *tx-1*           nil
        *tx-2*           nil
        *tx-3*           nil)
  (cosi-simgen::reset-nodes) 
  (ac:spawn
   (lambda ()
       (send-uncloaked-genesis-utxo :monetary-supply amount)
       (let ((txn-genesis (spend-uncloaked-from-genesis *user-1*)))
         (force-epoch-end)
         (setf *tx-1* txn-genesis)
         (let ((txout-from-genesis-list (cosi/proofs::trans-txouts txn-genesis)))
           (assert (= 1 (length txout-from-genesis-list)))
           (let ((txn2 (spend-uncloaked *user-1*
                                        (first txout-from-genesis-list)       ;; input to txn2 (txout from genesis txn1)
                                        (pbc:keying-triple-pkey *user-2*)
                                        (pbc:keying-triple-skey *user-2*))))
             (let ((txout2 (cosi/proofs::trans-txouts txn2))
                   (change (floor (/ amount 2))))
               #+nil               (let ((txn3 (spend-uncloaked-list *user-2*
                                                                     (first txout2)
;                                                 (list (pbc:keying-triple-pkey *user-3*)
;                                                       (pbc:keying-triple-pkey *user-2*))
                                                                     (list (- amount change) change))))
                                     (force-epoch-end)
                                     (setf *tx-1* txn-genesis
                                           *tx-2* txn2
                                           *tx-3* txn3)))))))))
       

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


#|
I spent a lot of time going over the code in transactions.lisp. It should now be much cleaner, and work properly for you. Code in dbm-0430a branch

DM
David McClain
10m
I invented a third TXOUT class = STAKE-TXOUT which sends the amount to a well-known public key. These UTXOS should only be permitted to be spent by current Leader, or Leader from prior epoch.

DM
David McClain
8m
Uncloaked, as well as cloaked, TXIN now require a gamma argument in second position. That is needed to compute a Pedersen commitment on the amount and should match the gamma advertised in the UTXO.

If gamma is from cloaked TXOUT, then you have to decrypt the secret information in the TXOUT to obtain gamma. If from uncloaked TXOUT, then gamma is available as one of the readers for the class instance object.

DM
David McClain
6m
The reason that we must carry gamma forward is that the resulting Pedersen commitment is folded into the computed hashlock (aka UTXO ID). You have to have a hashlock value that matches one of the UTXO's sitting in the UTXO database/

PT
Paul Tarvydas
2m
I'm currently trying to put together a test case where everything is uncloaked (like BitCoin). (I will look at the new transactions.lisp soon). That brings up and interesting question - should the Genesis UTXO be cloaked or uncloaked? (The answer was easy when everything was cloaked).

DM
David McClain
1m
and the reason we need any gamma at all is to avoid hash collisions with other like-valued UTXOs to the same public key.

Validation of both cloaked and uncloaked TXIN now checks for valid signature from public key holder.

In order to preserve confidentiality for any recipients of TXOUT that desire cloaking, you must provide at least one cloaked TXIN, even if a zero value. An error is triggered if you try to produce a transaction with some cloaked TXOUT and no cloaked TXIN.

At any rate, trying to get uncloaked transactions to work with the old code is pointless. It won't work. The new code cleans up and uniformizes the transactions for both cloaked and uncloaked TXIN/TXOUT.

DM
David McClain
18m
external changes to the API should be minimal, except for that one case of an uncloaked TXIN needing a gamma factor. Its API is now the same as for cloaked TXIN.

DM
David McClain
15m
The code in dbm-0430a was merged with dev branch, as of 20 minutes ago. I notice that Mark's concept of block is now in place and seems to work for TST-BLK. I haven't examined the block to see if all necessary information is present. I will do.

(I mean to say that dev as of 20 minutes ago was merged into dbm-0430a. I haven't touched dev at all.)

DM
David McClain
12m
2
Genesis cloaked / uncloaked seems to me to be a policy decision. I have no answer to that question.


|#
