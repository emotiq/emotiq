;; top-level glue code for a simulation node
 
(in-package :emotiq/sim)

(defun initialize (&key (leader-node "127.0.0.1"))
  (setf actors::*maximum-age* 120)
  (cosi-simgen::cosi-init leader-node)
  ;; Weird:  can't run cosi-generate as part of the process
  #+(or)
  (cosi-simgen::cosi-generate :nodes 3)
  (cosi-simgen::init-sim)
  #+(or)
  (emotiq/cli:main))

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
      (map nil
       (lambda (node)
         (cosi-simgen::send (cosi-simgen::node-self node) :genesis-utxo utxog))
       cosi-simgen::*node-bit-tbl*))))


(defun spend-from-genesis (receiver amount &key (cloaked t))
  (unless *genesis-output*
    (error "No genesis account to spend from."))
  (spend *genesis-account* *genesis-output* (pbc:keying-triple-pkey receiver) amount cosi-simgen::*node-bit-tbl* :cloaked cloaked))

(defun spend (from from-utxo to-pubkey amount bit-tbl &key (cloaked t))
  "from = from-account(key-pair), from-utxo = specific utxo to be spent here, to-pubkey = public key of receiver
    amount = number, bit-tbl = node's bit table"
  (with-accessors ((from-public-key pbc:keying-triple-pkey)
                     (from-private-key pbc:keying-triple-skey))
      from
    (let ((from-skey (pbc:keying-triple-skey from)))
      (let ((decrypted-from-utxo (cosi/proofs:decrypt-txout-info from-utxo from-skey)))
        (multiple-value-bind (txin txin-gamma)  ;; decrypt "from" txout, then make a txin for current txn
            (cosi/proofs:make-txin (cosi/proofs:txout-secr-amt decrypted-from-utxo) ;;
                                   (cosi/proofs:txout-secr-gamma decrypted-from-utxo)
                                   from-public-key from-private-key)
          (multiple-value-bind (new-utxo new-utxo-secrets) ;; sends
              (cosi/proofs:make-txout amount (if cloaked to-pubkey nil))
            (let ((transaction (cosi/proofs:make-transaction (list txin)
                                                             (list txin-gamma)
                                                             (list new-utxo)
                                                             (list new-utxo-secrets))))
              (map nil (lambda (node)
                         (cosi-simgen::send (cosi-simgen::node-self node) :new-transaction transaction))
                   bit-tbl)
              transaction)))))))

(defun spend-list (from from-utxo to-pubkey-list amount-list bit-tbl &key (cloaked t))
  " like spend, but allows multiple receivers, returns one transaction with multiple txouts"
  (with-accessors ((from-public-key pbc:keying-triple-pkey)
                     (from-private-key pbc:keying-triple-skey))
      from
    (let ((from-skey (pbc:keying-triple-skey from)))
      (let ((decrypted-from-utxo (cosi/proofs:decrypt-txout-info from-utxo from-skey)))
        (multiple-value-bind (txin txin-gamma)  ;; decrypt "from" txout, then make a txin for current txn
            (cosi/proofs:make-txin (cosi/proofs:txout-secr-amt decrypted-from-utxo) ;;
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
              (map nil (lambda (node)
                         (cosi-simgen::send (cosi-simgen::node-self node) :new-transaction transaction))
                   bit-tbl)
              transaction)))))))

(defun force-epoch-end ()
  (cosi-simgen::send cosi-simgen::*leader* :make-block))

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


(defparameter *user1* (pbc:make-key-pair :user1))
(defparameter *user2* (pbc:make-key-pair :user2))
(defparameter *user3* (pbc:make-key-pair :user3))
(defparameter *txn1* nil)
(defparameter *txn2* nil)
(defparameter *txn3* nil)

(defun run (&key (cloaked t))
  (setf *genesis-output* nil
        *txn1*           nil
        *txn2*           nil
        *txn3*           nil)
  (cosi-simgen::reset-nodes) 
  (ac:spawn
   (lambda ()
     (let ((amount 1000))
       (send-genesis-utxo :monetary-supply amount :cloaked cloaked)
       ;; spend txn must use up all input UTXO's, this is actually, the "genesis transaction"
       (let ((txn-genesis (spend-from-genesis *user1* amount :cloaked cloaked))) 
         ;; actors run asynchronously, check value of *txn* only when all activity stops
         (setf *txn1* txn-genesis)  
         (let ((txout2 (cosi/proofs::trans-txouts txn-genesis)))
           (assert (= 1 (length txout2)))
           (let ((txn2 (spend *user1*
                              (first txout2)
                              (pbc:keying-triple-pkey *user2*)
                              amount
                              cosi-simgen::*node-bit-tbl*
                              :cloaked cloaked)))
             (let ((txout2 (cosi/proofs::trans-txouts txn2)))
               (let ((txn3 (spend-list *user2*
                                       (first txout2)
                                       (list (pbc:keying-triple-pkey *user3*)
                                             (pbc:keying-triple-pkey *user2*))
                                       ;; send 900 to user 3, 100 back as change
                                       (list (- amount 100) 100)  
                                       cosi-simgen::*node-bit-tbl*
                                       :cloaked cloaked)))
                 (force-epoch-end)
                 (setf *txn2* txn2)
                 (setf *txn3* txn3))))))))))

(defun sim (&key (cloaked t))
  (initialize)
  (run))

(defun txn1 ()
  *txn1*)

(defun txn2 ()
  *txn2*)

(defun txn3 ()
  *txn3*)

(defun blocks ()
  (cosi-simgen::node-blockchain cosi-simgen::*top-node*))

#|
todo:
- randomness
- staking
- election
x remove ac:spawn from test-network, see if it still works
x- txn that splits outputs
- elections
- insert :fee in transactions (new field)
- txn dumper
- get signing to work in sim (not always OK?) [what purpose?]
- [ignore] investigate why I saw 5 blocks for 3 epochs?  intermittent?  Or temp bug during fixing?
- uncloaked txouts
|#
