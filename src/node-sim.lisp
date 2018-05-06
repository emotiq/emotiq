;; top-level glue code for a simulation node
 
(in-package :emotiq-user)

(defun start-network (&key
                        (ipstr "127.0.0.1"))
  "IPSTR string) if you want something other than 127.0.0.1"
  (cosi-simgen::cosi-init ipstr)
  (cosi-simgen::cosi-generate)
  (cosi-simgen::init-sim)
  (cosi-simgen::reset-blockchain))

(defvar *singularity-account*
  (pbc:make-key-pair :singularity)
  "Origin of genesis transaction.")

(defvar *genesis-account*
  (pbc:make-key-pair :genesis)
  "Genesis account.")

(defvar *genesis-output*
  nil
  "Genesis UTXO.")

(defun send-genesis-utxo (&key (monetary-supply 1000))
  (when *genesis-output*
    (error "Can't create more than one genesis UTXO."))
  (let ((pkey  (pbc:keying-triple-pkey *singularity-account*)))
    (print "Construct Genesis transaction")
    (multiple-value-bind (utxog secrg)
        (cosi/proofs:make-txout monetary-supply pkey)
      (declare (ignore secrg))

      (setf *genesis-output* utxog)
      (map nil
       (lambda (node)
         (ac:send (cosi-simgen::node-self node) :genesis-utxo utxog))
       cosi-simgen::*node-bit-tbl*))))

(defun spend-from-genesis (receiver amount)
  (unless *genesis-output*
    (error "No genesis account to spend from."))
  (spend *singularity-account* *genesis-output* (pbc:keying-triple-pkey receiver) amount cosi-simgen::*node-bit-tbl*))

(defun spend (from from-utxo to-pubkey amount bit-tbl)
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
              (cosi/proofs:make-txout amount to-pubkey)
            (setf *utxo* new-utxo
                  *secrets* new-utxo-secrets)
            (let ((transaction (cosi/proofs:make-transaction (list txin)
                                                             (list txin-gamma)
                                                             (list new-utxo)
                                                             (list new-utxo-secrets))))
              (map nil (lambda (node)
                         (when (eq node cosi-simgen::*top-node*)
                           (setf emotiq::*txn* transaction)
                           (ac:pr (format nil "sending txn ~A" transaction)))
                         (ac:send (cosi-simgen::node-self node) :new-transaction transaction))
                   bit-tbl)
              transaction)))))))


(defun force-epoch-end ()
  (ac:send (cosi-simgen::node-self cosi-simgen::*top-node*) :make-block))

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


(defparameter *user* (pbc:make-key-pair :user))
(defparameter *Alice* (pbc:make-key-pair :Alice))

;; Recreate (tst-blk)
(defun test-network ()
  (setf cosi-simgen::*blocks* nil)
  (setf *genesis-output* nil)
  (ac:spawn
   (lambda ()
     (start-network)
     (send-genesis-utxo)
     (let ((txn (spend-from-genesis *user* 100)))
       (force-epoch-end))))
  #+(or)
  (emotiq/cli:main))

(defun blocks ()
  "returns *blocks* - only useful after network has settled"
  cosi-simgen::*blocks*)

