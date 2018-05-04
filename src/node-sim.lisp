;; top-level glue code for a simulation node
 
(in-package :cosi-simgen)

(defun node (&optional
             (name "Alice")
             (ipstr "127.0.0.1"))  ;; specify an IP (as string) if you want something other than 127.0.0.1

  (cosi-init ipstr)
  (cosi-generate)
  (init-sim)
  (tst-blk)

  (emotiq/cli:main)
    
  )



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
