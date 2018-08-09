
;; package :COSI was only used for initial cut (Brook's stage 1)
;; supplanted by :COSI-SIMGEN
;; Does this mean we should eliminate this package definition? --MTE
;; (defpackage :cosi
;;   (:use :common-lisp :crypto/modular-arith)
;;   (:import-from :edwards-ecc
;; 		:ed-add 
;; 		:ed-sub 
;; 		:ed-mul 
;; 		:ed-div 
;; 		:ed-affine
;; 		:ed-nth-pt
;; 		:*ed-r*
;; 		:*ed-q*
;;                 :ed-neutral-point
;;                 :ed-pt=
;; 		:with-ed-curve
;; 		:ed-compress-pt
;; 		:ed-decompress-pt
;; 		:ed-validate-point
;; 		:ed-random-pair)
;;   (:import-from :ecc-crypto-b571
;;                 :convert-bytes-to-int
;; 		:convert-int-to-nbytesv)
;;   (:export
;;    :schnorr-signature
;;    :verify-schnorr-signature))

(defpackage :range-proofs
  (:use :common-lisp
   :crypto/modular-arith
   :vec-repr
   :hash)
  (:import-from :edwards-ecc
   :ed-mul
   :ed-add
   :ed-sub
   :ed-div
   :ed-negate
   :ed-affine
   :ed-projective
   :*edcurve*
   :*ed-r*
   :*ed-q*
   :*ed-gen*
   :ed-curve-name
   :random-between
   :ed-neutral-point
   :ed-pt=
   :ed-compress-pt
   :ed-decompress-pt
   :with-ed-curve
   :ed-pt-from-hash
   :ed-random-generator
   :ed-random-pair
   :hash-to-pt-range)
  (:import-from :ecc-crypto-b571
   :convert-int-to-nbytesv
   :convert-bytes-to-int)
  (:export
   :*max-bit-length*
   :gpt
   :hpt
   :range-proof-block
   :range-proof-block-sum-gamma
   :range-proof-block-sum-cmt
   :range-proof-block-proofs
   :range-proof
   :make-range-proofs
   :make-range-proof
   :validate-range-proofs
   :validate-range-proof
   :simple-commit
   :proof-simple-commitment))


(defpackage :crypto-purchase
  (:use :common-lisp :crypto/modular-arith)
  (:import-from :edwards-ecc
   :ed-mul
   :ed-add
   :ed-sub
   :ed-div
   :ed-negate
   :ed-affine
   :*edcurve*
   :*ed-r*
   :*ed-q*
   :*ed-gen*
   :ed-curve-name
   :random-between
   :ed-pt=
   :ed-compress-pt
   :ed-decompress-pt
   :with-ed-curve
   :ed-nth-pt
   :ed-random-pair)
  (:import-from :ecc-crypto-b571
                :convert-bytes-to-int
                :convert-int-to-nbytesv)
  (:export))

(defpackage :cosi/proofs
  (:use
   :common-lisp
   ;;:cosi
   :crypto/modular-arith
   :vec-repr
   :edec
   :pbc)
  (:shadow block)            ; used internally, not required for users
  (:import-from :ecc-crypto-b571
   :random-between)
  (:export
   :txin
   :cloaked-txin
   :uncloaked-txin
   :make-cloaked-txin
   :make-uncloaked-txin
   :txin-cloaked-p
   :txin-hashlock
   :txin-pkey
   :txin-sig
   :txin-prf
   :uncloaked-txin-amt
   :cloaked-txin-encr
   :get-txin-amount
   
   :txout
   :cloaked-txout
   :uncloaked-txout
   :stake-txout
   :make-cloaked-txout
   :make-uncloaked-txout
   :make-stake-txout
   :txout-cloaked-p
   :txout-hashlock
   :txout-hashpkey
   :txout-prf
   :cloaked-txout-encr
   :uncloaked-txout-amt
   :uncloaked-txout-gamma
   
   :transaction
   :trans-txins
   :trans-txouts
   :trans-fee
   :trans-gamadj
   :trans-signature
   :make-transaction
   :validate-transaction
   :validate-txout

   :find-txout-for-pkey-hash
   :decrypt-txout-info
   :txout-secrets
   :txout-secr-pkey
   :txout-secr-amt
   :txout-secr-gamma
   )
  (:export
   public-key-to-address)
  (:export
   :eblock
   :protocol-version
   :epoch
   :prev-block-hash
   :merkle-root-hash
   :timestamp
   :transactions
   :signature-bitmap
   :witnesses
   :signature-pkey
   :signature
   
   :create-block
   :create-genesis-block
   :hash-block 
   :update-block-signature
   :serialize-block-octets
   :serialize-block-header-octets
   :compute-merkle-root-hash
   :compute-input-script-merkle-root-hash
   :compute-witness-merkle-root-hash

   :block-epoch
   :block-prev-block-hash
   :block-timestamp
   :block-leader-pkey
   :block-election-proof
   :block-transactions
   :block-witnesses
   :block-signature-bitmap
   :block-signature-pkey
   :block-signature
   :block-merkle-root-hash
   :block-input-script-merkle-root-hash
   :block-witness-merkle-root-hash
   :block-witnesses-and-stakes

   :*newtx-p*))                         ; for new transactions, short term temp! -mhd, 6/12/18

(defpackage :cosi/proofs/newtx          ; New Transactions
  (:use 
   :common-lisp
   ;;:cosi
   :crypto/modular-arith
   :vec-repr
   :edec
   :pbc)
  (:shadow block)            ; used internally, not required for users
  (:import-from :actors
   pr)
  (:export 
   transaction-id
   make-genesis-transaction
   transaction-outputs
   transaction-inputs
   make-transaction
   sign-transaction
   make-and-maybe-sign-transaction
   make-transaction-outputs
   make-transaction-inputs
   initial-total-coin-amount
   in-legal-money-range-p
   in-legal-stake-range-p
   validate-transaction
   get-transactions-for-new-block
   check-block-transactions
   dump-tx
   dump-txs
   clear-transactions-in-block-from-mempool
   txid-string
   wait-for-tx-count
   hash-transaction-id
   hash-transaction-input-scripts-id
   hash-transaction-witness-id

   utxo-p
   get-utxos-per-account
   get-balance

   to-txid
   find-tx))

;; from cosi-construction
(defpackage :cosi-simgen
  (:use
   :common-lisp
   ;;:cosi
   :crypto/modular-arith
   :vec-repr
   :hash
   :cosi/proofs)
  (:import-from :edwards-ecc
   :ed-add 
   :ed-sub 
   :ed-mul 
   :ed-div 
   :ed-affine
   :ed-nth-pt
   :*ed-r*
   :*ed-q*
   :ed-neutral-point
   :ed-pt=
   :with-ed-curve
   :ed-compress-pt
   :ed-decompress-pt
   :ed-validate-point
   :ed-random-pair
   :make-ecc-pt)
  (:import-from :ecc-crypto-b571
   :random-between
   :convert-int-to-nbytesv
   :convert-bytes-to-int)
  (:import-from :actors
   :=bind
   :=values
   :=defun
   :=lambda
   :=funcall
   :=apply
   :pmapcar
   :smapc
   :spawn
   :current-actor
   :send
   :recv
   :retry-recv
   :become
   :par
   :do-nothing
   :make-actor
   :set-executive-pool
   :with-borrowed-mailbox
   :pr)
  (:export
   :*current-node*
   :current-node
   :gossip-neighborcast
   :node
   :node-pkey
   :node-skey
   :node-stake
   :node-self
   :node-blockchain
   :node-blockchain-tbl
   :node-mempool
   :node-utxo-table
   :node-current-leader
   :*my-node*
   :*leader*
   :*blockchain*
   :*blockchain-tbl*
   :*mempool*
   :*utxo-table*
   :*rh-state*
   :send
   :reply
   :node-dispatcher
   :*cosi-prepare-timeout*
   :*cosi-commit-timeout*
   :leader-exec
   :init-sim
   :reset-nodes
   :forwarding
   :startup-elections
   :short-id
   :node-id-str
   :set-nodes
   :get-witness-list
   :with-current-node
   :block-list
   :latest-block
   :with-block-list
   :kill-node
   :enable-node
   :start-backfill
   ))

(defpackage :cosi-keying
  (:use
   :common-lisp
   :ecc-crypto-b571
   :crypto/modular-arith)
  (:export
   :need-integer-form
   :published-form
   :cosi-sign
   :cosi-validate
   :cosi-validate-signed-message
   :make-random-keypair
   :make-deterministic-keypair
   :make-public-subkey
   :make-secret-subkey
   :validate-pkey

   :convert-int-to-wordlist
   :convert-wordlist-to-int
   
   :add-key-to-blockchain
   :populate-pkey-database
   :refresh-pkey-database
   ))

(defpackage :cosi-blkdef
  (:use
   :common-lisp)
  (:import-from :ecc-crypto-b571
   :sha3/256-buffers)
  (:import-from :edwards-ecc
   :ed-convert-int-to-lev)
  (:export
   :add-key-to-block
   :add-transaction-to-block
   :get-block-keys
   :get-block-transactions
   :publish-block
   :goback
   ))



