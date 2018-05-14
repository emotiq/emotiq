
;; package :COSI was only used for initial cut (Brook's stage 1)
;; supplanted by :COSI-SIMGEN
;; Does this mean we should eliminate this package definition? --MTE
(defpackage :cosi
  (:use :common-lisp :crypto/modular-arith)
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
		:ed-random-pair)
  (:import-from :ecc-crypto-b571
                :convert-bytes-to-int
		:convert-int-to-nbytesv)
  (:export
   :schnorr-signature
   :verify-schnorr-signature))

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
   :ed-nth-pt
   :ed-random-generator
   :ed-random-pair)
  (:import-from :ecc-crypto-b571
   :convert-int-to-nbytesv
   :convert-bytes-to-int)
  (:export
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
   :cosi
   :crypto/modular-arith
   :vec-repr
   :edec
   :pbc)
  (:shadow block)            ; used internally, not required for users
  (:import-from :ecc-crypto-b571
   :random-between)
  (:export
   :txin
   :make-txin
   :make-uncloaked-txin
   :gam-txin
   :txin-hashlock
   :txin-pkey
   :txin-sig
   :txin-prf
   :txin-amt
   :get-txin-amount
   
   :txout
   :make-txout
   :txout-hashlock
   :txout-hashpkey
   :txout-prf
   :txout-encr
   
   :transaction
   :trans-txins
   :trans-txouts
   :make-transaction
   :validate-transaction

   :find-txout-for-pkey-hash
   :decrypt-txout-info
   :txout-secrets
   :txout-secr-pkey
   :txout-secr-amt
   :txout-secr-gamma
   )
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
   :hash-block 
   :update-block-signature
   :serialize-block-octets
   :compute-merkle-root-hash

   :block-transactions
   :block-witnesses
   :block-signature-bitmap
   :block-signature-pkey
   :block-signature
   :block-merkle-root-hash))

;; from cosi-construction
(defpackage :cosi-simgen
  (:use
   :common-lisp
   :cosi
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
   :recv
   :become
   :par
   :do-nothing
   :make-actor
   :set-executive-pool
   :with-borrowed-mailbox
   :pr)
  (:export
   :generate-tree
   :reconstruct-tree
   :forwarding))

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

(defpackage :cosi-test
  (:use :cl))


