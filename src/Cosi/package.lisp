
;; package :COSI was only used for initial cut (Brook's stage 1)
;; supplanted by :COSI-SIMGEN
(defpackage :cosi
  (:use :common-lisp :crypto-mod-math)
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
		:ed-hash
		:ed-random-pair)
  (:import-from :ecc-crypto-b571
                :convert-bytes-to-int
		:convert-int-to-nbytesv)
  (:export
   :schnorr-signature
   :verify-schnorr-signature))

(defpackage :range-proofs
  (:use :common-lisp :crypto-mod-math)
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
   :convert-int-to-nbytesv
   :convert-bytes-to-int)
  (:export
   :make-range-proofs
   :validate-range-proof
   :make-range-prover
   :range-proof-block))


(defpackage :crypto-purchase
  (:use :common-lisp :crypto-mod-math)
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
   :convert-bytes-to-int
   :with-ed-curve
   :ed-nth-pt
   :ed-random-pair)
  (:import-from :ecc-crypto-b571
                :convert-bytes-to-int
                :convert-int-to-nbytesv)
  (:export))

;; from cosi-construction
(defpackage :cosi-simgen
  (:use :common-lisp :cosi :crypto-mod-math)
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
   :ed-hash
   :ed-random-pair)
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
   :spawn
   :current-actor
   :recv
   :become
   :do-nothing
   :make-actor
   :set-executive-pool
   :with-borrowed-mailbox
   :pr)
  (:export
   :generate-tree
   :reconstruct-tree
   :forwarding))

(pushnew :ELLIGATOR *features*)
(defpackage :cosi-keying
  (:use
   :common-lisp
   :ecc-crypto-b571
   :crypto-mod-math
   :edwards-ecc)
  (:export
   :need-integer-form
   :published-form
   :cosi-dsa
   :cosi-dsa-validate
   :make-random-keypair
   :make-deterministic-keypair
   :make-unique-deterministic-keypair
   :make-subkey
   :validate-pkey
   
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
   ))

