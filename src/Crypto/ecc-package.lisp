;; ecc-package.lisp
;; -----------------------------------
#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :cl-user)

(defpackage :crypto-mod-math
  (:use :common-lisp)
  (:export
   :reset-blinders
   :expt-mod
   :sqrt-mod
   :mult-mod
   :add-mod
   :sub-mod
   :inv-mod
   :div-mod
   :quadratic-residue-p
   ))

(defpackage base58
  (:use :common-lisp)
  (:export
   :ub8        ;; type
   :ub8-vector ;; type
   :ub8v-repr  ;; mixin class
   :ub8v
   :ub8v-vec
   :lev
   :lev-vec
   :bev
   :bev-vec
   :base58
   :base58-str
   :base64
   :base64-str
   :hex
   :hex-str
   :int
   :levn
   :bevn
   ))

(defpackage :ecc-crypto-b571
  (:use :common-lisp :crypto-mod-math)
  (:nicknames :ecc)
  (:export
   :ctr-hash-prng
   :basic-random
   :basic-random-between
   :random-between
   
   :convert-int-to-nbytes
   :convert-int-to-nbytesv
   :convert-bytes-to-int
   :ctr-drbg
   :ctr-drbg-int
   :sha3-buffers
   :sha3/256-buffers
   
   :def-cached-var

   :encode-bytes-to-base64
   :decode-bytes-from-base64

   :convert-int-to-lev
   :convert-lev-to-int
   :encode-object-to-base58
   :decode-object-from-base58
   :encode-bytes-to-base58
   :decode-bytes-from-base58

   :get-cached-symbol-data
   ))

(defpackage :primes
  (:use #:common-lisp)
  (:export
   #:divides?
   #:expt-mod
   #:random-between
   #:make-prime
   #:is-prime?
   #:extended-gcd
   #:compute-modulo-inverse
   #:provably-prime?
   #:factors-of
   #:generate-strong-prime
   #:generate-rsa-base
   #:add-mod
   #:sub-mod
   #:mult-mod
   #:inv-mod
   #:div-mod
   #:expt-mod
   #:decompose
   ))

(defpackage :crypto/modular-arith
  (:use :common-lisp)
  (:import-from :ecc-crypto-b571
   :get-cached-symbol-data)
  (:export
   :with-mod
   :reset-blinders
   :m^
   :msqrt
   :m+
   :m-
   :m*
   :m/
   :minv
   :mmod
   :mchi
   :quadratic-residue-p
   :m!
   ))

(defpackage :edwards-ecc
  (:nicknames :edec)
  (:use :common-lisp
   :ecc-crypto-b571
   :crypto/modular-arith
   :base58)
  (:import-from :ecc-crypto-b571
		:convert-int-to-nbytes
		:convert-int-to-nbytesv
		:convert-bytes-to-int
                :convert-int-to-lev
                :convert-lev-to-int
		:ctr-drbg-int
		:sha3-buffers
		:random-between
                :get-cached-symbol-data)
  (:export
   :ed-curve
   :with-ed-curve
   :ed-curves
   :*ed-gen*
   :*ed-r*
   :*ed-h*
   :*ed-q*
   :*ed-name*
   :*ed-nb*
   :*ed-nbits*
   :ecc-pt
   :ed-proj-pt
   :ed-affine
   :ed-pt=
   :ed-neutral-point-p
   :ed-satisfies-curve
   :ed-add
   :ed-negate
   :ed-sub
   :ed-mul
   :ed-div
   :ed-nth-pt
   :ed-compress-pt
   :ed-decompress-pt
   :ed-validate-point
   :ed-hash
   :ed-random-pair
   :ed-random-generator
   
   :elligator-random-pt
   :elligator-tau-vector
   :elligator-encode
   :elligator-decode
   :elligator-limit
   :elligator-nbits
   :to-elligator-range
   
   :elli2-encode
   :elli2-decode
   :elli2-random-pt
   
   :ed-schnorr-sig
   :ed-schnorr-sig-verify
   
   :ed-hash
   :get-hash-bits
   
   :ed-convert-int-to-lev
   :ed-convert-lev-to-int
   :compute-deterministic-skey
   :compute-schnorr-deterministic-random
   :ed-dsa
   :ed-dsa-validate
   
   :compute-deterministic-elligator-skey
   :compute-elligator-summed-pkey
   :compute-elligator-schnorr-deterministic-random
   :elligator-ed-dsa
   :elligator-ed-dsa-validate
   ))

(defpackage :lagrange-4-square
  (:use :common-lisp)
  (:import-from :primes
   :is-prime?
   :expt-mod)
  (:import-from :useful-macros
   :curry
   :nlet-tail)
  (:export
   :decompose-integer
   ))
   
#|
(defpackage :ecc-crypto-b128
  (:use #:common-lisp)
  (:export
   ))
|#

(defpackage :core-crypto
  (:use :common-lisp
   :crypto/modular-arith
   :edwards-ecc)
  (:import-from :ecc-crypto-b571
   :convert-int-to-nbytes
   :convert-int-to-nbytesv
   :convert-bytes-to-int
   :sha3-buffers
   :sha3/256-buffers
   :ctr-drbg
   :ctr-drbg-int
   :random-between
   :get-cached-symbol-data)
  (:export
   ;; from crypto/modular-arith
   :with-mod
   :reset-blinders
   :m^
   :msqrt
   :m+
   :m-
   :m*
   :m/
   :minv
   :mmod
   :mchi
   :quadratic-residue-p
   :m!
   ;; from edwards-ecc
   :with-ed-curve
   :*edcurve*
   :*ed-r*
   :*ed-q*
   :*ed-gen*
   :ed-curve-name
   :ed-mul
   :ed-add
   :ed-sub
   :ed-div
   :ed-negate
   :ed-pt=
   :ed-affine
   :random-between
   :ed-compress-pt
   :ed-decompress-pt
   :ed-nth-pt
   :ed-random-pair
   
   :elli2-encode
   :elli2-decode
   :elli2-random-pt
   
   :ed-convert-int-to-lev
   :ed-convert-lev-to-int
   :compute-deterministic-skey
   :compute-schnorr-deterministic-random
   :ed-dsa
   :ed-dsa-validate
   
   :compute-deterministic-elligator-skey
   :compute-elligator-summed-pkey
   :compute-elligator-schnorr-deterministic-random
   :elligator-ed-dsa
   :elligator-ed-dsa-validate

   ;; from ecc-crypto-b571
   :convert-int-to-nbytes
   :convert-int-to-nbytesv
   :convert-bytes-to-int
   :sha3-buffers
   :sha3/256-buffers
   :ctr-drbg
   :ctr-drbg-int
   :random-between
   :get-cached-symbol-data
   ))
   
