;; range-proofs.lisp - blinded value range proofs
;;
;; DM/Emotiq  02/18
;; ------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2018 Emotiq AG

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
;; -------------------------------------------------------------------------
;; Compute a value range prover and verifier for use in validating
;; cryptographically blinded numbers. This version proves each bit of
;; the (integer) value. It produces a rather large structure of prime
;; number field integers, two per bit of every value being attested,
;; plus two more as cryptographic basis vector components for the
;; Pedersen Commitments offered for the value.
;;
;; Algorithm adapted to ECC crypto from the paper:
;;  "Bulletproofs: Short Proofs for Confidential Transactions and More" by
;;     Bunz, Bootle, Boneh, Poelstra, Wuille, and Maxwell.
;;       https://eprint.iacr.org/2017/1066.pdf

;; The code in this module now uses Bulletproofs for vector
;; dot-product proofs. That saves a considerable amount of memory.
;; Using Curve1174 with 252 bits per compressed ECC point, the cost is
;; roughly 1.4KB per proof, compared to a cost of 5.2KB for
;; non-Bulletproofs, atop an overhead in the basis vectors of around
;; 4.8KB.
;;
;; This code currently only provides list aggregation of proofs.
;; Algebraic aggregation will save even more memory, shrinking the
;; cost dramatically with an O(Log2(N)) cost on N-proof aggregates.

(in-package :range-proofs)

;; ------------------------------

(defvar *max-bit-length*  64)

;; ------------------------------------------------------------------

(defun rand-val ()
  ;; random value in Z_r
  (random-between 1 (pbc:get-order)))

;; ------------------------------

(defun zero-vector (&optional (nel *max-bit-length*))
  ;; create a vector of zeros
  (make-array nel
              :initial-element 0))

(defun bits-vector (n &optional (nbits *max-bit-length*))
  ;; convert a value n into a bit vector. Each index into the vector
  ;; is the i'th bit of the value
  (let* ((arr (zero-vector nbits)))
    (loop for ix from 0 below nbits do
          (when (logbitp ix n)
            (setf (aref arr ix) 1)))
    arr))

(defun random-vec (&optional (nel *max-bit-length*))
  ;; construct a vector of unique random values
  (let ((vec (make-array nel)))
    (um:nlet-tail iter ((ix 0))
      (unless (>= ix nel)
        (let ((g  (rand-val)))
          (if (find g vec) ;; prob vanishingly small chance of duplication
              (iter ix)
            (progn
              (setf (aref vec ix) g)
              (iter (1+ ix))))
          )))
    vec))

(defun random-generator ()
  ;; (pbc:mul-pt-zr (pbc:get-g1) (rand-val))
  (pbc:g1-from-hash (hash/256 (rand-val)))
  )

(defun basis-pts (&optional (nel *max-bit-length*))
  ;; compute a vector of basis-vector curve points
  (let ((vec  (make-array nel)))
    (map-into vec (lambda (x)
                    (declare (ignore x))
                    (random-generator))
              vec)))

(defun pow-vec (y &optional (nel *max-bit-length*))
  ;; construct a vector of powers of y #(1 y y^2 ... y^(n-1))
  (let ((vec (make-array nel)))
    (do ((v  1  (m* v y))
         (ix 0  (1+ ix)))
        ((>= ix nel) vec)
      (setf (aref vec ix) v))))

(defun ones-vec (&optional (nel *max-bit-length*))
  ;; a vector of 1's
  (make-array nel :initial-element 1))

(defun twos-vec (&optional (nel *max-bit-length*))
  ;; a vector of powers of 2: #(1 2 2^2 2^3 ... 2^(n-1))
  (pow-vec 2 nel))

;; ------------------------------
;; Vector arithmetic over the modular prime field

(defun vec-decr (v k)
  (map 'vector (um:rcurry 'm- k) v))

(defun vec-incr (v k)
  (map 'vector (um:rcurry 'm+ k) v))

(defun vec-add (v1 v2)
  (map 'vector 'm+ v1 v2))

(defun vec-sub (v1 v2)
  (map 'vector 'm- v1 v2))

(defun vec-scale (v k)
  (map 'vector (um:curry 'm* k) v))

(defun vec-hadamard-prod (v1 v2)
  (map 'vector 'm* v1 v2))

(defun vec-dot-prod (v1 v2)
  (reduce 'm+ (vec-hadamard-prod v1 v2)))

;; ------------------------------

(defun poly-dot-prod (poly1 poly2)
  ;;
  ;; Produce a dot-product polynomial from two polynomials with vector
  ;; coeffs. Polynomials are vectors with element at index ix
  ;; representing the ix'th order coefficient. But these coefficients
  ;; are themselves vectors of prime field integers.
  ;;
  (let ((vec   (make-array (+ (length poly1) (length poly2) -1)
                           :initial-element 0)))
    (loop for v1 across poly1
          for ix from 0
          do
          (loop for v2 across poly2
                for jx from ix
                do
                (setf (aref vec jx)
                      (m+ (aref vec jx)
                          (vec-dot-prod v1 v2)))
                ))
    vec))

;; ------------------------------
;; Pedersen Commitments

(defun simple-commit (hpt blind val)
  ;; commit to a value val with blinding blind*Hpt
  (pbc:add-pts (pbc:mul-pt-zr hpt blind)
              (pbc:mul-pt-zr (pbc:get-g1) val)))

(defun vec-commit (hpt blind hs hvec gs gvec)
  ;; form a vector commitment with basis vector gs,
  ;; blinding vector hs, and blinding term blind*Hpt
  (let ((pt (pbc:mul-pt-zr hpt blind)))
    (loop for h across hs
          for hv across hvec
          for g across gs
          for gv across gvec
          do
          (setf pt (pbc:add-pts pt
                           (pbc:add-pts (pbc:mul-pt-zr h hv)
                                       (pbc:mul-pt-zr g gv)))))
    pt))

;; ------------------------------

(defstruct range-proof-block
  ;; curve and basis vectors
  basis
  proofs)

(defstruct (range-proof
            (:constructor %make-range-proof))
  ;; commitments
  vcmt acmt scmt t1cmt t2cmt
  ;; parameters
  tau_x mu t_hat
  ;; left, right vectors
  dot-proof
  ;; challenge values (Shamir-Fiat hash values)
  x y z)

(defstruct (dot-prod-proof
            (:constructor %make-dot-prod-proof))
  u pcmt a b xlrs)

;; ------------------------------

(defstruct bp-basis
  curve nbits hpt gs hs)

(define-symbol-macro *bhpt*    (bp-basis-hpt    *bp-basis*))
(define-symbol-macro *bgs*     (bp-basis-gs     *bp-basis*))
(define-symbol-macro *bhs*     (bp-basis-hs     *bp-basis*))
(define-symbol-macro *curve*   (bp-basis-curve  *bp-basis*))
(define-symbol-macro *nbits*   (bp-basis-nbits  *bp-basis*))

(defvar *bp-basis*
  ;; define fully here for default basis so we have comparable proofs
  ;; across blockchain
  (make-bp-basis
   :CURVE :CURVE-FR449
   :NBITS 64
   :HPT #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0038fabadfe83718735a0479bf92551e72b68833347871535ec02765e33891defeda5f3b6e1b3b6875b978946ee8661a7820f8a5b7fe52542401")))

   :GS #(#.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0171da8998e07654a98c14c9713fee04fabd7affcafff05346e6489695487ae30658feae8666553cb96d61d89c60e60dc55ba2930a05d757be01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "00f9d14851afaa832c6912b34834cd82ab15b852c1b33c255db37c6111d2e7aa56801e421c0dcb66b38705dbc2fc7db0b00bb2ab1ba8e260a201")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "017ee8e91eff3afd07098fa81097a5fd33a1991b53bcc83bdc530eb2ab78916ac883b920ea98d876166d559f9e70405fac2d418481a4ee801801")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "000478637b0d91f519f9300a562ec88cd7c3519c062634094ab45e4075fa4b95f97efc5881d5b52c1125a08d0b8b2bd001570cdd0b1d66602201")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01f0edb30d3aead999e836fbb7cab412e241c5cbc5f89f8bfcbde38ef22660c38001f0edb30d3aead999e836fbb7cab412e241c5cbc5f89f8b01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "00c85d090c7f3711e81ce3afd6fc4592981eb5ef5906ea5d5802f2e0409f1dc166b9e67fcf72a854bc2a73aae6e471772e491eb22f47ca2c4701")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "007bdc896b0a4e0e82b6cff36cfe2641588283491ba7e020693ccac6f4a28aaf47b90243a5c5abc8282e11992c7a2f6dbd2fd0758c25fb308301")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "00f4676efeb1aa85ffd00cf67b5ae5cbf20be441b65cd016c34f1b8438b503c7acf90eecb20fef4b2b62339b4921593947757435ce0196940301")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01920be9a7bb6c786921dc59c3fcee2f11db87d04a745714d2a6903af624a0275abbdf0f705a2c9652b8491470c9cf713ea9e6dae8c855ba1101")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "009527ca55cd8dde00eb135853bcbfc6720c2ed10e37921a369e29e218c9065f574bdabdbea0e9312b434b17c624d61775483629dbe6216a3501")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01bee590fbf36bc31efb5b9932840dae8325ac1d0078068f4ef7bd1968640702f601bee590fbf36bc31efb5b9932840dae8325ac1d0078068f01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01d406aa063f6542252ee7cd39c0ab4b43307405444515d489a7e748407ee7918a01d406aa063f6542252ee7cd39c0ab4b43307405444515d401")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "017ba1c919e70f9dfb51f76935abd2eed966b12747f505c76feb76d035e7c16024017ba1c919e70f9dfb51f76935abd2eed966b12747f505c701")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "011282757c6ed3e12596819ab43dffd513e7446c299d21fc0044695379aa4ed58554d77d1c6428dc8be010164e293c3373337979dd511af25d01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01e95299002da3192577d92da8203a0814ebc6037b1dcce7cbf01892069d938f0001e95299002da3192577d92da8203a0814ebc6037b1dcce701")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01e1bd863f13cd774ad65a7f88356fa4a388000c13104e7473a8255a569460aad801e1bd863f13cd774ad65a7f88356fa4a388000c13104e7401")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01e3591c505df817cfa0f637a9051a6f31cf9345b1b017e9ceaabe97c942a10dd401e3591c505df817cfa0f637a9051a6f31cf9345b1b017e901")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01363634108f9d7fd3667a7b1cc0b106bf3afb4cc3565c90d60b44f3691b93c2f33dd6d2e95f9973b15aeeac7ab665e65fec8816025eedbb9101")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01c96dd81eaa41312f470dab22b6c2ef76a928980e1132718640d6e39b2b909017e814ee20fbc7adace1317daf66f96c6c74e4f16e18807a4201")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0148e973f56024332ab54927731b4df94b69768cfe09e16046aa6fd828145b2ecc93906eb06188a0dd8befe1f8578d56cf038b68bed8fc9d6901")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "002b92f4f7b82525f9dfd0838a8e0da94ec7ed6276ae6fa4f1574964bc9523579dab0dd2a32483d6d1d66371584186fc0f5a9fcc6efa6d81a401")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01f1ead39227efed9f905000fb2a9bfd1f73aec19a6cf5be81e58b5e38136ffd4401f1ead39227efed9f905000fb2a9bfd1f73aec19a6cf5be01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01da75f9cb06bcf24c8d4f6b2937c86f6d2062bf1e7fecb619062c03f4f37621d4125a26594e0288a6a27a9c93e5120c48e2b405959ad2dc6f01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01febb880bf59a9ff0b17fc8b664ab17acece2b6a1d55a6e0297c6b95936eb803a428d0b37b15492db612df4614ca290ae25f817e6721731ae01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0159ab40c0dd9d1cb221e5e3da7b73d681b19349af7418c90e6e7422f29733c04e37334fba8de1081f3c95ea8b587a09dede2e9b518eae34ec01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01ca23dca75dbfbafcb214c83e2619641853fea64dc287e857313183a91d3bf86c4067dae9f2743232460d8bfd1c885b1c3fe4f0b4eea509fa01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0187f007bf824a3b272aecac0cdec2219c0804283864a1d9b55dece1f4745a52420187f007bf824a3b272aecac0cdec2219c0804283864a1d901")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "00726afb35398afe231fcbb130cd069a5b2a3d8a4193c4384ce7c534dbd6d86a8fbc687e5acd8f4d627c72d2527a7da38ddf19581ecbc2074201")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01a478ab19cbdb4c94b2393673f6a22aca0a2a8250565ad097c773784d0da2d34001a478ab19cbdb4c94b2393673f6a22aca0a2a8250565ad001")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01c5771dd8b9ded78bf9337cf9718a413b787db72e8ae5ad05bed1eaf7b023450001c5771dd8b9ded78bf9337cf9718a413b787db72e8ae5ad01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0001c443d071f58d3d30a7bbb5f97052522c4501c3c8e0b74175865464d064a9b55321a19a11ef4336eff1604b8dd3c62ecfca9b44003461b901")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01537b637d02cc680428d2ce24663e47d942f7c268d6e73c144ec03e56e7a7eb9e01537b637d02cc680428d2ce24663e47d942f7c268d6e73c01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "012992915b9f796bd25395eaaf3e1fbe285b14d3327022543de8a7c9462da34dd4012992915b9f796bd25395eaaf3e1fbe285b14d33270225401")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "00ac0a493aea1975985d0dd94590bef77c153977b90c58c9aebd6ce135bef90c9d0d36e522a8126eb1836699673b9aa3d0ecf25e433d594ae701")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0139928c11063afb38da35a7fcaf9ab93c8f1e7a47d52206311c2960f0930412200139928c11063afb38da35a7fcaf9ab93c8f1e7a47d5220601")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01a3e8d6c24e46192cae170b3d4b1ed1cd4feb7a20f0367f14e8bade2f230c7afd15464a01e809cf073da99758b79998b72d5b9472815ab7f901")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "018283d62a6eb17d38a55558482c39827c3998a1b9b03fe53fa82ce3c5de4645c2018283d62a6eb17d38a55558482c39827c3998a1b9b03fe501")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01a5a9364fdf4d1a7f21c9bca8df297c502cfd2ff169d7f159376a938397fa8f2c01a5a9364fdf4d1a7f21c9bca8df297c502cfd2ff169d7f101")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "019ae81df0aae8ba9b625627d101208138deace359e7d9ecfb9e44923b3df1e220019ae81df0aae8ba9b625627d101208138deace359e7d9ec01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "007a4ffb0282988e639365940128a9a9316a2dd13e750e14f39f915aecee24d4a1755dec7da7aec7f23721374779ee3bb3c97425a4ecf1835a01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "016d78e5decdf50c7053f53c79e54a97025022cac01a7b0aa13c1b5d73d0da85871e4d5dbfc936cfc300dcac38898a61b4e7f184b4cef3482901")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01fa42f69a7a8502a02cb3f2f324130acc05a70528b482aec9948d07db027f4df001fa42f69a7a8502a02cb3f2f324130acc05a70528b482ae01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "014f598f27d4e1970d2ef742990ae3606b9c6e239373d6524b2442eab4e09e0dd0014f598f27d4e1970d2ef742990ae3606b9c6e239373d65201")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01e79383ce865f17b47d042dd919adc6bc8b5d91e6ed8fbf30df38c016941a1ece01e79383ce865f17b47d042dd919adc6bc8b5d91e6ed8fbf01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0167da354611044ac2e70832d0ebcff101ba8d64b56f00611b4f0c8dde3c5e3b324f463562502d6141f468dc7676ecd0db84896037c95ae71a01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "016bd41500cdfba3ff9ed60fb55b04bffd7ed3dc5175b4a2a920a71ffc4ffb63b910c3ad2c10e4c8069298c8c214a04e1412a0ef21d2e53c2401")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "019ee6985b0f9c86226c85b1abe74d2807db82f80ab1ee705b36ea3bc960df19ca019ee6985b0f9c86226c85b1abe74d2807db82f80ab1ee7001")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "018949e23cfb1dc1e9aeb2ad37163b308730b47b1addc3ec0a0a0f8107d1d9c6ac018949e23cfb1dc1e9aeb2ad37163b308730b47b1addc3ec01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "012c676b987f627901bccf0ef4e3eb6afbbb07ae40c02dba504216a1620d70ee22012c676b987f627901bccf0ef4e3eb6afbbb07ae40c02dba01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01f124d5afe9bc032775dd268b8ac232591aff49fca5a64eb354ec005912be32f401f124d5afe9bc032775dd268b8ac232591aff49fca5a64e01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0128bd849aaf30567954145b78802af8610f4b21f7738a90bf61a8c5259ad15d327037cbc03c08f39110abf7e09739c8ecb08c6e442c65beb901")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01f9aeeb4b8121e934d63113336239a0e0472655503b52e4eb285fe7a46d89fa0401f9aeeb4b8121e934d63113336239a0e0472655503b52e401")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "007f51e0a43ba1dab33be001453d1684130b79ed2c53de79377ac6f9be6e2a389b9b2c75ed0c62d8f224d05e99e26ddec15c75a368068cfd4701")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "00ac34892b612c7ac6f4dc37066882a50e533b41ed39cd266c5ff843c6b2af52c16aaaced18d6a3fb183942972bcd073a34bb04ad699ff935901")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01b9b823309ae1b57fcf99af3f353a75c815bf09b619d033030090a6f2aeed032e01b9b823309ae1b57fcf99af3f353a75c815bf09b619d03301")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "00dabe8ddfa41cf5d084e71c341c6324fcf6951f07169b89329b8bd315da14b4475498561cef5b0066e2718471100984576d14c9fd8d63ced301")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0010277c90c925170d1df969b129aa669a700dcf462f37b25bfd17ef12b6b6d3d91e753ef5fdcedf474c216c2789a903000e0ba2240f187d6a01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "005b7a86d5fe8de680a0e13376eda6144a91ac55707f55d467f7407f08753608969b79b28b03340efab42906d17c24fa6adf39e33d5450b13b01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "009f247e082fe17744e2ef720f42bbc8ac58111bed9311c5c3d66e96e82a936300c395de34f2a04087f0e25433e44c583a33a5aed5d4fe65e801")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0052cfd1990fe71ecf3b9f798b31beeaee19ea0b6cc3d378117d6cdd5b4fce59c0580c5d20be6431a8c98e6ba92521ff2073856decc01f4c1101")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "014a32dd75c8afb8fb774d93c951a5a85a23b8d211e92ec138d5d27caffcd5ab5f627475f4c93f4be648a9b18fd8a387049f328f461f07d53b01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01c75c21b4706ae87a14b10d9dc4464e1c56e67edbb093f19eb920ad62a046850401c75c21b4706ae87a14b10d9dc4464e1c56e67edbb093f101")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01b78c51ac5fca3e48769d04b56c665cd6fe7ea2842d76ae34c3abf4444aae0f9001b78c51ac5fca3e48769d04b56c665cd6fe7ea2842d76ae01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "011ed67273b753ed70fa0bec9d9d6b109f28ac64bf2e36f763d78ba751d4cb7bc4011ed67273b753ed70fa0bec9d9d6b109f28ac64bf2e36f701"))))

   :HS #(#.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0188006e8cd770ed4aa380902026aa9c94599c51fbfba9372d15ce7593b3c7610e0188006e8cd770ed4aa380902026aa9c94599c51fbfba93701")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "017ba51002c714b34015c7d61dac440de061a571586219d1704229b0affda34a78017ba51002c714b34015c7d61dac440de061a571586219d101")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "015a1ce3f20ef54c3f93ffa09de70df14bf5f2a93bd2fb36c1fb65a7828d042fd8015a1ce3f20ef54c3f93ffa09de70df14bf5f2a93bd2fb3601")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "012a0a89aa46c23c7db7094c14697aaa1eaa6b6b1dc0fd0e3471b48095aea1f308012a0a89aa46c23c7db7094c14697aaa1eaa6b6b1dc0fd0e01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "012f83b738b6262f96faa3bb03d459a00bb24d08ed6c7e8eca3e79ba4098cc4f90012f83b738b6262f96faa3bb03d459a00bb24d08ed6c7e8e01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01b8badcf3a345e3eb0b4bb5dee6c1fca5e8c8d986bcaacda7d05c52f5a0b649f8a8157e8c9694f1e80642b09627fc567827c4e88cbf173acc01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01ff26ecdfd81c7251bc7290ba3a52600c7b949e395e7c9198023fa0dc8b2acaaa01ff26ecdfd81c7251bc7290ba3a52600c7b949e395e7c9101")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0184d5d119becca882dbe34e1a31288c88bf29a4569549484e7893927011cac9680184d5d119becca882dbe34e1a31288c88bf29a45695494801")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "008dbf6cbaed5eb6d1a3a268b5668687db1931cda64c90885e3ce4dcca230662ad443de5c3d364326e4841886056ef845da34c324a62e5780d01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01cc68fcb513aa30058b8d2397c44aefc8497e5bccb74a78aa763674e4b6d5676801cc68fcb513aa30058b8d2397c44aefc8497e5bccb74a7801")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "00919049f8346239aba10179ec02aee4e0cfca7afdc2060bb0387e5d451f0e6a57e244208ad4c37801b28e6e6f9817f302bc9dc02f8378623501")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "011e9ce587b7297868109e21511b9d00cd1f0a9f794451dbb2ed073764d95f953137b4fd387421eb790015fc85edc936212aa8c751976b9b8701")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "009c004d8f73bb264ad7c46a7f4f2fca8db7f8a700590cfff9ebf3e0c07e8be4ad7ada6e1387c64d4716dc5703e7c4cf00079add50a4f03c9101")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0025409804a96cabe33cccb6c3357d5bf99c0709dc4cbbfc9299c5bb8f1d461db01e4c234ba101bee03c10abc1e3c2be59a2da563e09f5564101")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0018dd99e6b9d7342491aae9e27551cbd101da63c5c2303abae82b1526cbef7ffa7348637a5afeb806a5df31a0df5adc1d6f48b9f7e5aba18c01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "011aa5f0a82ad30d00e6c762fce976b4792eca174b6281a95c06281097c21d1f70011aa5f0a82ad30d00e6c762fce976b4792eca174b6281a901")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "013a9ab69f34a5d7bab070c4c4f7daf94d9dbf96fbf33836797e5c1ac487547fbc013a9ab69f34a5d7bab070c4c4f7daf94d9dbf96fbf3383601")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01ed8a18fb23e657cf2bca7029113473ac3fce010b936941d721c466620414669e01ed8a18fb23e657cf2bca7029113473ac3fce010b93694101")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01090f34414eca316db8a96dbb9e31356decfd3979d3a8426f8a2842615b09fd9001090f34414eca316db8a96dbb9e31356decfd3979d3a84201")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "019f4caf25a7415cc087af1d2325e260ce4954e55b14777f085a589124585d962d866742eb029819e1d14d3d84e983fec94daddafbefc699fb01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "010d809bbf52cd8ca8bc8ddba1ec815cf44379fa83614128b94b4ffb04e0e906c72f7f189faff434cc8dd9fe4e0cc0d4faccea0ef5c95f52ff01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01f8ebbd9ccf36a936896416b95b071938ac40e0a5892362703f234c00b118635c01f8ebbd9ccf36a936896416b95b071938ac40e0a589236201")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "019cb2247d3b124c80c3491f90bfa2ba48b13743f75d3989a7eb2ec8df024519eaf53511baf814ddc7753cbcd284294ffeebb26ad605e48d0601")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "00d44be61b4f5548907b870c4beace57d33fdf476279e9659363bb94724751319dc0d0e62f3c1c9ed2be1404152e11e35e98671bc7ef6e6f0301")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "019db6bfbafa908180cb7ef121e97aeb5cad89a0fefc25c811fd4eed032f2f5798019db6bfbafa908180cb7ef121e97aeb5cad89a0fefc25c801")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "00e3cbaaddb5ccc23173bd2965039af95fe63788c3c285ea1be6b358010de8503f957b75f3f339999ce4e0ff07cd25c4ed833a94863df8ee3a01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "018ce82a8b0c9ef97ce4ae4c365da20ed9dca7e9af4b36c26298599bcf3574556808144ea038ab83da2b2fccaffd52a5bf0e548aa7a37dd8be01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "00f9c2ffd531fd627b2d2a98908e6bc348a7a85c3fbe7185f4ec6bf68252fa38f540a5185c33ffa58f7e892217a4c0801e1d35927983790d7e01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "005eb99a4ca4f9f430982022521a1e33b4e7e7c532f0bf0641973372e8f047df49b872273e431b4e1f417e6ee683e1ee00adc04ce7dc2130c401")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01843c2127d54372f5e7e2969fbf470f62c0463daf89f4dbc8a769568f419758b001843c2127d54372f5e7e2969fbf470f62c0463daf89f4db01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01b8632b0dde951faf263290cd77e2fbc7cf8af5cb653ca702725398cfd377397db78800354c80dd812f13c205b2fd1415462fc388aad5eadc01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01efd33fcd5b48e86782a591234902fd7bf645744aa57b67a7833adc8cace8e31a01efd33fcd5b48e86782a591234902fd7bf645744aa57b6701")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0111d8bea10b05c2c564780a3f1dfac96766380310e841d414f7320007a0e8b9a2aa1eeced4be5d9ac6d52689266655394cc92b62ac60aa0e301")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01cd540e4dcc3748bf619c651df108cabbbb800e4f869d0169d9d41547dd190eacc8f155d0777d16ed07f3fa99416fa567fabf8aa1cf19bd7701")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "003392ebab3df7062c1153d3ce19754e86df1be27cd5e1d2cfe3d7fd8de485bfa71aec0c24542649de5acad5c8838556840632283bd278aceb01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "014d33dd56a70a7e6064e8c7de312f43de9c4f854c6086b7832e0ec322688c3708014d33dd56a70a7e6064e8c7de312f43de9c4f854c6086b701")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "013db4e39880ab2de65d2ed459382445485c06fc62ce6339a4d7aabbb665c3d910013db4e39880ab2de65d2ed459382445485c06fc62ce633901")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "018d1d040696b1b66e62493f0e6b73a6a6e80bc0b64bde4bf035ba7848bdc6d79a018d1d040696b1b66e62493f0e6b73a6a6e80bc0b64bde4b01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "018baa6cbc80eb004a8c6e1611ab6a530e123686548c1d21c54be5ee9f6b037f12018baa6cbc80eb004a8c6e1611ab6a530e123686548c1d2101")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0084b96a52efcb645d9256d7d8fbf1d49ae864530b51af56b0501787bf4679ad82e7429ed0205d57bc78958e2e192079db90f3f9cecbe6aff101")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0105782b59fb7db4cdc9b06d27f5ebdbe77cb7960d64a430a249813435c7226815cbd20aa7b69b46072e71f8dbfc24698631b83efa9db0201201")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01df2853a986a5dd834c2f6de901c24475da05405059eccaf664163932e820b0a401df2853a986a5dd834c2f6de901c24475da05405059ecca01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "00b5b6f7c9d9ea2c17403b47535f1649d4967e5afbabc31272692c2718c30793287842de413e71ac9b3534bb37ecb8dff16833637cf4be4d7e01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "00f2c3f47dc60006c9a6bb163db6293fc706e1d6fee17bca18032c296adbc135388f524ef802e06ab3c5259009101841c3cdc8151a778128cd01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01bf301759ff7c94dc0d2c6df0f2b8c5899015505cf91d52f5475e955ac693a33001bf301759ff7c94dc0d2c6df0f2b8c5899015505cf91d5201")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01fc921c135f7b1e6dbbd35fffa2eabe155cc618df8597bfdea7cefe2394cdb8b946f1cf1bcb3d1b83ef44a667d48c21f641c87a72eef8b0cb01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01a6991e66f204562725c90f3b2d1669717956a2af5f68c10f42bc303ba30dd31df27c3ed74d7e4f9622e7fded59293ace1bf47a4281255e5201")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01b41cbf1091ae7dff5e6c5743de794a55390b0a0f81f7b6742ed55d4529db707e01b41cbf1091ae7dff5e6c5743de794a55390b0a0f81f7b601")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01ec9c2ad01d4f62833c7864541c511d09a7428d4279e5fc5c62493633c83f6a052884f4b6ab09fc90b4c728d17e1cf0d933b1d08fa38feba601")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0194c643626419a947e550f4a55f479479e899c075a71e45d22beaea3b01a5e8853bd1713124149ce9f6935a7832706ab286651d75b5d9b36e01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01ea9b422e46f89942ccbb029eed6c8cc066ca668dd65211c0df42f6c4a1ae99d801ea9b422e46f89942ccbb029eed6c8cc066ca668dd6521101")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01a46f05c9689708e7c8f7a0667431a8d39055af524387b434d20257c0dc4ce51c01a46f05c9689708e7c8f7a0667431a8d39055af524387b401")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "005c91afe9c587d17545381ceb964f4c6c3a0a1ad2bec57b14ebc0b02e1a6f8ef4a015e4f712e5d41cd6fc4eda51123c4f17f330129dfeb7cf01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01ac9a11ded8edde1c728227a25892e27b499bfb1ee4d384132386f342eebf85a401ac9a11ded8edde1c728227a25892e27b499bfb1ee4d38401")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "00a039ae15527aa74885ff84442103d2eba5aa1f9741b456e7c4465e49f71e6b1abf20a201b2eb2470c5df9d0ece644af16747919f026d7dcc01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0114023435e528b1c161ad68c658114b515e3ab7c9490b95dd136236bcf89d54540114023435e528b1c161ad68c658114b515e3ab7c9490b9501")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "00b11ac8b0f311909df2a66742866362222bd6b1f1a6b16e9203d379889957a00e148922e24c4352b7ad5d258c233b25721e62ae31875c1d1901")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "019e8f0e71a44d5826b7de488c7dd2a752a969ac7c61f230056ae873c89530d80af93ad5db5271320c0b53d95d401d17de76be356cd67ab05701")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "0112d902d8cda0caa04c636fa2d4c24acdbb55c2aba10138ce176b1ffacbc32e860112d902d8cda0caa04c636fa2d4c24acdbb55c2aba1013801")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "014391edf55b8cf8c697b7fb1d4e4f407f8562507c0ece5d452f40e05e0d05015c8abe07ecfc05a84816016f3bd7e4c2ce7984c5488e8190d201")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "00a5a32a324b96936423b50660674499056e8a86b925a5bc62710acaed1553150cc34641c6623d5d8b5198dc71447f428ae52cfb5656ceba1b01")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "01829705d3c1500466409258719fd95663c6054c9855a087836cff158413136b7563a0c4b7276a993b6db466c5524b2a6644ec21c17cfa857901")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "013f99ca9f32f367cb720b6a9a6949eae2af7f8312250e20f869796bb913f95018013f99ca9f32f367cb720b6a9a6949eae2af7f8312250e2001")))
         #.(make-instance 'PBC-INTERFACE:G1-CMPR :value #.(VEC-REPR:BEV (VEC-REPR::HEX-OF-STR "017feed6e4731c762eb1e46143db97f3c2e2422e7b3c6660aff3cf68919fd81e5337113bd4859cf0a9e50400d10f2de1c08f145493c9f0626b01")))
         )))




(defvar *gs*        nil)
(defvar *hs*        nil)
(defvar *hpt*       nil)

(defun init-basis (&key (nbits *max-bit-length*)
                        (curve pbc::*curve*))
  (cond (*bp-basis*
         (cond ((eql curve *curve*)
                (cond ((= nbits *nbits*)
                       *bp-basis*)
                      ((< nbits *nbits*)
                       (make-bp-basis
                        :curve  curve
                        :nbits  nbits
                        :hpt    *bhpt*
                        :gs     *bgs*
                        :hs     *bhs*))
                      (t
                       (make-bp-basis
                        :curve  curve
                        :nbits  nbits
                        :hpt    *bhpt*
                        :gs     (concatenate 'vector 
                                             (basis-pts (- nbits *nbits*))
                                             *bgs*)
                        :hs     (concatenate 'vector
                                             (basis-pts (- nbits *nbits*))
                                             *bhs*)))
                      ))
               (t
                (make-bp-basis
                 :curve  curve
                 :nbits  nbits
                 :hpt    (random-generator)
                 :gs     (basis-pts nbits)
                 :hs     (basis-pts nbits)))
               ))

        (t
         (setf *bp-basis*
               (make-bp-basis
                :curve  curve
                :nbits  nbits
                :hpt    (random-generator)
                :gs     (basis-pts nbits)
                :hs     (basis-pts nbits))
               ))
        ))

(defun make-range-proofs (nbits &rest vals)
  (let ((*bp-basis*  (init-basis :nbits nbits)))
    (with-mod (pbc:get-order)
      (let* ((*hpt*  *bhpt*)
             (*gs*   *bgs*)
             (*hs*   *bhs*)
             (prover (make-range-prover :nbits nbits)))
        (make-range-proof-block
         :basis  *bp-basis*
         :proofs (mapcar prover vals))
        ))))

(defmethod validate-range-proofs ((proof-block range-proof-block))
  (let ((*bp-basis*  (range-proof-block-basis proof-block)))
    (with-mod (pbc:get-order)
      (let* ((*hpt*  *bhpt*)
             (*gs*   *bgs*)
             (*hs*   *bhs*))
        (every 'validate-range-proof (range-proof-block-proofs proof-block))
        ))))

;; ------------------------------
;; Construct a range-prover for use on multiple values

(defun make-range-prover (&key (nbits *max-bit-length*))
  #-ccl
  (check-type nbits (fixnum 1))
  #+ccl
  (check-type nbits fixnum)
  
  ;; let's compute the basis vectors just once, and share them
  (let* ((hpt  *hpt*)
         (hs   *hs*)
         (gs   *gs*))

    (labels
        ;; ---------------------------------------------
        ((make-range-proof (v)
           (check-type v (integer 0))
           (assert (< v (ash 1 nbits)))
           (let* ((gamma      (rand-val))
                  (vcmt       (simple-commit hpt gamma v))
                  
                  (a_l        (bits-vector v nbits))
                  (a_r        (vec-decr a_l 1))
                  (alpha      (rand-val))
                  (acmt       (vec-commit hpt alpha hs a_r gs a_l))
                  
                  (s_l        (random-vec nbits))
                  (s_r        (random-vec nbits))
                  (rho        (rand-val))
                  (scmt       (vec-commit hpt rho hs s_r gs s_l))
                  
                  ;; publish Acmt, Scmt
                  
                  (y          (int (hash/256 vcmt acmt scmt)))
                  (z          (int (hash/256 y)))
                  
                  (poly_l0    (vec-decr a_l z))
                  (poly_l1    s_l)
                  (poly_l     (vector poly_l0 poly_l1))
                  
                  (yvec       (pow-vec y nbits))
                  (zsq        (m* z z))
                  (poly_r0    (vec-add
                               (vec-hadamard-prod yvec
                                                  (vec-incr a_r z))
                               (vec-scale (twos-vec nbits) zsq)))
                  (poly_r1    (vec-hadamard-prod yvec s_r))
                  (poly_r     (vector poly_r0 poly_r1))              
                  
                  (poly_t     (poly-dot-prod poly_l poly_r))
                  (t1         (aref poly_t 1))
                  (t2         (aref poly_t 2))
                  
                  (tau1       (rand-val))
                  (tau2       (rand-val))
                  
                  (t1cmt      (simple-commit hpt tau1 t1))
                  (t2cmt      (simple-commit hpt tau2 t2))
                  
                  ;; publish T1cmt, T2cmt
                  
                  (x          (int (hash/256 t1cmt t2cmt)))
                  (lvec       (vec-add poly_l0 (vec-scale poly_l1 x)))
                  (rvec       (vec-add poly_r0 (vec-scale poly_r1 x)))
                  (t_hat      (vec-dot-prod lvec rvec))
                  (tau_x      (vec-dot-prod (pow-vec x 3)
                                            (vector (m* gamma zsq) tau1 tau2)))
                  (mu         (m+ alpha
                                  (m* rho x)))
                  
                  ;; publish tau_x, mu, t_hat, lvec, rvec
                  )
             (%make-range-proof
              ;; commitments
              :vcmt  vcmt
              :acmt  acmt
              :scmt  scmt
              :t1cmt t1cmt
              :t2cmt t2cmt
              ;; parameters
              :tau_x tau_x
              :mu    mu
              :t_hat t_hat
              :dot-proof (make-lr-dot-prod-proof y mu t_hat lvec rvec)
              ;; challenge values x, y, z
              :x     x
              :y     y
              :z     z))
           ))
      
      ;; ---------------------------------------------------
      #'make-range-proof)))

;; ---------------------------------------------------------------------
;; Range proof validation

(defun validate-range-proof (proof)
  (let* ((nbits     *nbits*)
         (y         (range-proof-y proof))
         (yvec      (pow-vec y nbits))
         (z         (range-proof-z proof))
         (zsq       (m* z z))
         (x         (range-proof-x proof))
         (xsq       (m* x x))
         (delta     (m- (m* (m- z zsq)
                            (reduce 'm+ yvec))
                        (m* (1- (ash 1 nbits))
                            (m* z zsq))))
         (chck-v-l  (simple-commit *hpt* (range-proof-tau_x proof)
                                   (range-proof-t_hat proof)))
         (vcmt      (range-proof-vcmt proof))
         (t1cmt     (range-proof-t1cmt proof))
         (t2cmt     (range-proof-t2cmt proof))
         (chck-v-r  (pbc:add-pts (pbc:mul-pt-zr vcmt zsq)
                            (pbc:add-pts (pbc:mul-pt-zr (pbc:get-g1) delta)
                                        (pbc:add-pts (pbc:mul-pt-zr t1cmt x)
                                                    (pbc:mul-pt-zr t2cmt xsq))
                                        ))))
    (when (= (int chck-v-l) (int chck-v-r))
      (let* ((*hs*      (map 'vector 'pbc:mul-pt-zr
                             *hs*
                             (pow-vec (m/ y) nbits)))
             (hpows     (vec-add (vec-scale yvec z)
                                 (vec-scale (twos-vec nbits) zsq)))
             (gpows     (vec-scale (ones-vec nbits) (m- z)))
             (acmt      (range-proof-acmt proof))
             (scmt      (range-proof-scmt proof))
             (chk-p-l   (pbc:add-pts acmt
                                    (vec-commit scmt x
                                                *hs* hpows
                                                *gs* gpows)))
             (dot-proof (range-proof-dot-proof proof))
             (p         (dot-prod-proof-pcmt dot-proof)))
        
        (when (= (int chk-p-l) (int p))
          (fast-validate-dot-prod-proof dot-proof))
        ))))

;; -----------------------------------------------------------

(defun make-lr-dot-prod-proof (y mu t_hat lvec rvec)
  ;; set up conditions for range proofs to provide Bulletproofs on
  ;; vector dot products
  (let* ((nbits (length lvec))
         (*hs*  (map 'vector 'pbc:mul-pt-zr
                     *hs*
                     (pow-vec (m/ y) nbits)))
         (u     (pbc:mul-pt-zr *hpt* (m/ mu t_hat)))
         (p     (vec-commit *hpt* mu
                            *hs*  rvec
                            *gs*  lvec)))
    
    (make-dot-prod-proof u p lvec rvec)))


(defun make-dot-prod-proof (u pcmt a b)
  ;; for input vectors a, b, commitment p, aux arg u
  ;; global basis vectors g, h, order n = length g
  ;; n should be pow2
  ;;
  ;; construct proof that c = <a b> and P = g^a h^b u^c
  ;;
  (um:nlet-tail iter ((n  (length a))
                      (g  *gs*)
                      (h  *hs*)
                      (p  pcmt)
                      (a  a)
                      (b  b)
                      (accum nil))
    (if (eql 1 n)
        (%make-dot-prod-proof ;; validate as P = g^a h^b u^(a*b)
                              :u    u
                              :pcmt pcmt
                              :a    (aref a 0)
                              :b    (aref b 0)
                              :xlrs (nreverse accum))
      ;; else
      (let* ((n/2  (ash n -1))
             (gl   (subseq g 0 n/2))
             (gr   (subseq g n/2))
             (hl   (subseq h 0 n/2))
             (hr   (subseq h n/2))
             (al   (subseq a 0 n/2))
             (ar   (subseq a n/2))
             (bl   (subseq b 0 n/2))
             (br   (subseq b n/2))
             (cl   (vec-dot-prod al br))
             (cr   (vec-dot-prod ar bl))
             (l    (vec-commit u cl
                               hl br
                               gr al))
             (r    (vec-commit u cr
                               hr bl
                               gl ar))
             ;; publish L,R
             (x     (int (hash/256 l r))) ;; challenge x
             
             (invx  (m/ x))
             (gp    (map 'vector
                         (lambda (ptl ptr)
                           (pbc:add-pts 
                            (pbc:mul-pt-zr ptl invx)
                            (pbc:mul-pt-zr ptr x)))
                         gl gr))
             (hp    (map 'vector
                         (lambda (ptl ptr)
                           (pbc:add-pts 
                            (pbc:mul-pt-zr ptl x)
                            (pbc:mul-pt-zr ptr invx)))
                         hl hr))
             
             (xsq    (m* x x))
             (xsqinv (m/ xsq))
             (pp     (pbc:add-pts
                      (pbc:mul-pt-zr l xsq)
                      (pbc:add-pts p
                              (pbc:mul-pt-zr r xsqinv))))
             
             (ap     (vec-add
                      (vec-scale al x)
                      (vec-scale ar invx)))
             (bp     (vec-add
                      (vec-scale bl invx)
                      (vec-scale br x))))
        
        (iter n/2 gp hp pp ap bp
              (cons (list x l r) accum)))
      )))

;; -------------------------------------------------------
#|
  ;; not needed with fast validation running properly...
(defun validate-dot-prod-proof (proof)
  (let* ((u         (ed-decompress-pt (dot-prod-proof-u proof)))
         (p         (ed-decompress-pt (dot-prod-proof-pcmt proof)))
         (a         (dot-prod-proof-a proof))
         (b         (dot-prod-proof-b proof))
         (xlrs      (dot-prod-proof-xlrs proof)))
    (um:nlet-tail iter ((gs   *gs*)
                        (hs   *hs*)
                        (p    p)
                        (xlrs xlrs))
      (if (endp xlrs)
          (ed-pt= p (ed-add
                     (ed-mul (aref gs 0) a)
                     (ed-add
                      (ed-mul (aref hs 0) b)
                      (ed-mul u (m* a b)))))
        ;; else
        (destructuring-bind (x lcmpr rcmpr) (first xlrs)
          (let* ((l    (ed-decompress-pt lcmpr))
                 (r    (ed-decompress-pt rcmpr))
                 (n/2  (ash (length gs) -1))
                 (gl   (subseq gs 0 n/2))
                 (gr   (subseq gs n/2))
                 (hl   (subseq hs 0 n/2))
                 (hr   (subseq hs n/2))
                 (xsq  (m* x x))
                 (pp   (ed-add
                        (ed-mul l xsq)
                        (ed-add
                         p
                         (ed-mul r (m/ xsq)))))
                 
                 (invx  (m/ x))
                 (gp    (map 'vector
                             (lambda (ptl ptr)
                               (ed-add
                                (ed-mul ptl invx)
                                (ed-mul ptr x)))
                             gl gr))
                 (hp    (map 'vector
                             (lambda (ptl ptr)
                               (ed-add
                                (ed-mul ptl x)
                                (ed-mul ptr invx)))
                             hl hr)))
            (iter gp hp pp (cdr xlrs))))
        ))))
|#             
;; --------------------------------------------------------------

(defun compute-svec (xlrs nbits)
  (let ((svec (make-array nbits))
        (xs   (mapcar 'first xlrs)))
    (loop for ix from 0 below nbits
          do
          (setf (aref svec ix)
                (let ((prod 1))
                  (loop for jx from 0
                        for x in xs
                        do
                        (setf prod (m* prod
                                       (if (logbitp jx ix)
                                           x
                                         (m/ x)))))
                  prod)))
    svec))
  
(defun fast-validate-dot-prod-proof (proof)
  (let* ((nbits     *nbits*)
         (u         (dot-prod-proof-u proof))
         (p         (dot-prod-proof-pcmt proof))
         (a         (dot-prod-proof-a proof))
         (b         (dot-prod-proof-b proof))
         (xlrs      (reverse (dot-prod-proof-xlrs proof)))
         (sv        (compute-svec xlrs nbits))
         (svinv     (map 'vector 'm/ sv))
         (chk_l     (vec-commit u (m* a b)
                                *gs* (vec-scale sv a)
                                *hs* (vec-scale svinv b)))
         (chk_r     (reduce (lambda (ans triple)
                              (destructuring-bind (x l r) triple
                                (let ((xsq (m* x x)))
                                  (pbc:add-pts ans
                                          (pbc:add-pts 
                                           (pbc:mul-pt-zr l xsq)
                                           (pbc:mul-pt-zr r (m/ xsq))))
                                  )))
                            xlrs
                            :initial-value p)))
    (= (int chk_l) (int chk_r))))

;; ----------------------------------------------------------------------
;; test it out
#|
(defun tst (nbits &rest vals)
  (let* ((proofs (apply 'make-range-proofs nbits vals)))
    (assert (validate-range-proofs proofs))
    proofs))

(defun timing-test (&optional (niter 10))
  (let* ((vals   (loop repeat niter collect (random-between 0 #.(ash 1 64))))
         (proofs (time (apply 'make-range-proofs 64 vals))))
    (assert (time (validate-range-proofs proofs)))
    proofs))
|#

