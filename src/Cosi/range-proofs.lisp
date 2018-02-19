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
;; 
;; The code in this module is pre-Bulletproofs. Bulletproofs will help
;; to shrink the proof size dramatically with a Log2 folding of vector
;; sizes.

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
   :convert-bytes-to-int
   :with-ed-curve
   :ed-nth-pt
   :ed-random-pair
   )
  (:import-from :ecc-crypto-b571
   :convert-int-to-nbytesv
   :convert-bytes-to-int)
  (:export
   :make-range-proof-system
   :range-proof-system
   :range-proof-system-prover-fn
   :range-proof-system-validator-fn
   :range-proof
   ))

(in-package :range-proofs)

;; ------------------------------

(defvar *max-bit-length*  64)

;; ------------------------------------------------------------------

(defun rand-val ()
  ;; random value in Z_r
  (random-between (ash *ed-r* -1) *ed-r*))

(defun hashem-int (&rest args)
  (convert-bytes-to-int
   (apply 'sha3-buffers args)))

;; ------------------------------------------------------------------

(defun hash-length-bits ()
  (1+ (integer-length *ed-q*)))

(defun select-sha3-hash ()
  (let ((nb  (hash-length-bits)))
    (cond ((> nb 384) :sha3)
          ((> nb 256) :sha3/384)
          (t          :sha3/256)
          )))

(defun sha3-buffers (&rest bufs)
  ;; assumes all buffers are UB8
  (let ((dig  (ironclad:make-digest (select-sha3-hash))))
    (dolist (buf bufs)
      (ironclad:update-digest dig buf))
    (ironclad:produce-digest dig)))

(defun convert-pt-to-v (pt)
  (let ((nb (ceiling (1+ (integer-length *ed-q*)) 8)))
    (convert-int-to-nbytesv (ed-compress-pt pt) nb)))

(defun hash-pt-pt (pt1 pt2)
  (let ((v1 (convert-pt-to-v pt1))
        (v2 (convert-pt-to-v pt2)))
    (convert-bytes-to-int (sha3-buffers v1 v2))))

(defun hash-pts (&rest pts)
  (convert-bytes-to-int
   (apply 'sha3-buffers (mapcar 'convert-pt-to-v pts))))

;; ------------------------------
;; Arithmetic over prime field equal to Curve order

(defun sub-mod-r (a b)
  (sub-mod *ed-r* a b))

(defun add-mod-r (a b)
  (add-mod *ed-r* a b))

(defun mult-mod-r (a b)
  (mult-mod *ed-r* a b))

(defun div-mod-r (a b)
  (div-mod *ed-r* a b))

(defun neg-mod-r (x)
  (sub-mod *ed-r* x))

(defun inv-mod-r (x)
  (div-mod *ed-r* x))

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

(defun basis-pts (&optional (nel *max-bit-length*))
  ;; compute a vector of basis-vector curve points
  (let ((vec (random-vec nel)))
    (map-into vec 'ed-nth-pt vec)))

(defun pow-vec (y &optional (nel *max-bit-length*))
  ;; construct a vector of powers of y #(1 y y^2 ... y^(n-1))
  (let ((vec (make-array nel)))
    (do ((v  1  (mult-mod-r v y))
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
  (map 'vector (um:rcurry 'sub-mod-r k) v))

(defun vec-incr (v k)
  (map 'vector (um:rcurry 'add-mod-r k) v))

(defun vec-add (v1 v2)
  (map 'vector 'add-mod-r v1 v2))

(defun vec-sub (v1 v2)
  (map 'vector 'sub-mod-r v1 v2))

(defun vec-scale (v k)
  (map 'vector (um:curry 'mult-mod-r k) v))

(defun vec-hadamard-prod (v1 v2)
  (map 'vector 'mult-mod-r v1 v2))

(defun vec-dot-prod (v1 v2)
  (reduce 'add-mod-r (vec-hadamard-prod v1 v2)))

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
                      (add-mod-r (aref vec jx)
                                 (vec-dot-prod v1 v2)))
                ))
    vec))

;; ------------------------------

(defstruct (range-proof
            (:constructor %make-range-proof))
  ;; commitments
  vcmt acmt scmt t1cmt t2cmt
  ;; parameters
  tau_x mu t_hat
  ;; left, right vectors
  lvec rvec
  ;; challenge values (Shamir-Fiat hash values)
  x y z)

(defstruct (range-proof-system
            (:constructor %make-range-proof-system))
  prover-fn  validator-fn)

;; ------------------------------
;; Construct a range-proof-system, containing closures for range-proof
;; construction and range-proof validataion

(defun make-range-proof-system (&key (nbits *max-bit-length*))
  (check-type nbits (fixnum 1))
  ;; let's compute the basis vectors just once, and share them
  (let* ((hpt  (ed-nth-pt (rand-val)))
         (hs   (basis-pts nbits))
         (gs   (basis-pts nbits))
         (hashlen (ceiling (hash-length-bits) 8)))

  (labels
      ;; ---------------------------------------------
      ;; support routines
      
      ((simple-commit (blind val)
         (ed-add (ed-mul hpt blind)
                 (ed-nth-pt val)))
       
       (vec-commit (blind hs hvec gs gvec)
         (let ((pt (ed-mul hpt blind)))
           (loop for h across hs
                 for hv across hvec
                 for g across gs
                 for gv across gvec
                 do
                 (setf pt (ed-add pt
                                  (ed-add (ed-mul h hv)
                                          (ed-mul g gv)))))
           pt))

       (int-to-vec (val)
         (convert-int-to-nbytesv val hashlen))
       
       (hash-cmpr (&rest args)
         (apply 'hashem-int (mapcar #'int-to-vec args)))

       ;; -------------------------------------------------
       
       (make-range-proof (v)
         (check-type v (integer 0))
         (assert (< v (ash 1 nbits)))
         (let* ((gamma      (rand-val))
                (vcmt       (simple-commit gamma v))
                (vcmt-cmpr  (ed-compress-pt vcmt))
                
                (a_l        (bits-vector v nbits))
                (a_r        (vec-decr a_l 1))
                (alpha      (rand-val))
                (acmt       (vec-commit alpha hs a_r gs a_l))
                (acmt-cmpr  (ed-compress-pt acmt))
                
                (s_l        (random-vec nbits))
                (s_r        (random-vec nbits))
                (rho        (rand-val))
                (scmt       (vec-commit rho hs s_r gs s_l))
                (scmt-cmpr  (ed-compress-pt scmt))
                
                ;; publish Acmt, Scmt

                (y          (hash-cmpr vcmt-cmpr acmt-cmpr scmt-cmpr))
                (z          (hashem-int (int-to-vec y)))

                (poly_l0    (vec-decr a_l z))
                (poly_l1    s_l)
                (poly_l     (vector poly_l0 poly_l1))
                (yvec       (pow-vec y nbits))
                (zsq        (mult-mod-r z z))
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
                
                (t1cmt      (simple-commit tau1 t1))
                (t1cmt-cmpr (ed-compress-pt t1cmt))
                (t2cmt      (simple-commit tau2 t2))
                (t2cmt-cmpr (ed-compress-pt t2cmt))
                
                ;; publish T1cmt, T2cmt
                
                (x          (hash-cmpr t1cmt-cmpr t2cmt-cmpr))
                (lvec       (vec-add poly_l0 (vec-scale poly_l1 x)))
                (rvec       (vec-add poly_r0 (vec-scale poly_r1 x)))
                (t_hat      (vec-dot-prod lvec rvec))
                (tau_x      (vec-dot-prod (pow-vec x 3)
                                          (vector (mult-mod-r gamma zsq) tau1 tau2)))
                (mu         (add-mod-r alpha
                                       (mult-mod-r rho x)))
                
                ;; publish tau_x, mu, t_hat, lvec, rvec
                )
           (%make-range-proof
            ;; commitments
            :vcmt  vcmt-cmpr
            :acmt  acmt-cmpr
            :scmt  scmt-cmpr
            :t1cmt t1cmt-cmpr
            :t2cmt t2cmt-cmpr
            ;; parameters
            :tau_x tau_x
            :mu    mu
            :t_hat t_hat
            :lvec  lvec
            :rvec  rvec
            ;; challenge values x, y, z
            :x     x
            :y     y
            :z     z)))

       ;; ---------------------------------------------------
       
       (validate-range-proof (proof)
         (let* ((y         (range-proof-y proof))
                (yvec      (pow-vec y nbits))
                (z         (range-proof-z proof))
                (zsq       (mult-mod-r z z))
                (x         (range-proof-x proof))
                (xsq       (mult-mod-r x x))
                (delta     (sub-mod-r
                            (mult-mod-r
                             (sub-mod-r z zsq)
                             (reduce 'add-mod-r yvec))
                            (mult-mod-r (1- (ash 1 nbits))
                                        (mult-mod-r z zsq))))
                (chck-v-l  (simple-commit (range-proof-tau_x proof)
                                          (range-proof-t_hat proof)))
                (vcmt      (ed-decompress-pt (range-proof-vcmt proof)))
                (t1cmt     (ed-decompress-pt (range-proof-t1cmt proof)))
                (t2cmt     (ed-decompress-pt (range-proof-t2cmt proof)))
                (chck-v-r  (ed-add (ed-mul vcmt zsq)
                                   (ed-add (ed-nth-pt delta)
                                           (ed-add (ed-mul t1cmt x)
                                                   (ed-mul t2cmt xsq))
                                           ))))
           (when (ed-pt= chck-v-l chck-v-r)
             (let* ((hprimes   (map 'vector 'ed-mul
                                    hs
                                    (pow-vec (inv-mod-r y) nbits)))
                    (hpows     (vec-add (vec-scale yvec z)
                                        (vec-scale (twos-vec nbits) zsq)))
                    (gpows     (vec-scale (ones-vec nbits) (neg-mod-r z)))
                    (acmt      (ed-decompress-pt (range-proof-acmt proof)))
                    (scmt      (ed-decompress-pt (range-proof-scmt proof)))
                    (chk-p-l   (ed-add acmt
                                       (ed-add (ed-mul scmt x)
                                               (vec-commit 0
                                                           hprimes hpows
                                                           gs      gpows))))
                    (lvec      (range-proof-lvec proof))
                    (rvec      (range-proof-rvec proof))
                    (chk-p-r   (vec-commit (range-proof-mu proof)
                                           hprimes rvec
                                           gs      lvec)))
               (when (ed-pt= chk-p-l chk-p-r)
                 (eql (range-proof-t_hat proof)
                      (vec-dot-prod lvec rvec))
                 )))
           )))
    ;; ------------------------------------------------
    (%make-range-proof-system
     :prover-fn     #'make-range-proof
     :validator-fn  #'validate-range-proof)
    )))

;; ----------------------------------------------------------------------
;; test it out
#|
(defun tst (n &key (nbits 4))
  (let* ((sys (make-range-proof-system :nbits nbits))
         (proof (funcall (range-proof-system-prover-fn sys) n)))
    (funcall (range-proof-system-validator-fn sys) proof)))

(defun timing-test (&optional (niter 1000))
  (time (loop repeat niter do
              (tst (random-between 0 #.(1- (ash 1 64))) :nbits 64))))

(defun proof-timing-test (&optional (niter 1000))
  (time (loop repeat niter do
              (let ((sys (make-range-proof-system :nbits 64)))
                (funcall (range-proof-system-prover-fn sys) (random-between 0 #.(1- (ash 1 64))))))))

(defun verifier-timing-test (&optional (niter 1000))
  (let* ((sys   (make-range-proof-system :nbits 64))
         (proof (funcall (range-proof-system-prover-fn sys) (random-between 0 #.(1- (ash 1 64))))))
    (time (loop repeat niter do
                (funcall (range-proof-system-validator-fn sys) proof)))))
|#
