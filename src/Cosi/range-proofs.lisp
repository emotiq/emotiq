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
  (random-between (ash *ed-r* -1) *ed-r*))

;; ------------------------------------------------------------------

(defun hash-length-bits ()
  (1+ (integer-length *ed-q*)))

(defun hash-length-bytes ()
  (ceiling (hash-length-bits) 8))

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

(defun hashem-int (&rest args)
  ;; args should be UB8V
  ;; output integer in prime field group
  (mod
   (convert-bytes-to-int
    (apply 'sha3-buffers args))
   *ed-r*))

(defun int-to-vec (val)
  ;; for consistent hashing of integer values
  (convert-int-to-nbytesv val (hash-length-bytes)))
       
(defun hash-cmpr (&rest args)
  ;; for hashing a list of compressed ECC points
  (apply 'hashem-int (mapcar #'int-to-vec args)))

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
;; Pedersen Commitments

(defun simple-commit (hpt blind val)
  ;; commit to a value val with blinding blind*Hpt
  (ed-add (ed-mul hpt blind)
          (ed-nth-pt val)))

(defun vec-commit (hpt blind hs hvec gs gvec)
  ;; form a vector commitment with basis vector gs,
  ;; blinding vector hs, and blinding term blind*Hpt
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

;; ------------------------------

(defvar *gs*    nil)
(defvar *hs*    nil)
(defvar *hpt*   nil)

(defstruct range-proof-block
  ;; curve and basis vectors
  curve
  gs hs hpt
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

(defun make-range-proofs (nbits &rest vals)
  (let* ((*max-bit-length* nbits)
         (*hpt*    (ed-nth-pt (rand-val)))
         (*gs*     (basis-pts nbits))
         (*hs*     (basis-pts nbits))
         (prover   (make-range-prover :nbits nbits)))
    (make-range-proof-block
     :curve  (ed-curve-name *edcurve*)
     :gs     (map 'vector 'ed-compress-pt *gs*)
     :hs     (map 'vector 'ed-compress-pt *hs*)
     :hpt    (ed-compress-pt *hpt*)
     :proofs (mapcar prover vals))
    ))

(defun validate-range-proofs (proof-block)
  (with-ed-curve (range-proof-block-curve proof-block)
    (let* ((*hpt*  (ed-decompress-pt (range-proof-block-hpt proof-block)))
           (*hs*   (map 'vector 'ed-decompress-pt (range-proof-block-hs proof-block)))
           (*gs*   (map 'vector 'ed-decompress-pt (range-proof-block-gs proof-block)))
           (*max-bit-length* (length *gs*)))
      (every 'validate-range-proof (range-proof-block-proofs proof-block)))))

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
                  (vcmt-cmpr  (ed-compress-pt vcmt))
                  
                  (a_l        (bits-vector v nbits))
                  (a_r        (vec-decr a_l 1))
                  (alpha      (rand-val))
                  (acmt       (vec-commit hpt alpha hs a_r gs a_l))
                  (acmt-cmpr  (ed-compress-pt acmt))
                  
                  (s_l        (random-vec nbits))
                  (s_r        (random-vec nbits))
                  (rho        (rand-val))
                  (scmt       (vec-commit hpt rho hs s_r gs s_l))
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
                  
                  (t1cmt      (simple-commit hpt tau1 t1))
                  (t1cmt-cmpr (ed-compress-pt t1cmt))
                  (t2cmt      (simple-commit hpt tau2 t2))
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
  (let* ((nbits     (length *gs*))
         (y         (range-proof-y proof))
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
         (chck-v-l  (simple-commit *hpt* (range-proof-tau_x proof)
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
      (let* ((*hs*      (map 'vector 'ed-mul
                             *hs*
                             (pow-vec (inv-mod-r y) nbits)))
             (hpows     (vec-add (vec-scale yvec z)
                                 (vec-scale (twos-vec nbits) zsq)))
             (gpows     (vec-scale (ones-vec nbits) (neg-mod-r z)))
             (acmt      (ed-decompress-pt (range-proof-acmt proof)))
             (scmt      (ed-decompress-pt (range-proof-scmt proof)))
             (chk-p-l   (ed-add acmt
                                (vec-commit scmt x
                                            *hs* hpows
                                            *gs* gpows)))
             (dot-proof (range-proof-dot-proof proof))
             (p         (ed-decompress-pt (dot-prod-proof-pcmt dot-proof))))
        
        (when (ed-pt= chk-p-l p)
          (fast-validate-dot-prod-proof dot-proof))
        ))))

;; -----------------------------------------------------------

(defun make-lr-dot-prod-proof (y mu t_hat lvec rvec)
  ;; set up conditions for range proofs to provide Bulletproofs on
  ;; vector dot products
  (let* ((nbits (length lvec))
         (*hs*  (map 'vector 'ed-mul
                     *hs*
                     (pow-vec (inv-mod-r y) nbits)))
         (u     (ed-mul *hpt* (div-mod-r mu t_hat)))
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
           :u    (ed-compress-pt u)
           :pcmt (ed-compress-pt pcmt)
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
             (lcmpr (ed-compress-pt l))
             (rcmpr (ed-compress-pt r))
             (x     (hash-cmpr lcmpr rcmpr)) ;; challenge x
             
             (invx  (inv-mod-r x))
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
                         hl hr))
             
             (xsq    (mult-mod-r x x))
             (xsqinv (inv-mod-r xsq))
             (pp     (ed-add
                      (ed-mul l xsq)
                      (ed-add p
                              (ed-mul r xsqinv))))
             
             (ap     (vec-add
                      (vec-scale al x)
                      (vec-scale ar invx)))
             (bp     (vec-add
                      (vec-scale bl invx)
                      (vec-scale br x))))
        
        (iter n/2 gp hp pp ap bp
              (cons (list x lcmpr rcmpr) accum)))
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
                      (ed-mul u (mult-mod-r a b)))))
        ;; else
        (destructuring-bind (x lcmpr rcmpr) (first xlrs)
          (let* ((l    (ed-decompress-pt lcmpr))
                 (r    (ed-decompress-pt rcmpr))
                 (n/2  (ash (length gs) -1))
                 (gl   (subseq gs 0 n/2))
                 (gr   (subseq gs n/2))
                 (hl   (subseq hs 0 n/2))
                 (hr   (subseq hs n/2))
                 (xsq  (mult-mod-r x x))
                 (pp   (ed-add
                        (ed-mul l xsq)
                        (ed-add
                         p
                         (ed-mul r (inv-mod-r xsq)))))

                 (invx  (inv-mod-r x))
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
                        (setf prod (mult-mod-r prod
                                               (if (logbitp jx ix)
                                                   x
                                                 (inv-mod-r x)))))
                  prod)))
    svec))
  
(defun fast-validate-dot-prod-proof (proof)
  (let* ((nbits     (length *gs*))
         (u         (ed-decompress-pt (dot-prod-proof-u proof)))
         (p         (ed-decompress-pt (dot-prod-proof-pcmt proof)))
         (a         (dot-prod-proof-a proof))
         (b         (dot-prod-proof-b proof))
         (xlrs      (reverse (dot-prod-proof-xlrs proof)))
         (sv        (compute-svec xlrs nbits))
         (svinv     (map 'vector 'inv-mod-r sv))
         (chk_l     (vec-commit u (mult-mod-r a b)
                                *gs* (vec-scale sv a)
                                *hs* (vec-scale svinv b)))
         (chk_r     (reduce (lambda (ans triple)
                              (destructuring-bind (x lcmpr rcmpr) triple
                                (let ((xsq (mult-mod-r x x))
                                      (l   (ed-decompress-pt lcmpr))
                                      (r   (ed-decompress-pt rcmpr)))
                                  (ed-add ans
                                          (ed-add
                                           (ed-mul l xsq)
                                           (ed-mul r (inv-mod-r xsq))))
                                  )))
                            xlrs
                            :initial-value p)))
    (ed-pt= chk_l chk_r)))

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

