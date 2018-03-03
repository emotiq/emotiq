;; edwards.lisp -- Edwards Curves
;; DM/RAL 07/15
;; -----------------------------------------------------------------------
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

(in-package :edwards-ecc)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 0) #+:LISPWORKS (FLOAT 0)))

;; Curve1174:  x^2 + y^2 = 1 + d*x^2*y^2
;; curve has order 4 * *ed-r* for field arithmetic over prime field *ed-q*
;;    (ed-mul *ed-gen* (* 4 *ed-r*)) -> (0, *ed-c*)
;;
;; isomorphs for d = d' * c'^4 then with (x,y) -> (x',y') = (c'*x, c'*y)
;;  x^2 + y^2 = 1 + d*x^2*y^2 -> x'^2 + y'^2 = c'^2*(1 + d'*x'^2*y'^2)
;;
;; See paper: "Elligator: Elliptic-curve points indistinguishable from uniform random strings"
;; by Bernstein, Hamburg, Krasnova, and Lange

(defstruct ed-curve
  name c d q h r gen)

;; -----------------------------------------------------------------------------
;; for cached values dependent only on curve

(defun get-cached-symbol-data (sym key1 key2 fn-compute)
  ;;
  ;; defines a generalized 2-level cache lookup, defined in the
  ;; symbol-plist of sym
  ;;
  ;; key1 = category
  ;; key2 = instance
  ;;
  (let* ((alst  (get sym key1))
         (item  (cdr (assoc key2 alst))))
    (or item
        (let ((item (funcall fn-compute)))
          (setf (get sym key1)
                (cons (cons key2 item) alst))
          item))))

;; -----------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'make-ecc-pt)
    (defstruct ecc-pt
      x y))
  (defstruct ed-proj-pt
    x y z))

;; ------------------------------------------------------------------------------
;; Curve parameters from SafeCurves
;; https://safecurves.cr.yp.to
;;
;; c = curve parameter : Theorem 1, Definition 2, c = 2/s^2, d = -(c + 1)^2/(c - 1)^2
;; d = curve parameter
;; q = F_q prime number field
;; r = prime cofactor for curve order
;; h = cofactor for curve order #E(K) = h*r
;; gen = generator point

(defvar *curve1174*
  ;; rho-security (security by Pollard's rho attack) = 2^124.3
  ;; rho-security = (* 0.886 (sqrt *ed-r*))  (0.886 = (sqrt (/ pi 4)))
  ;; x^2 + y^2 = 1 - 1174 x^2 y^2
  (make-ed-curve
   :name :Curve1174
   :c    1
   :d    -1174
   :q    (- (ash 1 251) 9)
   :r    904625697166532776746648320380374280092339035279495474023489261773642975601
   ;; = 2^249 - 11332719920821432534773113288178349711
   :h    4  ;; cofactor -- #E(K) = h*r
   :gen  (make-ecc-pt
          :x  1582619097725911541954547006453739763381091388846394833492296309729998839514
          :y  3037538013604154504764115728651437646519513534305223422754827055689195992590)
   ))

;; ---------------------------

(defvar *curve-E382*
  ;; rho-security = 2^188.8
  (make-ed-curve
   :name :Curve-E382
   :c    1
   :d    -67254
   :q    (- (ash 1 382) 105)
   :r    2462625387274654950767440006258975862817483704404090416745738034557663054564649171262659326683244604346084081047321
            ;; = 2^380 - 1030303207694556153926491950732314247062623204330168346855
   :h    4
   :gen  (make-ecc-pt
          :x  3914921414754292646847594472454013487047137431784830634731377862923477302047857640522480241298429278603678181725699
          :y  17)
   ))

;; ---------------------------

(defvar *curve41417*
  ;; rho-security = 2^205.3
  (make-ed-curve
   :name :Curve41417
   :c    1
   :d    3617
   :q    (- (ash 1 414) 17)
   :r    5288447750321988791615322464262168318627237463714249754277190328831105466135348245791335989419337099796002495788978276839289
            ;; = 2^411 - 33364140863755142520810177694098385178984727200411208589594759
   :h    8
   :gen  (make-ecc-pt
          :x  17319886477121189177719202498822615443556957307604340815256226171904769976866975908866528699294134494857887698432266169206165
          :y  34)
   ))

;; ---------------------------

(defvar *curve-E521*
  ;; rho-security = 2^259.3
  (make-ed-curve
   :name :curve-E521
   :c    1
   :d    -376014
   :q    (- (ash 1 521) 1)
   :r    1716199415032652428745475199770348304317358825035826352348615864796385795849413675475876651663657849636693659065234142604319282948702542317993421293670108523
            ;; = 2^519 - 337554763258501705789107630418782636071904961214051226618635150085779108655765
   :h    4
   :gen  (make-ecc-pt
          :x  1571054894184995387535939749894317568645297350402905821437625181152304994381188529632591196067604100772673927915114267193389905003276673749012051148356041324
          :y  12)
   ))

;; ------------------------------------------------------

(defvar *edcurve* *curve1174*)

(define-symbol-macro *ed-c*     (ed-curve-c     *edcurve*))
(define-symbol-macro *ed-d*     (ed-curve-d     *edcurve*))
(define-symbol-macro *ed-q*     (ed-curve-q     *edcurve*))
(define-symbol-macro *ed-r*     (ed-curve-r     *edcurve*))
(define-symbol-macro *ed-h*     (ed-curve-h     *edcurve*))
(define-symbol-macro *ed-gen*   (ed-curve-gen   *edcurve*))
(define-symbol-macro *ed-name*  (ed-curve-name  *edcurve*))

;; ------------------------------------------------------

(defvar *known-curves*
  (list *curve1174* *curve-e382* *curve41417* *curve-e521*))

(defmethod select-curve ((curve ed-curve))
  curve)

(defmethod select-curve ((curve symbol))
  (find curve *known-curves*
        :key 'ed-curve-name))

(defmacro with-ed-curve (curve &body body)
  `(let ((*edcurve* (select-curve ,curve)))
     ,@body))

(defun ed-curves ()
  ;; present caller with a list of symbols that can be used to select
  ;; a curve using WITH-ED-CURVE
  (mapcar 'ed-curve-name *known-curves*))

;; ----------------------------------------------------------------

(defun ed+ (&rest args)
  (apply #'add-mod *ed-q* args))

(defun ed- (&rest args)
  (apply #'sub-mod *ed-q* args))

(defun ed* (&rest args)
  (apply #'mult-mod *ed-q* args))

(defun ed/ (&rest args)
  (apply #'div-mod *ed-q* args))

(defun ed-sqrt (arg)
  (sqrt-mod *ed-q* arg))

;; expt-mod not needed here, but convenient to define vs *ed-q*
;; we use this in the elligator code below
(defun ed-expt (arg n)
  (expt-mod *ed-q* arg n))

;; ----------------------------------------------------------------

(defun ed-neutral-point ()
  (get-cached-symbol-data '*edcurve* :ed-neutral-point *ed-c*
                          (lambda ()
                            (make-ecc-pt
                             :x 0
                             :y *ed-c*))))

(defun ed-neutral-point-p (pt)
  (optima:ematch pt
    ((ecc-pt- :x x)     (zerop x))
    ((ed-proj-pt- :x x) (zerop x))
    ))

(defun ed-affine (pt)
  (optima:ematch pt
    ((ecc-pt-) pt)
    ((ed-proj-pt- :x x :y y :z z)
       (make-ecc-pt
        :x (ed/ x z)
        :y (ed/ y z)))))

(defun ed-projective (pt)
  (optima:ematch pt
    ((ecc-pt- :x x :y y)
       (let* ((alpha (random-between 1 *ed-q*)))
         (make-ed-proj-pt
          :x (ed* alpha x)
          :y (ed* alpha y)
          :z alpha)
         ))
    ((ed-proj-pt-) pt)
    ))

(defun ed-random-projective (pt)
  (let* ((alpha (random-between 1 *ed-q*)))
    (optima:ematch pt
      ((ecc-pt- :x x :y y)
         (make-ed-proj-pt
          :x (ed* alpha x)
          :y (ed* alpha y)
          :z alpha))
      ((ed-proj-pt- :x x :y y :z z)
         (make-ed-proj-pt
          :x (ed* alpha x)
          :y (ed* alpha y)
          :z (ed* alpha z)))
      )))

(defun ed-unify-pair-type (pt1 pt2)
  ;; contageon to projective coords
  (optima:ematch pt1
    ((ecc-pt-)
     (optima:ematch pt2
       ((ecc-pt-)
        (values pt1 pt2))
       ((ed-proj-pt-)
        (values (ed-projective pt1) pt2))
       ))
    ((ed-proj-pt-)
     (optima:ematch pt2
       ((ecc-pt-)
        (values pt1 (ed-projective pt2)))
       ((ed-proj-pt-)
        (values pt1 pt2))
       ))
    ))

(defun ed-pt= (pt1 pt2)
  (multiple-value-bind (upt1 upt2)
      (ed-unify-pair-type pt1 pt2)
    (optima:match upt1
      ((ecc-pt- :x x1 :y y1)
       (optima:match upt2
         ((ecc-pt- :x x2 :y y2)
            (and (= x1 x2)
                 (= y1 y2)))
         ))
      ((ed-proj-pt- :x x1 :y y1 :z z1)
       (optima:match upt2
         ((ed-proj-pt- :x x2 :y y2 :z z2)
          (and (= (ed* x1 z2)
                  (ed* x2 z1))
               (= (ed* y1 z2)
                  (ed* y2 z1))))
         ))
      )))

#|
(defun ed-pt= (pt1 pt2)
  (optima:ematch pt1
    ((ecc-pt- :x x1 :y y1)
       (optima:ematch pt2
         ((ecc-pt- :x x2 :y y2)
            (and (= x1 x2)
                 (= y1 y2)))
         ((ed-proj-pt-)
            (ed-pt= (ed-projective pt1) pt2))
         ))
    ((ed-proj-pt- :x x1 :y y1 :z z1)
       (optima:ematch pt2
         ((ecc-pt-)
            (ed-pt= pt1 (ed-projective pt2)))
         ((ed-proj-pt- :x x2 :y y2 :z z2)
            (and (= (ed* x1 z2)
                    (ed* x2 z1))
                 (= (ed* y1 z2)
                    (ed* y2 z1))))
         ))
    ))
|#

(defun ed-satisfies-curve (pt)
  (optima:ematch pt
    ((ecc-pt- :x x :y y)
       ;; x^2 + y^2 = c^2*(1 + d*x^2*y^2)
       (= (ed+ (ed* x x)
               (ed* y y))
          (ed* *ed-c* *ed-c*
               (ed+ 1 (ed* *ed-d* x x y y)))
          ))
    ((ed-proj-pt- :x x :y y :z z)
       (= (ed* z z (ed+ (ed* x x) (ed* y y)))
          (ed* *ed-c* *ed-c*
               (ed+ (ed* z z z z)
                    (ed* *ed-d* x x y y)))))
    ))

(defun ed-affine-add (pt1 pt2)
  ;; x^2 + y^2 = c^2*(1 + d*x^2*y^2)
  (um:bind* ((:struct-accessors ecc-pt ((x1 x) (y1 y)) pt1)
             (:struct-accessors ecc-pt ((x2 x) (y2 y)) pt2)
             (y1y2  (ed* y1 y2))
             (x1x2  (ed* x1 x2))
             (x1x2y1y2 (ed* *ed-d* x1x2 y1y2))
             (denx  (ed* *ed-c* (ed+ 1 x1x2y1y2)))
             (deny  (ed* *ed-c* (ed- 1 x1x2y1y2)))
             (numx  (ed+ (ed* x1 y2)
                         (ed* y1 x2)))
             (numy  (ed- y1y2 x1x2)))
    (make-ecc-pt
     :x  (ed/ numx denx)
     :y  (ed/ numy deny))
    ))

(defun ed-projective-add (pt1 pt2)
  (um:bind* ((:struct-accessors ed-proj-pt ((x1 x)
                                            (y1 y)
                                            (z1 z)) pt1)
             (:struct-accessors ed-proj-pt ((x2 x)
                                            (y2 y)
                                            (z2 z)) pt2)
             (a  (ed* z1 z2))
             (b  (ed* a a))
             (c  (ed* x1 x2))
             (d  (ed* y1 y2))
             (e  (ed* *ed-d* c d))
             (f  (ed- b e))
             (g  (ed+ b e))
             (x3 (ed* a f (ed- (ed* (ed+ x1 y1)
                                    (ed+ x2 y2))
                               c d)))
             (y3 (ed* a g (ed- d c)))
             (z3 (ed* *ed-c* f g)))
    (make-ed-proj-pt
     :x  x3
     :y  y3
     :z  z3)
    ))

(defun ed-add (pt1 pt2)
  ;; contageon to randomized projective coords for added security
  (reset-blinders)
  (multiple-value-bind (upt1 upt2)
      (ed-unify-pair-type pt1 pt2)
    (cond ((ecc-pt-p upt1) (ed-affine-add     upt1 upt2))
          (t               (ed-projective-add upt1 upt2))
          )))

#|
(defun ed-add (pt1 pt2)
  ;; contageon to randomized projective coords for added security
  (reset-blinders)
  (optima:ematch pt1
    ((ecc-pt-)
       (optima:ematch pt2
         ((ecc-pt-)     (ed-affine-add pt1 pt2))
         ((ed-proj-pt-) (ed-projective-add (ed-projective pt1) pt2))
         ))
    ((ed-proj-pt-)
       (optima:ematch pt2
         ((ecc-pt-)     (ed-projective-add pt1 (ed-projective pt2)))
         ((ed-proj-pt-) (ed-projective-add pt1 pt2))
         ))
    ))
|#

(defun ed-negate (pt)
  (optima:ematch pt
    ((ecc-pt- :x x :y y)
       (make-ecc-pt
        :x (ed- x)
        :y y))
    ((ed-proj-pt- :x x :y y :z z)
       (make-ed-proj-pt
        :x (ed- x)
        :y y
        :z z))
    ))

(defun ed-sub (pt1 pt2)
  (ed-add pt1 (ed-negate pt2)))

(defun naf (k)
  (declare (integer k))
  ;; non-adjacent form encoding of integers
  (um:nlet-tail iter ((k k)
                      (ans nil))
    (declare (integer k))
    (if (plusp k)
        (let ((kj (if (oddp k)
                      (- 2 (mod k 4))
                    0)))
          (declare (integer kj))
          (iter (ash (- k kj) -1) (cons kj ans)))
      ans)))

(defun ed-basic-mul (pt n)
  ;; this is about 50% faster than not using NAF
  (cond ((zerop n)  (ed-projective
                     (ed-neutral-point)))
        
        ((or (= n 1)
             (ed-neutral-point-p pt))  pt)
        
        (t  (let* ((r0  (ed-random-projective pt)) ;; randomize point
                   (r0n (ed-negate r0))
                   (nns (naf n))
                   (v   r0))
              (loop for nn in (cdr nns) do
                    (setf v (ed-add v v))
                    (case nn
                      (-1  (setf v (ed-add v r0n)))
                      ( 1  (setf v (ed-add v r0)))
                      ))
              v))
        ))

#|
(defun ed-basic-mul (pt n)
  ;; left-to-right algorithm
  ;; input pt is in affine or projective coords, result is in projective coords
  (cond ((zerop n)  (ed-projective
                     (ed-neutral-point)))
        
        ((or (= n 1)
             (ed-neutral-point-p pt)) pt)
        
        (t  (let* ((r0  (ed-random-projective pt)) ;; randomize point
                   (l   (1- (integer-length n)))
                   (v   (vector r0
                                r0)))
              (loop repeat l do
                    (decf l)
                    (setf (aref v 1)                  (ed-add (aref v 1) (aref v 1))
                          (aref v (ldb (byte 1 l) n)) (ed-add (aref v 1) r0)))
              (aref v 1)))
        ))
|#

(defun ed-mul (pt n)
  (let* ((alpha  (* *ed-r* *ed-h* (random-between 1 #.(ash 1 48)))))
    (ed-basic-mul pt (+ n alpha))))

(defun ed-div (pt n)
  (ed-mul pt (ed/ n)))

(defun ed-nth-pt (n)
  (ed-affine (ed-mul *ed-gen* n)))

;; ---------------------------------------------------------------
;; conversion between integers and little-endian UB8 vectors

(defun ed-nbits ()
  (get-cached-symbol-data '*edcurve* :ed-nbits *edcurve*
                          (lambda ()
                            (integer-length *ed-q*))))

(defun ed-nbytes ()
  (get-cached-symbol-data '*edcurve* :ed-nbytes *edcurve*
                          (lambda ()
                            (ceiling (ed-nbits) 8))))

(defun ed-convert-int-to-lev (v &optional (nel (ed-nbytes)))
  (unless (<= (integer-length v) (* 8 nel))
    (error "Truncation implied"))
  (let ((vec (make-array nel
                         :element-type '(unsigned-byte 8))))
    (loop for ix from 0 below nel
          for pos from 0 by 8
          do
          (setf (aref vec ix) (ldb (byte 8 pos) v)))
    vec))

(defun ed-convert-lev-to-int (vec)
  (let ((ans  0))
    (loop for v across vec
          for pos from 0 by 8
          do
          (setf (ldb (byte 8 pos) ans) v))
    ans))

;; ----------------------------------------

(defun ed-compressed-nbits ()
  (get-cached-symbol-data '*edcurve* :ed-compressed-nbits *edcurve*
                          (lambda ()
                            (1+ (ed-nbits)))))

(defun ed-compressed-nbytes ()
  (get-cached-symbol-data '*edcurve* :ed-compressed-nbytes *edcurve*
                          (lambda ()
                            (ceiling (ed-compressed-nbits) 8))))

(defun ed-compress-pt (pt &key lev) ;; lev = little-endian vector
  ;;
  ;; Standard encoding for EdDSA is X in little-endian notation, with
  ;; Odd(y) encoded as MSB beyond X.
  ;;
  ;; If lev is true, then a little-endian UB8 vector is produced,
  ;; else an integer value.
  ;;
  (um:bind* ((:struct-accessors ecc-pt (x y) (ed-affine pt)))
    (let ((enc
           (if (oddp y)
               (dpb 1 (byte 1 (ed-nbits)) x)
             x)))
      (if lev
          (let ((enc (ed-convert-int-to-lev enc (ed-compressed-nbytes))))
            (if (eq lev :base64)
                (encode-bytes-to-base64 enc)
              enc))
        enc))))

(defmethod ed-decompress-pt ((s string)) ;; assumed Base64
  (ed-decompress-pt (decode-bytes-from-base64 s)))

(defmethod ed-decompress-pt ((v vector)) ;; assumed LE UB8V
  (ed-decompress-pt (ed-convert-lev-to-int v)))

(defmethod ed-decompress-pt ((v integer))
  (let* ((nbits (ed-nbits))
         (sgn   (ldb (byte 1 nbits) v))
         (x     (dpb 0 (byte 1 nbits) v))
         (yy    (ed/ (ed* (ed+ *ed-c* x)
                          (ed- *ed-c* x))
                     (ed* (ed- 1 (ed* x x *ed-c* *ed-c* *ed-d*)))))
         (y     (ed-sqrt yy))
         (y     (if (eql sgn (ldb (byte 1 0) y))
                    y
                  (ed- y))))
    (assert (= yy (ed* y y))) ;; check that yy was a square
    ;; if yy was not a square then y^2 will equal -yy, not yy.
    (ed-validate-point
     (ed-projective
      (make-ecc-pt
       :x  x
       :y  y)))
    ))

;; -----------------------------------------------------------------

(defun ed-validate-point (pt)
  ;; guard against neutral point
  (assert (not (ed-neutral-point-p pt)))
  ;; guard against invalid curve attack
  (assert (ed-satisfies-curve pt))
  ;; guard against small subgroup attack
  (assert (not (ed-neutral-point-p (ed-basic-mul pt *ed-h*))))
  pt)

(defun ed-hash (pt)
  (sha3-buffers (ed-compress-pt pt :lev t)))

(defun get-hash-bits (nbits seed)
  ;; concatenated SHA3 until we collect enough bits
  (labels ((hash-part (ix)
             (sha3-buffers
              (loenc:encode (list ix seed)))))
    (um:nlet-tail iter ((ix   1)
                        (bits #()))
      (if (>= (* 8 (length bits)) nbits)
          bits
        (iter (1+ ix) (concatenate 'vector bits (hash-part ix)))))
    ))

(defun compute-deterministic-skey (seed &optional (index 0))
  ;;
  ;; Return a value based on seed, to be used for generating a public
  ;; key, (aka, a secret key), which is in the upper range of the
  ;; *ed-r* field, and which avoids potential small-group attacks
  ;;
  (let* ((h     (ed-convert-lev-to-int
                 (get-hash-bits (ed-nbits)
                                (list seed :generate-private-key index))))
         (nbits (1- (integer-length (floor *ed-r* *ed-h*))))
         (skey  (* *ed-h*  ;; avoid small-group attacks
                   (dpb 1 (byte 1 nbits) ;; ensure non-zero
                        (ldb (byte nbits 0) h)))))
    ;; (assert (< skey *ed-r*)) ;; should be true by construction
    skey))

(defun ed-random-pair ()
  ;; select a random private and public key from the curve, abiding by
  ;; the precautions discussed for COMPUTE-DETERMINISTIC-SKEY
  (let* ((seed (ctr-drbg 256))
         (skey (compute-deterministic-skey seed))
         (pt   (ed-nth-pt skey)))
    (values skey pt)))

(defun ed-random-generator ()
  ;; every point on the curve is a generator
  (second (multiple-value-list (ed-random-pair))))

;; -----------------------------------------------
;; madular arithmetic mod *ed-r* - for isometric prime field math

(defun mod-r (v)
  (mod v *ed-r*))

(defun hash-to-int (vec)
  (mod-r (ed-convert-lev-to-int vec)))

(defun add-mod-r (a b)
  (add-mod *ed-r* a b))

(defun sub-mod-r (a b)
  (sub-mod *ed-r* a b))

(defun mult-mod-r (a b)
  (mult-mod *ed-r* a b))

;; ---------------------------------------------------
;; The IETF EdDSA standard as a primitive

(defun compute-schnorr-deterministic-random (msgv k-priv)
  (um:nlet-tail iter ((ix 0))
    (let ((r   (mod-r
                (ed-convert-lev-to-int
                 (sha3-buffers
                  (ed-convert-int-to-lev ix 4)
                  (ed-convert-int-to-lev k-priv (ed-compressed-nbytes))
                  msgv)))))
      (if (plusp r)
          (values r (ed-nth-pt r) ix)
        (iter (1+ ix)))
      )))

(defun ed-dsa (msg skey)
  (let* ((msg-enc   (loenc:encode msg))
         (pkey      (ed-nth-pt skey))
         (pkey-cmpr (ed-compress-pt pkey)))
    (multiple-value-bind (r rpt)
        (compute-schnorr-deterministic-random msg-enc skey)
      (let* ((rpt-cmpr  (ed-compress-pt rpt))
             (nbcmpr    (ed-compressed-nbytes))
             (s         (add-mod-r
                         r
                         (mult-mod-r
                          skey
                          (ed-convert-lev-to-int
                           (sha3-buffers
                            (ed-convert-int-to-lev rpt-cmpr  nbcmpr)
                            (ed-convert-int-to-lev pkey-cmpr nbcmpr)
                            msg-enc))
                          ))))
        (list
         :msg   msg
         :pkey  pkey-cmpr
         :r     rpt-cmpr
         :s     s)
        ))))

(defun ed-dsa-validate (msg pkey r s)
  (let ((nbcmpr (ed-compressed-nbytes)))
    (ed-pt=
     (ed-nth-pt s)
     (ed-add (ed-decompress-pt r)
             (ed-mul (ed-decompress-pt pkey)
                     (ed-convert-lev-to-int
                      (sha3-buffers
                       (ed-convert-int-to-lev r    nbcmpr)
                       (ed-convert-int-to-lev pkey nbcmpr)
                       (loenc:encode msg)))
                     )))))

;; -----------------------------------------------------------

#|
(let* ((*edcurve* *curve41417*)
       ;; (*edcurve* *curve1174*)
       ;; (*edcurve* *curve-e521*)
       )
  (plt:window 'plt
              :xsize 330
              :ysize 340)
  (plt:polar-fplot 'plt `(0 ,(* 2 pi))
                   (lambda (arg)
                     (let* ((s (sin (+ arg arg)))
                            (a (* *ed-d* *ed-c* *ed-c* s s 1/4))
                            (b 1)
                            (c (- (* *ed-c* *ed-c*))))
                       (sqrt (/ (- (sqrt (- (* b b) (* 4 a c))) b)
                                (+ a a)))))
                   :clear t
                   :aspect 1))

(let* ((ans (loop for ix from 1 to 10000
                  for pt = *ed-gen* then (ed-add pt *ed-gen*)
                  collect (ecc-pt-x (ed-affine pt)))))
  (plt:plot 'raw (mapcar (um:curry #'ldb (byte 8 0)) ans)
            :clear t)
  (plt:histogram 'plt (mapcar (um:curry #'ldb (byte 8 0)) ans)
                 :clear t
                 :cum t))

(loop repeat 1000 do
      (let* ((x   (random-between 1 (* *ed-h* *ed-r*)))
             (pt  (ed-mul *ed-gen* x))
             (ptc (ed-compress-pt pt))
             (pt2 (ed-decompress-pt ptc)))
        (assert (ed-validate-point pt))
        (unless (ed-pt= pt pt2)
          (format t "~%pt1: ~A" (ed-affine pt))
          (format t "~%pt2: ~A" (ed-affine pt2))
          (format t "~%ptc: ~A" ptc)
          (format t "~%  k: ~A" x)
          (assert (ed-pt= pt pt2)))
        ))
 |#

;; -----------------------------------------------------------------------------
;; Elligator encoding of curve points

(defun elligator-limit ()
  (get-cached-symbol-data '*edcurve*
                          :elligator-limit *edcurve*
                          (lambda ()
                            (floor (1+ *ed-q*) 2))))

(defun elligator-nbits ()
  (get-cached-symbol-data '*edcurve*
                          :elligator-nbits *edcurve*
                          (lambda ()
                            (integer-length (1- (elligator-limit))))))

(defun elligator-nbytes ()
  (get-cached-symbol-data '*edcurve*
                          :elligator-nbytes *edcurve*
                          (lambda ()
                            (ceiling (elligator-nbits) 8))))

(defun elligator-padding ()
  ;; generate random padding bits for the initial byte
  ;; of an octet Elligator encoding
  (let* ((nbits (mod (elligator-nbits) 8)))
    (if (zerop nbits)
        0
      (ash (ctr-drbg-int (- 8 nbits)) nbits))))

(defun elligator-int-padding ()
  ;; generate random padding bits for an elligator int
  (let* ((enb   (elligator-nbits))
         (nbits (mod enb 8)))
    (if (zerop nbits)
        0
      (ash (ctr-drbg-int (- 8 nbits)) enb))
    ))

(defun chi (x)
  ;; chi(x) -> {-1,0,+1}
  ;; = +1 or 0 when x is square residue
  (ed-expt x (floor (1- *ed-q*) 2)))

(defun compute-csr ()
  ;; from Bernstein -- correct only for isomorph curve *ed-c* = 1
  (let* ((dp1  (ed+ *ed-d* 1))
         (dm1  (ed- *ed-d* 1))
         (dsqrt (ed* 2 (ed-sqrt (ed- *ed-d*))))
         (c    (ed/ (ed+ dsqrt dm1) dp1))
         (c    (if (quadratic-residue-p *ed-q* c)
                   c
                 (ed/ (ed- dsqrt dm1) dp1)))
         (r    (ed+ c (ed/ c)))
         (s    (ed-sqrt (ed/ 2 c))))
    (list c s r )))

(defun csr ()
  ;; Bernstein's Elligator c,s,r depend only on the curve.
  ;; Compute once and cache in the property list of *edcurve*
  ;; associating the list: (c s r) with the curve currently in force.
  (get-cached-symbol-data '*edcurve* :elligator-csr
                          *edcurve* #'compute-csr))

(defun to-elligator-range (x)
  (logand x (1- (ash 1 (elligator-nbits)))))

(defun elligator-decode (z)
  ;; z in (1,2^(floor(log2 *ed-q*/2)))
  ;; good multiple of bytes for curve-1174 is 248 bits = 31 bytes
  ;;                            curve-E382    376        47
  ;;                            curve-41417   408        51
  ;;                            curve-E521    520        65
  ;; from Bernstein -- correct only for isomorph curve *ed-c* = 1
  (let ((z (to-elligator-range z)))
    (cond ((= z 1)
           (ed-neutral-point))
          
          (t
           (um:bind* (((c s r) (csr))
                      (u     (ed/ (ed- 1 z) (ed+ 1 z)))
                      (u^2   (ed* u u))
                      (c^2   (ed* c c))
                      (u2c2  (ed+ u^2 c^2))
                      (u2cm2 (ed+ u^2 (ed/ c^2)))
                      (v     (ed* u u2c2 u2cm2))
                      (chiv (chi v))
                      (xx   (ed* chiv u))
                      (yy   (ed* (ed-expt (ed* chiv v) (truncate (1+ *ed-q*) 4))
                                 chiv
                                 (chi u2cm2)))
                      (1+xx (ed+ xx 1))
                      (x    (ed/ (ed* (ed- c 1)
                                      s
                                      xx
                                      1+xx)
                                 yy))
                      (y   (ed/ (ed- (ed* r xx)
                                     (ed* 1+xx 1+xx))
                                (ed+ (ed* r xx)
                                     (ed* 1+xx 1+xx))))
                      (pt  (ed-projective
                            (make-ecc-pt
                             :x  x
                             :y  y))))
             ;; (assert (ed-satisfies-curve pt))
             pt
             ))
          )))
 
(defun elligator-encode (pt)
  ;; from Bernstein -- correct only for isomorph curve *ed-c* = 1
  ;; return encoding tau for point pt, or nil if pt not in image of phi(tau)
  (if (ed-neutral-point-p pt)
      (logior 1 (elligator-int-padding))
    ;; else
    (um:bind* ((:struct-accessors ecc-pt (x y) (ed-affine pt))
               (yp1  (ed+ y 1)))
      (unless (zerop yp1)
        (um:bind* (((c s r) (csr))
                   (etar       (ed* r (ed/ (ed- y 1) (ed* 2 yp1))))
                   (etarp1     (ed+ 1 etar))
                   (etarp1sqm1 (ed- (ed* etarp1 etarp1) 1))
                   (scm1       (ed* s (ed- c 1))))
          (when (and (quadratic-residue-p *ed-q* etarp1sqm1)
                     (or (not (zerop (ed+ etar 2)))
                         (= x (ed/ (ed* 2 scm1 (chi c))
                                   r))))
            (um:bind* ((xx    (ed- (ed-expt etarp1sqm1 (floor (1+ *ed-q*) 4))
                                   etarp1))
                       (z     (chi (ed* scm1
                                        xx
                                        (ed+ xx 1)
                                        x
                                        (ed+ (ed* xx xx) (ed/ (ed* c c))))))
                       (u     (ed* z xx))
                       (enc   (ed/ (ed- 1 u) (ed+ 1 u)))
                       (tau   (min enc (ed- enc))))
              ;; (assert (ed-pt= pt (elligator-decode enc))) ;; check that pt is in the Elligator set
              ;; (assert (< tau (elligator-limit)))
              (logior tau (elligator-int-padding))
              ))
          )))
    ))

;; -------------------------------------------------------

(defun compute-deterministic-elligator-skey (seed &optional (index 0))
  ;; compute a private key from the seed that is safe, and produces an
  ;; Elligator-capable public key.
  (um:nlet-tail iter ((ix  index))
    (let* ((skey (compute-deterministic-skey seed ix))
           (pkey (ed-nth-pt skey))
           (tau  (elli2-encode pkey)))
      (if tau
          (values skey tau ix)
        (iter (1+ ix))))))

(defun compute-elligator-summed-pkey (sum-pkey)
  ;; post-processing step after summing public keys. This corrects the
  ;; summed key to become an Elligator-capable public key. Can only be
  ;; used on final sum, not on intermediate partial sums.
  (um:nlet-tail iter ((ix 0))
    (let ((p  (ed-add sum-pkey (ed-nth-pt ix))))
      (or (elli2-encode p)
          (iter (1+ ix))))))
#|
(multiple-value-bind (skey1 pkey1) (compute-elligator-skey :dave)
  (multiple-value-bind (skey2 pkey2) (compute-elligator-skey :dan)
    (let ((p  (ed-add pkey1 pkey2)))
      (compute-elligator-summed-pkey p))))

(defun tst (nel)
  (let ((ans nil)
        (dict (make-hash-table)))
    (loop for ix from 0 below nel do
          (multiple-value-bind (skey tau ct)
              (compute-elligator-skey (ed-convert-int-to-lev ix 4))
            (if (gethash skey dict)
                (print "Collision")
              (setf (gethash skey dict) tau))
            (when (plusp ct)
              (push (cons ix ct) ans))))
    ans))
 |#
             
(defun compute-elligator-schnorr-deterministic-random (msgv k-priv)
  (um:nlet-tail iter ((ix 0))
    (let* ((r     (mod-r
                   (ed-convert-lev-to-int
                    (sha3-buffers
                     (ed-convert-int-to-lev ix 4)
                     (ed-convert-int-to-lev k-priv (elligator-nbytes))
                     msgv))))
           (rpt   (ed-nth-pt r))
           (tau-r (elli2-encode rpt)))
      (if (and (plusp r) tau-r)
          (values r tau-r ix)
        (iter (1+ ix)))
      )))

#|
(defun tst (nel)
  (let ((ans  nil)
        (skey (compute-elligator-skey :dave)))
    (loop for ix from 0 below nel do
          (multiple-value-bind (r rpt ct)
              (compute-elligator-schnorr-deterministic-random
               (ed-convert-int-to-lev ix 4) skey)
            (declare (ignore r rpt))
            (when (plusp ct)
              (push (cons ix ct) ans))))
    ans))
 |#

(defun elligator-ed-dsa (msg k-priv)
  (let ((msg-enc (loenc:encode msg))
        (tau-pub (elli2-encode (ed-nth-pt k-priv))))
    (unless tau-pub
      (error "Not an Elligator key"))
    (multiple-value-bind (r tau-r)
        (compute-elligator-schnorr-deterministic-random msg-enc k-priv)
      (let* ((nbytes (elligator-nbytes))
             (s      (add-mod-r
                      r
                      (mult-mod-r
                       k-priv
                       (ed-convert-lev-to-int
                        (sha3-buffers
                         (ed-convert-int-to-lev tau-r   nbytes)
                         (ed-convert-int-to-lev tau-pub nbytes)
                         msg-enc))
                       ))))
        (list
         :msg     msg
         :tau-pub tau-pub
         :tau-r   tau-r
         :s       s)
        ))))

(defun elligator-ed-dsa-validate (msg tau-pub tau-r s)
  (let ((nbytes (elligator-nbytes)))
    (ed-pt=
     (ed-nth-pt s)
     (ed-add (elli2-decode tau-r)
             (ed-mul (elli2-decode tau-pub)
                     (ed-convert-lev-to-int
                      (sha3-buffers
                       (ed-convert-int-to-lev tau-r   nbytes)
                       (ed-convert-int-to-lev tau-pub nbytes)
                       (loenc:encode msg)))
                     )))))

;; ------------------------------------------------------------

(defun do-elligator-random-pt (fn-gen)
  ;; search for a random multiple of *ed-gen*
  ;; such that the point is in the Elligator set.
  ;; Return a property list of
  ;;  :r   = the random integer in [1,q)
  ;;  :pt  = the random point in projective form
  ;;  :tau = the Elligator encoding of the random point
  ;;  :pad = bits to round out the integer length to multiple octets
  (um:nlet-tail iter ()
    (multiple-value-bind (skey pkey) (ed-random-pair)
      (let ((tau  (and (plusp skey)
                       (funcall fn-gen pkey)))) ;; elligatorable? - only about 50% are
        (if tau
            (list :r   skey
                  :pt  pkey
                  :tau tau
                  :pad (elligator-int-padding))
          (iter))
        ))))

(defun do-elligator-schnorr-sig (msg tau-pub k-priv fn-gen)
  ;; msg is a message vector suitable for hashing
  ;; tau-pub is the Elligator vector encoding for public key point pt-pub
  ;; k-priv is the private key integer for pt-pub = k-priv * *ec-gen*
  (um:nlet-tail iter ()
    (let* ((lst   (funcall fn-gen))
           (vtau  (elligator-tau-vector lst))
           (h     (convert-bytes-to-int (sha3-buffers vtau tau-pub msg)))
           (r     (getf lst :r))
           (q     (* *ed-h* *ed-r*))
           (s     (add-mod q r (mult-mod q h k-priv)))
           (smax  (elligator-limit)))
      (if (>= s smax)
          (progn
            ;; (print "restart ed-schnorr-sig")
            (iter))
        (let* ((nbits (integer-length smax))
               (nb    (ceiling nbits 8))
               (spad  (logior s (elligator-int-padding)))
               (svec  (convert-int-to-nbytes spad nb)))
          (list vtau svec))
        ))))

(defun do-elligator-schnorr-sig-verify (msg tau-pub sig fn-decode)
  ;; msg is a message vector suitable for hashing
  ;; tau-pub is the Elligator vector encoding for public key point pt-pub
  ;; k-priv is the private key integer for pt-pub = k-priv * *ec-gen*
  (um:bind* (((vtau svec) sig)
             (pt-pub (funcall fn-decode tau-pub))
             (pt-r   (funcall fn-decode (convert-bytes-to-int vtau)))
             (h      (convert-bytes-to-int (sha3-buffers vtau tau-pub msg)))
             (s      (to-elligator-range (convert-bytes-to-int svec)))
             (pt     (ed-mul *ed-gen* s))
             (ptchk  (ed-add pt-r (ed-mul pt-pub h))))
    (ed-pt= pt ptchk)))

;; -------------------------------------------------------

(defun elligator-random-pt ()
  (do-elligator-random-pt #'elligator-encode))

(defun elligator-tau-vector (lst)
  ;; lst should be the property list returned by elligator-random-pt
  (let* ((tau (getf lst :tau))
         (pad (getf lst :pad))
         (nb  (ceiling (elligator-nbits) 8)))
    (convert-int-to-nbytesv (+ tau pad) nb)))

(defun ed-schnorr-sig (m tau-pub k-priv)
  (do-elligator-schnorr-sig m tau-pub k-priv #'elligator-random-pt))

(defun ed-schnorr-sig-verify (m tau-pub sig)
  (do-elligator-schnorr-sig-verify m tau-pub sig #'elligator-decode))

;; -------------------------------------------------------

#|
(defun chk-elligator ()
  (loop repeat 1000 do
        ;; ix must be [0 .. (q-1)/2]
        (let* ((ix (random-between 0 (floor (1+ *ed-q*) 2)))
               (pt (elligator-decode ix))
               (jx (elligator-encode pt)))
          (assert (= ix jx))
          )))
(chk-elligator)

(let* ((lst     (elligator-random-pt))
       (k-priv  (getf lst :r))
       (pt-pub  (getf lst :pt))
       (tau-pub (elligator-tau-vector lst))
       (msg     (ensure-8bitv "this is a test"))
       (sig     (ed-schnorr-sig msg tau-pub k-priv)))
   (ed-schnorr-sig-verify msg tau-pub sig))

(let* ((lst     (elligator-random-pt))
       (k-priv  (getf lst :r))
       (pt-pub  (getf lst :pt))
       (tau-pub (elligator-tau-vector lst))
       (msg     (ensure-8bitv "this is a test"))
       (sig     (elli2-schnorr-sig msg tau-pub k-priv)))
   (elli2-schnorr-sig-verify msg tau-pub sig))

(let ((arr (make-array 256
                       :initial-element 0)))
  (loop repeat 10000 do
        (let* ((lst (elligator-random-pt))
               (tau (getf lst :tau)))
          (incf (aref arr (ldb (byte 8 200) tau)))))
  (plt:histogram 'xhisto arr
                 :clear t)
  (plt:plot 'histo arr
            :clear t)
  )
        
                      
|#

#|
(let* ((c4d  (ed* *ed-c* *ed-c* *ed-c* *ed-c* *ed-d*))
       (a    (ed/ (ed* 2 (ed+ c4d 1)) (ed- c4d 1)))
       (b    1))
  (ed* a b (ed- (ed* a a) (ed* 4 b)))) ;; must not be zero
|#

;; --------------------------------------------------------

(defun find-quadratic-nonresidue (m)
  (um:nlet-tail iter ((n  -1))
    (if (quadratic-residue-p m n)
        (iter (1- n))
      n)))

(defun compute-elli2-ab ()
  ;; For Edwards curves:  x^2 + y^2 = c^2*(1 + d*x^2*y^2)
  ;; x --> -2*c*u/v
  ;; y --> c*(1+u)/(1-u)
  ;; to get Elliptic curve: v^2 = (c^4*d -1)*(u^3 + A*u^2 + B*u)
  ;; v --> w*Sqrt(c^4*d - 1)
  ;; to get: w^2 = u^3 + A*u^2 + B*u
  ;; we precompute c4d = c^4*d, A = 2*(c^4*d+1)/(c^4*d-1), and B = 1
  ;; must have: A*B*(A^2 - 4*B) != 0
  (let* ((c4d        (ed* *ed-c* *ed-c* *ed-c* *ed-c* *ed-d*))
         (sqrt-c4dm1 (ed-sqrt (ed- c4d 1))) ;; used during coord conversion
         (a          (ed/ (ed* 2 (ed+ c4d 1)) (ed- c4d 1)))
         (b          1)
         (u          (find-quadratic-nonresidue *ed-q*))
         (dscr       (ed- (ed* a a) (ed* 4 b))))
    ;; (assert (not (quadratic-residue-p *ed-q* dscr)))
    (assert (not (zerop (ed* a b dscr))))
    (list sqrt-c4dm1 a b u)))

(defun elli2-ab ()
  ;; Bernstein's Elligator c,s,r depend only on the curve.
  ;; Compute once and cache in the property list of *edcurve*
  ;; associating the list: (c s r) with the curve currently in force.
  (get-cached-symbol-data '*edcurve* :elligator2-ab
                          *edcurve* #'compute-elli2-ab))

(defun montgy-pt-to-ed (pt)
  ;; v = w * Sqrt(c^4*d-1)
  ;; x = -2*c*u/v
  ;; y = c*(1+u)/(1-u)
  ;; w^2 = u^3 + A*u^2 + B =>  x^2 + y^2 = c^2*(1 + d*x^2*y^2)
  (um:bind* ((:struct-accessors ecc-pt ((xu x)
                                        (yw y)) pt))
    (if (and (zerop xu)
             (zerop yw))
        (ed-neutral-point)
      ;; else
      (destructuring-bind (sqrt-c4dm1 a b u) (elli2-ab)
        (declare (ignore a b u))
        (let* ((yv   (ed* sqrt-c4dm1 yw))
               (x    (ed/ (ed* -2 *ed-c* xu) yv))
               (y    (ed/ (ed* *ed-c* (ed+ 1 xu)) (ed- 1 xu)))
               (pt   (make-ecc-pt
                      :x  x
                      :y  y)))
          (assert (ed-satisfies-curve pt))
          pt)))))
              
(defun elli2-decode (r)
  ;; protocols using the output of elli2-decode must validate the
  ;; results. All output will be valid curve points, but many
  ;; protocols must avoid points of low order and the neutral point.
  ;;
  ;; Elli2-encode will provide a value of 0 for the neutral point.
  ;; But it will refuse to generate a value for the other low order
  ;; torsion points.
  ;;
  ;; However, that doesn't prevent an attacker from inserting values
  ;; for them.  There are no corresponding values for the low order
  ;; torsion points, apart from the neutral point. But certain random
  ;; values within the domain [0..(q-1)/2] are invalid.
  ;;
  (let ((r (to-elligator-range r)))
    (cond  ((zerop r)  (ed-neutral-point))
           (t 
            (um:bind* (((sqrt-c4dm1 a b u) (elli2-ab))
                       (ur2   (ed* u r r))
                       (1pur2 (ed+ 1 ur2)))
              
              ;; the following error can never trigger when fed with
              ;; r from elli2-encode. But random values fed to us
              ;; could cause it to trigger the error.
              
              (when (or (zerop 1pur2)     ;; this could happen for r^2 = -1/u
                        (= (ed* a a ur2)  ;; this can never happen: B=1 so RHS is square
                           (ed* b 1pur2 1pur2)))  ;; and LHS is not square.
                (error "invalid argument"))
              
              (let* ((v    (ed- (ed/ a 1pur2)))
                     (eps  (chi (ed+ (ed* v v v)
                                     (ed* a v v)
                                     (ed* b v))))
                     (xu   (ed- (ed* eps v)
                                (ed/ (ed* (ed- 1 eps) a) 2)))
                     (rhs  (ed* xu
                                (ed+ (ed* xu xu)
                                     (ed* a xu)
                                     b)))
                     (yw   (ed- (ed* eps (ed-sqrt rhs))))
                     ;; now we have (xu, yw) as per Bernstein: yw^2 = xu^3 + A*xu^2 + B*xu
                     ;; Now convert to our Edwards coordinates:
                     ;;   (xu,yw) --> (x,y): x^2 + y^2 = c^2*(1 + d*x^2*y^2)
                     (yv   (ed* sqrt-c4dm1 yw))
                     (x    (ed/ (ed* -2 *ed-c* xu) yv))
                     (y    (ed/ (ed* *ed-c* (ed+ 1 xu)) (ed- 1 xu)))
                     (pt   (ed-projective
                            (make-ecc-pt
                             :x  x
                             :y  y))))
                #|
                (assert (ed-satisfies-curve pt)) ;; true by construction
                (assert (not (zerop (ed* v eps xu yw)))) ;; true by construction
                (assert (= (ed* yw yw) rhs))     ;; true by construction
                |#
                pt
                )))
           )))
  
(defun ed-pt-to-montgy (pt)
  ;; u = (y - c)/(y + c)
  ;; v = -2 c u / x
  ;; w = v / sqrt(c^4 d - 1)
  ;; montgy pt (u,w) in: w^2 = u^3 + A u^2 + B u
  (if (ed-neutral-point-p pt)
      (make-ecc-pt
       :x 0
       :y 0)
    ;; else
    (um:bind* ((:struct-accessors ecc-pt (x y) (ed-affine pt))
               ((sqrt-c4dm1 a b u) (elli2-ab))
               (declare (ignore u))
               (xu   (ed/ (ed- y *ed-c*) (ed+ y *ed-c*)))
               (yv   (ed/ (ed* -2 *ed-c* xu) x ))
               (yw   (ed/ yv sqrt-c4dm1)))
      (assert (= (ed* yw yw)
                 (ed+ (ed* xu xu xu)
                      (ed* a xu xu)
                      (ed* b xu))))
      (make-ecc-pt
       :x xu
       :y yw))))
             
(defun elli2-encode (pt)
  (cond ((ed-neutral-point-p pt)
         (elligator-int-padding))
        (t 
         (um:bind* ((:struct-accessors ecc-pt (x y) (ed-affine pt))
                    ((sqrt-c4dm1 a b u) (elli2-ab))
                    (declare (ignore b))
                    ;; convert our Edwards coords to the form needed by Elligator-2
                    ;; for Montgomery curves
                    (xu   (ed/ (ed- y *ed-c*) (ed+ y *ed-c*)))
                    (yv   (ed/ (ed* -2 *ed-c* xu) x ))
                    (yw   (ed/ yv sqrt-c4dm1))
                    (xu+a (ed+ xu a)))
           ;; now we have (x,y) --> (xu,yw) for:  yw^2 = xu^3 + A*xu^2 + B*xu
           (when (and (not (zerop xu+a))
                      (or (not (zerop yw))
                          (zerop xu))
                      (quadratic-residue-p *ed-q* (ed- (ed* u xu xu+a))))
             (let* ((e2    (if (quadratic-residue-p *ed-q* yw)
                               (ed/ xu xu+a)
                             (ed/ xu+a xu)))
                    (enc   (ed-sqrt (ed/ e2 (ed- u))))
                    (tau   (min enc (ed- enc)))
                    ;; (ur2   (ed* u tau tau))
                    ;; (1pur2 (ed+ 1 ur2))
                    )
               
               ;; (assert (< tau (elligator-limit)))
               #|
               (when (zerop 1pur2) ;; never happens, by construction
                 (format t "~%Hit magic #1: tau = ~A" tau))
               (when (= (ed* a a ur2) ;; never happens, by construction
                        (ed* b 1pur2 1pur2))
                 (format t "~%Hit magic #2: tau = ~A" tau))
               
               (unless (or (zerop 1pur2) ;; never happens, by construction
                           (= (ed* a a ur2)
                              (ed* b 1pur2 1pur2)))
                 tau)
               |#
               (logior tau (elligator-int-padding))
          ))))
        ))
                
(defun elli2-random-pt ()
  (do-elligator-random-pt #'elli2-encode))

(defun elli2-schnorr-sig (m tau-pub k-priv)
  (do-elligator-schnorr-sig m tau-pub k-priv #'elli2-random-pt))

(defun elli2-schnorr-sig-verify (m tau-pub sig)
  (do-elligator-schnorr-sig-verify m tau-pub sig #'elli2-decode))

;; ------------------------------------------------------------------------------
;; General scheme for creating private / public keys with Elligator encodings...
;; (elli2-random-pt) => property list with (getf ans :r) = private key integer
;;                                         (+ (getf ans :tau)
;;                                            (getf ans :padding)) = public Elligator integer
;; ------------------------------------------------------------------------------
#|
(defun chk-elli2 ()
  (loop repeat 100 do
        (let* ((lst (elli2-random-pt))
               (pt  (ed-affine (getf lst :pt)))
               (tau (getf lst :tau))
               (pt2 (ed-affine (elli2-decode tau))))
          (assert (ed-pt= pt pt2))
          )))
(chk-elli2)

(let ((arr (make-array 256
                       :initial-element 0)))
  (loop repeat 10000 do
        (let* ((lst (elli2-random-pt))
               (tau (getf lst :tau)))
          (incf (aref arr (ldb (byte 8 200) tau)))))
  (plt:histogram 'xhisto arr
                 :clear t)
  (plt:plot 'histo arr
            :clear t)
  )

;; pretty darn close to 50% of points probed
;; result in successful Elligator-2 encodings
(let ((cts 0))
  (loop repeat 1000 do
        (let* ((ix (random-between 1 (* *ed-h* *ed-r*)))
               (pt (ed-mul *ed-gen* ix)))
          (when (elli2-encode pt)
            (incf cts))))
  (/ cts 1000.0))
|#
             