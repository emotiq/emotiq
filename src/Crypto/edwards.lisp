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
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))

;; Curve1174:  x^2 + y^2 = 1 + d*x^2*y^2
;; curve has order 4 * *ed-r* for field arithmetic over prime field *ed-q*
;;    (ed-mul *ed-gen* (* 4 *ed-r*)) -> (0, *ed-c*)
;;
;; isomorphs for d = d' * c'^4 then with (x,y) -> (x',y') = (c'*x, c'*y)
;;  x^2 + y^2 = 1 + d*x^2*y^2 -> x'^2 + y'^2 = c'^2*(1 + d'*x'^2*y'^2)
;;
;; See paper: "Elligator: Elliptic-curve points indistinguishable from uniform random strings"
;; by Bernstein, Hamburg, Krasnova, and Lange
;;
;; See also, https://safecurves.cr.yp.to

(defstruct ed-curve
  name c d q h r gen)

;; -----------------------------------------------------------

(defstruct ecc-pt
  x y)

(defstruct ed-proj-pt
  x y z)

(defmethod make-load-form ((point ed-proj-pt) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots point))

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

(defvar *chk-curve1174*
  ;; = (hex-str (hash/256 *curve1174))
  "50c953d8d6f83ab18a8a475acc04567cf621ea543a576f20d70f1f1784a3c727")

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

(defvar *chk-curve-E382*
  ;; = (hex-str (hash/256 *curve-E382*))
  "58aa9ce913eb890d831f3ec0e59d0aaaf32304cc4240bfc6ff045599b8ac8419")

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

(defvar *chk-curve41417*
  ;; = (hex-str (hash/256 *curve41417*))
  "a26c08cde33e9d353b0ec47090c1d6fdea32842d9e5b7130a026b09e02d10fd0")

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

(defvar *chk-curve-E521*
  ;; = (hex-str (hash/256 *curve-E521*))
  "689a5918f2d28f1d62965551d44c635a141b623ad672b464b21052b481a21420")

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

(defun ed-neutral-point ()
  (get-cached-symbol-data '*edcurve*
                          :ed-neutral-point *ed-c*
                          (lambda ()
                            (make-ecc-pt
                             :x 0
                             :y *ed-c*))))

(defun ed-neutral-point-p (pt)
  (optima:ematch pt
    ((ecc-pt- :x x)     (zerop x))
    ((ed-proj-pt- :x x) (zerop x))
    ))

;; ---------------------------------------------------------
;; Attempt faster modular field arithmetic

(defun fast-m* (arg &rest args)
  (declare (integer arg)
           (optimize  (speed 3)
                      (safety 0)))
  (setf arg (mmod arg))
  (dolist (opnd args)
    (declare (integer opnd))
    (setf arg (mmod (* arg (the integer (mmod opnd))))))
  arg)

(defvar *fast-q-mods*
  (let ((arr (make-array 256 :initial-element 0)))
    (with-mod *ed-q*
      (loop for ix from 0 below 256
            for v = (ash 1 (integer-length *ed-q*)) then (+ v v)
            do
            (setf (aref arr ix) (mmod v)))
      arr)))
    
(defun fast-mod (arg base)
  (declare (integer arg base)
           (optimize (speed 3)
                     (safety 0)))
  (let* ((nb0   (integer-length base))
         (m     (mod (ash 1 nb0) base))
         (nbm1  (1- nb0))
         (mnbm1 (- nbm1)))
    (declare (fixnum nb0 nbm1)
             (integer m))
    (um:nlet-tail iter ((a arg))
      (declare (integer a))
      (let* ((f    (ldb (byte nbm1 0) a))
             (w    (ash a mnbm1)))
        (declare (fixnum nb)
                 (integer f w))
        (if (zerop w)
            f
          (iter  (+ f (* w m))))
        ))))
  
(defun fast-q-mod (arg)
  (declare (integer arg)
           (optimize (speed 3)
                     (safety 0)))
  (um:nlet-tail iter ((a arg))
    (declare (integer a))
    (let* ((f  (ldb (byte 251 0) a))
           (w  (ash a -251)))
      (declare (integer f w))
      (if (zerop w)
          (if (< f *ed-q*)
              f
            (- f *ed-q*))
        (iter (+ f (* 9 w))))
      )))

(defun fast-q-mod* (arg1 arg2)
  (declare (integer arg1 arg2)
           (optimize (speed 3)
                     (safety 0)))
  
  (let ((a1-0    (ldb (byte 64   0) arg1))
        (a1-64   (ldb (byte 64  64) arg1))
        (a1-128  (ldb (byte 64 128) arg1))
        (a1-192  (ldb (byte 64 192) arg1))
        (a1-256  (ash arg1 -256))
        
        (a2-0    (ldb (byte 64   0) arg2))
        (a2-64   (ldb (byte 64  64) arg2))
        (a2-128  (ldb (byte 64 128) arg2))
        (a2-192  (ldb (byte 64 192) arg2))
        (a2-256  (ash arg2 -256)))
    
    (declare (integer a1-0 a1-64 a1-128 a1-192 a1-256
                      a2-0 a2-64 a2-128 a2-192 a2-256))

    (fast-q-mod (+ (* a1-0 a2-0)
                   (* 288 (+ (* a1-64  a2-192)
                             (* a1-128 a2-128)
                             (* a1-192 a2-64)
                             (* a1-256 a2-256 288)))
                   
                   (ash (+ (* a1-0  a2-64)
                           (* a1-64 a2-0)
                           (* 288 (+ (* a1-128 a2-192)
                                     (* a1-192 a2-128))))
                        64)
                   
                   (ash (+ (* a1-0   a2-128)
                           (* a1-64  a2-64)
                           (* a1-128 a2-0)
                           (* a1-192 a2-192 288))
                        128)
                   
                   (ash (+ (* a1-0   a2-192)
                           (* a1-64  a2-128)
                           (* a1-128 a2-64)
                           (* a1-192 a2-0))
                        192)
                   ))))
  
#|
(with-mod *ed-q*
  (let ((x (* *ed-r* *ed-r*)))
    (time (loop repeat 1000000 do
                (mmod x)))))
  
(with-mod *ed-q*
  (let ((x (* *ed-r* *ed-r*)))
    (time (loop repeat 1000000 do
                (fast-mod x *ed-q*)))))

(with-mod *ed-q*
  (let ((x (* *ed-r* *ed-r*)))
    (time (loop repeat 1000000 do
                (fast-q-mod x)))))

;; --------------------------------------------

(with-mod *ed-q*
  (time (loop repeat 1000000 do
              (m* *ed-r* *ed-r*))))

(time (loop repeat 1000000 do
            (fast-q-mod* *ed-r* *ed-r*)))


 |#
#|
(lw:defadvice (m* fast-m* :around) (arg &rest args)
  (apply 'fast-m* arg args))
|#

;; ---------------------------------------------------------

(defun ed-affine (pt)
  (optima:ematch pt
    ((ecc-pt-) pt)
    ((ed-proj-pt- :x x :y y :z z)
     (with-mod *ed-q*
       (make-ecc-pt
        :x (m/ x z)
        :y (m/ y z))))))

(defun ed-projective (pt)
  (optima:ematch pt
    ((ecc-pt- :x x :y y)
     (progn ;; let* ((alpha (random-between 1 *ed-q*)))
         (with-mod *ed-q*
           (make-ed-proj-pt
            :x x ;; (m* alpha x)
            :y y ;; (m* alpha y)
            :z 1 ;; alpha
            )
           )))
    ((ed-proj-pt-) pt)
    ))

(defun ed-random-projective (pt)
  (with-mod *ed-q*
    (progn ;; let* ((alpha (random-between 1 *ed-q*)))
      ;; (declare (integer alpha))
      (optima:ematch pt
        ((ecc-pt- :x x :y y)
         (make-ed-proj-pt
          :x x ;; (m* alpha x)
          :y y ;; (m* alpha y)
          :z 1 ;; alpha
          ))
        ((ed-proj-pt- :x x :y y :z z)
         (make-ed-proj-pt
          :x x ;; (m* alpha x)
          :y y ;; (m* alpha y)
          :z z ;; (m* alpha z)
          ))
        ))))

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
          (with-mod *ed-q*
            (and (= (m* x1 z2)
                    (m* x2 z1))
                 (= (m* y1 z2)
                    (m* y2 z1)))))
         ))
      )))

(defun ed-satisfies-curve (pt)
  (with-mod *ed-q*
    (optima:ematch pt
      ((ecc-pt- :x x :y y)
       ;; x^2 + y^2 = c^2*(1 + d*x^2*y^2)
       (= (m+ (m* x x)
              (m* y y))
          (m* *ed-c* *ed-c*
              (m+ 1 (m* *ed-d* x x y y)))
          ))
      ((ed-proj-pt- :x x :y y :z z)
       (= (m* z z (m+ (m* x x) (m* y y)))
          (m* *ed-c* *ed-c*
              (m+ (m* z z z z)
                  (m* *ed-d* x x y y)))))
      )))

(defun ed-affine-add (pt1 pt2)
  ;; x^2 + y^2 = c^2*(1 + d*x^2*y^2)
  (with-mod *ed-q*
    (um:bind* ((:struct-accessors ecc-pt ((x1 x) (y1 y)) pt1)
               (:struct-accessors ecc-pt ((x2 x) (y2 y)) pt2)
               (y1y2  (m* y1 y2))
               (x1x2  (m* x1 x2))
               (x1x2y1y2 (m* *ed-d* x1x2 y1y2))
               (denx  (m* *ed-c* (m+ 1 x1x2y1y2)))
               (deny  (m* *ed-c* (m- 1 x1x2y1y2)))
               (numx  (m+ (m* x1 y2)
                          (m* y1 x2)))
               (numy  (m- y1y2 x1x2)))
      (make-ecc-pt
       :x  (m/ numx denx)
       :y  (m/ numy deny))
      )))

(defun ed-projective-add (pt1 pt2)
  (with-mod *ed-q*
    (um:bind* ((:struct-accessors ed-proj-pt ((x1 x)
                                              (y1 y)
                                              (z1 z)) pt1)
               (:struct-accessors ed-proj-pt ((x2 x)
                                              (y2 y)
                                              (z2 z)) pt2)
               (a  (m* z1 z2))
               (b  (m* a a))
               (c  (m* x1 x2))
               (d  (m* y1 y2))
               (e  (m* *ed-d* c d))
               (f  (m- b e))
               (g  (m+ b e))
               (x3 (m* a f (m- (m* (m+ x1 y1)
                                   (m+ x2 y2))
                               c d)))
               (y3 (m* a g (m- d c)))
               (z3 (m* *ed-c* f g)))
      (make-ed-proj-pt
       :x  x3
       :y  y3
       :z  z3)
      )))

(defun ed-add (pt1 pt2)
  ;; contageon to randomized projective coords for added security
  ;; (reset-blinders)
  (multiple-value-bind (upt1 upt2)
      (ed-unify-pair-type pt1 pt2)
    (cond ((ecc-pt-p upt1) (ed-affine-add     upt1 upt2))
          (t               (ed-projective-add upt1 upt2))
          )))

(defun ed-negate (pt)
  (with-mod *ed-q*
    (optima:ematch pt
      ((ecc-pt- :x x :y y)
       (make-ecc-pt
        :x (m- x)
        :y y))
      ((ed-proj-pt- :x x :y y :z z)
       (make-ed-proj-pt
        :x (m- x)
        :y y
        :z z))
      )))

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

(defun ed-mul (pt n)
  #|
  (let* ((alpha  (* *ed-r* *ed-h* (random-between 1 #.(ash 1 48)))))
    (ed-basic-mul pt (+ n alpha)))
  |#
  (ed-basic-mul pt n))

(defun ed-div (pt n)
  (with-mod *ed-r*
    (ed-mul pt (m/ n))))

(defun ed-nth-pt (n)
  (ed-affine (ed-mul *ed-gen* n)))

;; ---------------------------------------------------------------
;; conversion between integers and little-endian UB8 vectors

(defun ed-nbits ()
  (get-cached-symbol-data '*edcurve*
                          :ed-nbits *edcurve*
                          (lambda ()
                            (integer-length *ed-q*))))

(defun ed-nbytes ()
  (get-cached-symbol-data '*edcurve*
                          :ed-nbytes *edcurve*
                          (lambda ()
                            (ceiling (ed-nbits) 8))))

;; ----------------------------------------

(defun ed-compressed-nbits ()
  (get-cached-symbol-data '*edcurve*
                          :ed-compressed-nbits *edcurve*
                          (lambda ()
                            (1+ (ed-nbits)))))

(defun ed-compressed-nbytes ()
  (get-cached-symbol-data '*edcurve*
                          :ed-compressed-nbytes *edcurve*
                          (lambda ()
                            (ceiling (ed-compressed-nbits) 8))))

(defun ed-compress-pt (pt &key lev) ;; lev = little-endian vector
  ;;
  ;; Standard encoding for EdDSA is X in little-endian notation, with
  ;; Odd(Y) encoded as MSB beyond X.
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
          (let ((enc (levn enc (ed-compressed-nbytes))))
            (if (eq lev :base58)
                (base58 enc)
              enc))
        enc))))

(defmethod ed-decompress-pt ((x ub8v))
  (ed-decompress-pt (int x)))

(defmethod ed-decompress-pt ((v integer))
  (with-mod *ed-q*
    (let* ((nbits (ed-nbits))
           (sgn   (ldb (byte 1 nbits) v))
           (x     (dpb 0 (byte 1 nbits) v))
           (yy    (m/ (m* (m+ *ed-c* x)
                          (m- *ed-c* x))
                      (m* (m- 1 (m* x x *ed-c* *ed-c* *ed-d*)))))
           (y     (msqrt yy))
           (y     (if (eql sgn (ldb (byte 1 0) y))
                      y
                    (m- y))))
      (assert (= yy (m* y y))) ;; check that yy was a square
      ;; if yy was not a square then y^2 will equal -yy, not yy.
      (ed-validate-point
       (ed-projective
        (make-ecc-pt
         :x  x
         :y  y)))
      )))

;; -----------------------------------------------------------------

(defun ed-valid-point-p (pt)
  (and (not (ed-neutral-point-p pt))
       (ed-satisfies-curve pt)
       (not (ed-neutral-point-p (ed-basic-mul pt *ed-h*)))
       pt))

(defun ed-validate-point (pt)
  (assert (ed-valid-point-p pt))
  pt)

;; -----------------------------------------------------------------

(defmethod hashable ((x ecc-pt))
  (hashable (ed-compress-pt x :lev t)))

(defmethod hashable ((x ed-proj-pt))
  (hashable (ed-affine x)))

(defun get-hash-nbits (nbits seed)
  "Concatenated SHA3 until we collect enough bits"
  (get-hash-nbytes (ceiling nbits 8) seed))

;; -------------------------------------------------

(defun compute-deterministic-skey (seed &optional (index 0))
  "Return a value based on seed, to be used for generating a public
  key, (aka, a secret key), which is in the upper range of the
  *ed-r* field, and which avoids potential small-group attacks"
  (let* ((nbits (integer-length *ed-r*))
         (h     (int
                 (get-hash-nbits nbits
                                 (list seed :generate-private-key index))))
         (s     (dpb 1 (byte 1 (1- nbits)) ;; set hi bit
                     (ldb (byte nbits 0) h)))
         (skey  (- s (mod s *ed-h*))))
    (if (< skey *ed-r*) ;; will be true with overwhelming probability (failure ~1e-38)
        skey
      (compute-deterministic-skey seed (1+ index)))
    ))

(defun ed-random-pair ()
  "Select a random private and public key from the curve, abiding by
  the precautions discussed for COMPUTE-DETERMINISTIC-SKEY"
  (let* ((seed (ctr-drbg 256))
         (skey (compute-deterministic-skey seed))
         (pt   (ed-nth-pt skey)))
    (values skey pt)))

;; -----------------------------------------------------
;; Hashing onto curve

(defun ed-from-hash (h)
  "Hash onto curve. Treat h as X coord with sign indication,
just like a compressed point. Then if Y is a quadratic residue
we are done. Else re-probe with (X^2 + 1)."
  (with-mod *ed-q*
    (let* ((nbits (ed-nbits))
           (v     (int h))
           (sgn   (ldb (byte 1 nbits) v))
           (x     (mmod (ldb (byte nbits 0) v))))
      (um:nlet-tail iter ((x x))
        (let ((yy (m/ (m* (m+ *ed-c* x)
                          (m- *ed-c* x))
                      (m* (m- 1 (m* x x *ed-c* *ed-c* *ed-d*))))
                  ))
          (if (quadratic-residue-p yy)
              (let* ((y  (msqrt yy))
                     (y  (if (eql sgn (ldb (byte 1 0) y))
                             y
                           (m- y))))
                (or (ed-valid-point-p
                     ;; Watch out! This multiply by cofactor is necessary
                     ;; to prevent winding up in a small subgroup.
                     (ed-basic-mul (make-ecc-pt
                                    :x x
                                    :y y)
                                   *ed-h*))
                    (iter (m+ 1 (m* x x)))))
            ;; else 
            (iter (m+ 1 (m* x x)))
            ))))))

(defun ed-random-generator ()
  (ed-from-hash (get-hash-nbits (1+ (ed-nbits))
                                (random-between 1 *ed-q*))))

;; ---------------------------------------------------
;; The IETF EdDSA standard as a primitive

(defun compute-schnorr-deterministic-random (msgv k-priv)
  (um:nlet-tail iter ((ix 0))
    (let ((r   (with-mod *ed-r*
                 (mmod
                  (int
                   (bev (hash/512 
                          (levn ix 4)
                          (levn k-priv (ed-compressed-nbytes))
                          msgv)))))))
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
             (s         (with-mod *ed-r*
                          (m+ r
                              (m* skey
                                  (int
                                   (bev (hash/512
                                         (levn rpt-cmpr nbcmpr)
                                         (levn pkey-cmpr nbcmpr)
                                         msg-enc))
                                   ))))))
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
                     (int
                      (bev (hash/512
                            (levn r nbcmpr)
                            (levn pkey nbcmpr)
                            (lev  (loenc:encode msg)))))
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

(defun elligator-int-padding ()
  ;; generate random padding bits for an elligator int
  (let* ((enb   (elligator-nbits))
         (nbits (mod enb 8)))
    (if (zerop nbits)
        0
      (ash (ctr-drbg-int (- 8 nbits)) enb))
    ))

(defun compute-csr ()
  ;; from Bernstein -- correct only for isomorph curve *ed-c* = 1
  (with-mod *ed-q*
    (let* ((dp1  (m+ *ed-d* 1))
           (dm1  (m- *ed-d* 1))
           (dsqrt (m* 2 (msqrt (m- *ed-d*))))
           (c    (m/ (m+ dsqrt dm1) dp1))
           (c    (if (quadratic-residue-p c)
                     c
                   (m/ (m- dsqrt dm1) dp1)))
           (r    (m+ c (m/ c)))
           (s    (msqrt (m/ 2 c))))
      (list c s r ))))

(defun csr ()
  ;; Bernstein's Elligator c,s,r depend only on the curve.
  ;; Compute once and cache in the property list of *edcurve*
  ;; associating the list: (c s r) with the curve currently in force.
  (get-cached-symbol-data '*edcurve*
                          :elligator-csr *edcurve*
                          'compute-csr))

(defun to-elligator-range (x)
  (ldb (byte (elligator-nbits) 0) x))

(defun elligator-decode (z)
  ;; z in (1,2^(floor(log2 *ed-q*/2)))
  ;; good multiple of bytes for curve-1174 is 248 bits = 31 bytes
  ;;                            curve-E382    376        47
  ;;                            curve-41417   408        51
  ;;                            curve-E521    520        65
  ;; from Bernstein -- correct only for isomorph curve *ed-c* = 1
  (let ((z (to-elligator-range z)))
    (declare (integer z))
    (cond ((= z 1)
           (ed-neutral-point))
          
          (t
           (with-mod *ed-q*
             (um:bind* (((c s r) (csr))
                        (u     (m/ (m- 1 z) (m+ 1 z)))
                        (u^2   (m* u u))
                        (c^2   (m* c c))
                        (u2c2  (m+ u^2 c^2))
                        (u2cm2 (m+ u^2 (m/ c^2)))
                        (v     (m* u u2c2 u2cm2))
                        (chiv  (mchi v))
                        (xx    (m* chiv u))
                        (yy    (m* (m^ (m* chiv v) (truncate (1+ *ed-q*) 4))
                                   chiv
                                   (mchi u2cm2)))
                        (1+xx  (m+ xx 1))
                        (x     (m/ (m* (m- c 1)
                                       s
                                       xx
                                       1+xx)
                                   yy))
                        (y     (m/ (m- (m* r xx)
                                       (m* 1+xx 1+xx))
                                   (m+ (m* r xx)
                                       (m* 1+xx 1+xx))))
                        (pt    (ed-projective
                                (make-ecc-pt
                                 :x  x
                                 :y  y))))
               ;; (assert (ed-satisfies-curve pt))
               pt
               )))
          )))
 
(defun elligator-encode (pt)
  ;; from Bernstein -- correct only for isomorph curve *ed-c* = 1
  ;; return encoding tau for point pt, or nil if pt not in image of phi(tau)
  (if (ed-neutral-point-p pt)
      (logior 1 (elligator-int-padding))
    ;; else
    (with-mod *ed-q*
      (um:bind* ((:struct-accessors ecc-pt (x y) (ed-affine pt))
                 (yp1  (m+ y 1)))
        (unless (zerop yp1)
          (um:bind* (((c s r) (csr))
                     (etar       (m* r (m/ (m- y 1) (m* 2 yp1))))
                     (etarp1     (m+ 1 etar))
                     (etarp1sqm1 (m- (m* etarp1 etarp1) 1))
                     (scm1       (m* s (m- c 1))))
            (when (and (quadratic-residue-p etarp1sqm1)
                       (or (not (zerop (m+ etar 2)))
                           (= x (m/ (m* 2 scm1 (mchi c))
                                    r))))
              (um:bind* ((xx    (m- (m^ etarp1sqm1 (floor (1+ *ed-q*) 4))
                                    etarp1))
                         (z     (mchi (m* scm1
                                          xx
                                          (m+ xx 1)
                                          x
                                          (m+ (m* xx xx) (m/ (m* c c))))))
                         (u     (m* z xx))
                         (enc   (m/ (m- 1 u) (m+ 1 u)))
                         (tau   (min enc (m- enc))))
                ;; (assert (ed-pt= pt (elligator-decode enc))) ;; check that pt is in the Elligator set
                ;; (assert (< tau (elligator-limit)))
                (logior tau (elligator-int-padding))
                ))
            ))))
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
    (let* ((r     (with-mod *ed-r*
                    (mmod
                     (int
                      (bev (hash/512
                            (levn ix 4)
                            (levn k-priv (elligator-nbytes))
                            msgv))))))
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
  (let ((msg-enc (lev (loenc:encode msg)))
        (tau-pub (elli2-encode (ed-nth-pt k-priv))))
    (unless tau-pub
      (error "Not an Elligator key"))
    (multiple-value-bind (r tau-r)
        (compute-elligator-schnorr-deterministic-random msg-enc k-priv)
      (let* ((nbytes (elligator-nbytes))
             (s      (with-mod *ed-r*
                       (m+ r
                           (m* k-priv
                               (int
                                (bev (hash/512
                                      (levn tau-r nbytes)
                                      (levn tau-pub nbytes)
                                      msg-enc))
                                ))))))
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
                     (int
                      (bev (hash/512
                            (levn tau-r   nbytes)
                            (levn tau-pub nbytes)
                            (lev (loenc:encode msg)))))
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
                  :tau tau)
          (iter))
        ))))

(defun elligator-tau-vector (tau)
  ;; lst should be the property list returned by elligator-random-pt
  (levn tau (elligator-nbytes)))

(defun do-elligator-schnorr-sig (msg tau-pub k-priv fn-gen)
  ;; msg is a message vector suitable for hashing
  ;; tau-pub is the Elligator vector encoding for public key point pt-pub
  ;; k-priv is the private key integer for pt-pub = k-priv * *ec-gen*
  (um:nlet-tail iter ()
    (let* ((lst   (funcall fn-gen))
           (vtau  (elligator-tau-vector (getf lst :tau)))
           (h     (int
                   (hash/512
                    vtau
                    (elligator-tau-vector tau-pub)
                    msg)))
           (r     (getf lst :r))
           (s     (with-mod *ed-r*
                    (m+ r (m* h k-priv))))
           (smax  (elligator-limit)))
      (if (>= s smax)
          (progn
            ;; (print "restart ed-schnorr-sig")
            (iter))
        (let* ((spad  (logior s (elligator-int-padding)))
               (svec  (elligator-tau-vector spad)))
          (list vtau svec))
        ))))

(defun do-elligator-schnorr-sig-verify (msg tau-pub sig fn-decode)
  ;; msg is a message vector suitable for hashing
  ;; tau-pub is the Elligator vector encoding for public key point pt-pub
  ;; k-priv is the private key integer for pt-pub = k-priv * *ec-gen*
  (um:bind* (((vtau svec) sig)
             (pt-pub (funcall fn-decode tau-pub))
             (pt-r   (funcall fn-decode (int vtau)))
             (h      (int
                      (hash/512
                       vtau
                       (elligator-tau-vector tau-pub)
                       msg)))
             (s      (to-elligator-range (int svec)))
             (pt     (ed-nth-pt s))
             (ptchk  (ed-add pt-r (ed-mul pt-pub h))))
    (ed-pt= pt ptchk)))

;; -------------------------------------------------------

(defun elligator-random-pt ()
  (do-elligator-random-pt #'elligator-encode))

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
(with-mod *ed-q*
  (let* ((c4d  (m* *ed-c* *ed-c* *ed-c* *ed-c* *ed-d*))
         (a    (m/ (m* 2 (m+ c4d 1)) (m- c4d 1)))
         (b    1))
    (m* a b (m- (m* a a) (m* 4 b))))) ;; must not be zero
|#

;; --------------------------------------------------------

(defun find-quadratic-nonresidue ()
  (um:nlet-tail iter ((n  -1))
    (if (quadratic-residue-p n)
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
  (with-mod *ed-q*
    (let* ((c4d        (m* *ed-c* *ed-c* *ed-c* *ed-c* *ed-d*))
           (sqrt-c4dm1 (msqrt (m- c4d 1))) ;; used during coord conversion
           (a          (m/ (m* 2 (m+ c4d 1)) (m- c4d 1)))
           (b          1)
           (u          (find-quadratic-nonresidue))
           (dscr       (m- (m* a a) (m* 4 b))))
      ;; (assert (not (quadratic-residue-p *ed-q* dscr)))
      (assert (not (zerop (m* a b dscr))))
      (list sqrt-c4dm1 a b u))))
  
(defun elli2-ab ()
  ;; Bernstein's Elligator c,s,r depend only on the curve.
  ;; Compute once and cache in the property list of *edcurve*
  ;; associating the list: (c s r) with the curve currently in force.
  (get-cached-symbol-data '*edcurve*
                          :elligator2-ab *edcurve*
                          'compute-elli2-ab))

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
        (with-mod *ed-q*
          (let* ((yv   (m* sqrt-c4dm1 yw))
                 (x    (m/ (m* -2 *ed-c* xu) yv))
                 (y    (m/ (m* *ed-c* (m+ 1 xu)) (m- 1 xu)))
                 (pt   (make-ecc-pt
                        :x  x
                        :y  y)))
            (assert (ed-satisfies-curve pt))
            pt))))))
              
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
  (let ((r (to-elligator-range r))) ;; mask off top random
    (declare (integer r))
    (cond  ((zerop r)  (ed-neutral-point))
           (t
            (with-mod *ed-q*
              (um:bind* (((sqrt-c4dm1 a b u) (elli2-ab))
                         (u*r^2   (m* u r r))
                         (1+u*r^2 (m+ 1 u*r^2)))
                
                ;; the following error can never trigger when fed with
                ;; r from elli2-encode. But random values fed to us
                ;; could cause it to trigger the error.
                
                (when (or (zerop 1+u*r^2)     ;; this could happen for r^2 = -1/u
                          (= (m* a a u*r^2)   ;; this can never happen: B=1 so RHS is square
                             (m* b 1+u*r^2 1+u*r^2)))  ;; and LHS is not square.
                  (error "invalid argument"))
                
                (let* ((v    (m- (m/ a 1+u*r^2)))
                       (eps  (mchi (m+ (m* v v v)
                                       (m* a v v)
                                       (m* b v))))
                       (xu   (m- (m* eps v)
                                 (m/ (m* (m- 1 eps) a) 2)))
                       (rhs  (m* xu
                                 (m+ (m* xu xu)
                                     (m* a xu)
                                     b)))
                       (yw   (m- (m* eps (msqrt rhs))))
                       ;; now we have (xu, yw) as per Bernstein: yw^2 = xu^3 + A*xu^2 + B*xu
                       ;; Now convert- to our Edwards coordinates:
                       ;;   (xu,yw) --> (x,y): x^2 + y^2 = c^2*(1 + d*x^2*y^2)
                       (yv   (m* sqrt-c4dm1 yw))
                       (x    (m/ (m* -2 *ed-c* xu) yv))
                       (y    (m/ (m* *ed-c* (m+ 1 xu)) (m- 1 xu)))
                       (pt   (ed-projective
                              (make-ecc-pt
                               :x  x
                               :y  y))))
                  #|
                  (assert (ed-satisfies-curve pt)) ;; true by construction
                  (assert (not (zerop (m* v eps xu yw)))) ;; true by construction
                  (assert (= (m* yw yw) rhs))     ;; true by construction
                  |#
                  pt
                  ))))
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
    (with-mod *ed-q*
      (um:bind* ((:struct-accessors ecc-pt (x y) (ed-affine pt))
                 ((sqrt-c4dm1 a b u) (elli2-ab))
                 (declare (ignore u))
                 (xu   (m/ (m- y *ed-c*) (m+ y *ed-c*)))
                 (yv   (m/ (m* -2 *ed-c* xu) x ))
                 (yw   (m/ yv sqrt-c4dm1)))
        (assert (= (m* yw yw)
                   (m+ (m* xu xu xu)
                       (m* a xu xu)
                       (m* b xu))))
        (make-ecc-pt
         :x xu
         :y yw)))))
             
(defun elli2-encode (pt)
  ;; Elligator2 mapping of pt to Zk
  ;; return Zk or nil
  (cond ((ed-neutral-point-p pt)
         (elligator-int-padding))
        (t
         (with-mod *ed-q*
           (um:bind* ((:struct-accessors ecc-pt (x y) (ed-affine pt))
                      ((sqrt-c4dm1 a b u) (elli2-ab))
                      (declare (ignore b))
                      ;; convert our Edwards coords to the form needed by Elligator-2
                      ;; for Montgomery curves
                      (xu   (m/ (m- y *ed-c*) (m+ y *ed-c*)))
                      (yv   (m/ (m* -2 *ed-c* xu) x ))
                      (yw   (m/ yv sqrt-c4dm1))
                      (xu+a (m+ xu a)))
             ;; now we have (x,y) --> (xu,yw) for:  yw^2 = xu^3 + A*xu^2 + B*xu
             (when (and (not (zerop xu+a))
                        (or (not (zerop yw))
                            (zerop xu))
                        (quadratic-residue-p (m- (m* u xu xu+a))))
               (let* ((e2    (if (quadratic-residue-p yw)
                                 (m/ xu xu+a)
                               (m/ xu+a xu)))
                      (enc   (msqrt (m/ e2 (m- u))))
                      (tau   (min enc (m- enc)))
                      ;; (ur2   (m* u tau tau))
                      ;; (1pur2 (m+ 1 ur2))
                      )
                 
                 ;; (assert (< tau (elligator-limit)))
                 #|
                  (when (zerop 1pur2) ;; never happens, by construction
                    (format t "~%Hit magic #1: tau = ~A" tau))
                  (when (= (m* a a ur2) ;; never happens, by construction
                           (m* b 1pur2 1pur2))
                    (format t "~%Hit magic #2: tau = ~A" tau))
                  
                  (unless (or (zerop 1pur2) ;; never happens, by construction
                              (= (m* a a ur2)
                                 (m* b 1pur2 1pur2)))
                    tau)
                  |#
                 (logior tau (elligator-int-padding))
                 )))))
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
             
