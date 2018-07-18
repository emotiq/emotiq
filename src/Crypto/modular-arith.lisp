;; modular-arith.lisp -- Prime Field Arithmetic
;; DM/RAL 03/18
;; -----------------------------------------------------
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

(in-package :crypto/modular-arith)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) #|(SAFETY 0)|# #+:LISPWORKS (FLOAT 0)))

;; -----------------------------------------------------

(defvar *m*  1)   ;; current modular base

(declaim (integer *m*)
         (inline mmod m-1 m/2l m+1 m/2u))

#|
;; this is not faster than the built-in MOD function...

(defstruct moddescr
  nbits rem)

(defun make-mod-descr (base)
  (let* ((nbits  (integer-length base))
         (rem    (- (ash 1 nbits) base)))
    (make-moddescr
     :nbits nbits
     :rem   rem)))

(defvar *fastmod*  nil)

(defun fastmod (x)
  ;; full mod
  (declare (integer x))
  (let* ((nbits  (moddescr-nbits *fastmod*))
         (rem    (moddescr-rem   *fastmod*))
         (mnbits (- nbits))
         (sgn    (minusp x)))
    (declare (integer rem)
             (fixnum  nbits mnbits))
    (labels ((ret (x)
               (declare (integer x))
               (cond ((zerop x) 0)
                     (sgn       (- *m* x))
                     (t         x))))
      (um:nlet-tail iter ((v (abs x)))
        (declare (integer v))
        (if (< v *m*)
            (ret v)
          ;; else
          (let ((ve (ash v mnbits)))
            (declare (integer ve))
            (if (zerop ve)
                (ret (- v *m*))
              (let ((vf (ldb (byte nbits 0) v)))
                (declare (integer vf))
                (iter (+ vf (* rem ve)))
                )))))
      )))

(defun get-fastmod (base)
  (get-cached-symbol-data '*m* :fastmod base
                          (lambda ()
                            (make-mod-descr base))))

(defmacro with-mod (base &body body)
  `(let* ((*m*       ,base)
          (*fastmod* (get-fastmod *m*)))
     ,@body))

(defun mmod (x)
  (declare (integer x))
  ;; (mod x *m*)
  (fastmod x))

(defun m! (m)
  ;; for REPL convenience, so we don't have to keep doing WITH-MOD
  (check-type m (integer 1))
  (setf *m*       m
        *fastmod* (get-fastmod m)))
|#

(defmacro with-mod (base &body body)
  `(let ((*m*  ,base))
     ,@body))

#|
(with-mod 13
  (print (fastmod 43))
  (print (fastmod -43)))
 |#

#+:LISPWORKS
(editor:setup-indent "with-mod" 1)

(defun mmod (x)
  (declare (integer x))
  (mod x *m*))

(defun m-1 ()
  (1- *m*))

(defun m/2l ()
  (truncate (m-1) 2))

(defun m+1 ()
  (1+ *m*))

(defun m/2u ()
  (truncate (m+1) 2))

(defun m! (m)
  ;; for REPL convenience, so we don't have to keep doing WITH-MOD
  (check-type m (integer 1))
  (setf *m* m))

;; -----------------------------------------------------

#|
(defvar *blinders* (make-hash-table))

(defun create-blinder (m)
  (declare (integer m))
  (* m (ecc-crypto-b571:random-between #.(ash 1 31) #.(ash 1 32))))

(defun get-blinder (&optional (m *m*))
  (declare (integer m))
  (or 0 ;; no blinding...
      (gethash m *blinders*)
      (setf (gethash m *blinders*) (create-blinder m))))

(defun reset-blinders ()
  (clrhash *blinders*))
|#

;; ------------------------------------------------------------

(defun m= (a b)
  (= (mmod a) (mmod b)))

;; ------------------------------------------------------------

(defun m* (arg &rest args)
  (declare (integer arg))
  (dolist (opnd args)
    (declare (integer opnd))
    (setf arg (mmod (* arg opnd))))
  arg)
    
;; ------------------------------------------------------------

(defun m+ (&rest args)
  (mmod (apply '+ args)))

(defun m- (&rest args)
  (mmod (apply '- args)))

;; -----------------------------------------------------
;; Prime-Field Arithmetic

(defun m^ (base exp)
  ;; base^exponent mod modulus, for any modulus
  ;; use a 4-bit fixed window algorithm
  (declare (integer base exp))
  (let ((x (mmod base)))
    (declare (integer x))
    (if (< x 2)  ;; x = 0,1
        x
      ;; else
      (let* ((n     (integer-length exp))
             (prec  (coerce
                     (loop for ix fixnum from 0 below 16
                           for xx = 1 then (m* x xx)
                           collect xx)
                     'vector))
             (ans   1))
        (declare (fixnum n)
                 (integer ans)
                 ((vector integer) prec))
        (loop for pos fixnum from (* 4 (floor n 4)) downto 0 by 4 do
              (setf ans (m* ans ans)
                    ans (m* ans ans)
                    ans (m* ans ans)
                    ans (m* ans ans))
              (let ((bits (ldb (byte 4 pos) exp)))
                (declare (fixnum bits))
                (unless (zerop bits)
                  (setf ans (m* ans (aref prec bits))))
                ))
        ans))))

#|
(defun m^ (base exp)
  ;; base^exponent mod modulus, for any modulus
  (declare (integer base exp))
  (let ((x (mmod base)))
    (declare (integer x))
    (if (< x 2)  ;; x = 0,1
        x
      ;; else
      (let* ((n  (integer-length exp)))
        (declare (fixnum n)
                 (integer exp))
        (do ((b  x  (m* b b))
             (p  1)
             (ix 0  (1+ ix)))
            ((>= ix n) (mmod p))
          (declare (integer b p)
                   (fixnum ix))
          (when (logbitp ix exp)
            (setf p (m* p b)))) ))
    ))
|#

;; ------------------------------------------------------------

(defun minv (a)
  ;; modular inverse by Extended Euclidean algorithm
  (declare (integer a))
  (let* ((u  (mmod a))
         (v  *m*)
         (x1 1)
         (x2 0))
    (declare (integer u v x1 x2))
    (do ((ct 1 (1+ ct)))
        ((= u 1) (values (mmod x1) ct))
      (multiple-value-bind (q r) (truncate v u)
        (declare (integer q r))
        (let ((x (- x2 (* q x1))))
          (declare (integer x))
          (shiftf v u r)
          (shiftf x2 x1 x))
        ))))

#|
(defun minv (a)
  ;; modular inverse by Extended Euclidean algorithm
  (declare (integer a))
  (let* ((u  (mmod a))
         (v  *m*)
         (x1 1)
         (x2 0))
    (declare (integer u v x1 x2))
    (do ((ct 1 (1+ ct)))
        ((= u 1) (values x1 ct))
      (multiple-value-bind (q r) (truncate v u)
        (declare (integer q r))
        (let ((x (m- x2 (m* q x1))))
          (declare (integer x))
          (shiftf v u r)
          (shiftf x2 x1 x))
        ))))
|#

(defun m/ (arg &rest args)
  (declare (integer arg))
  (if args
      (m* arg (minv (apply 'm* args)))
    (minv arg)))

;; ------------------------------------------------------------

(defun mchi (x)
  ;; chi(x) -> {-1,0,+1}
  ;; = +1 when x is square residue
  ;; =  0 when x = 0
  ;; = -1 when x is non-square
  (m^ x (m/2l)))

(defun quadratic-residue-p (x)
  ;; aka Legendre Symbol (x|m)
  (= 1 (mchi x)))

(defun fast-cipolla (x)
  ;; Cipolla method for finding square root of x over prime field m
  ;; use fixed 4-bit window evaluation
  ;;
  ;; Cipolla defines a quadratic extnsion field, where every value in
  ;; Fq^2 is a square, albeit possibly "imaginary". If a value is a
  ;; square in Fq then it has zero imaginary component in its square
  ;; root in Fq^2. Otherwise, it has zero real part and finite
  ;; imaginary part.
  ;;
  ;; This routine will work happily on every field value in Fq, but it
  ;; only returns the real part of the result, which will be zero for
  ;; Fq non-squares.
  ;;
  (declare (integer x))
  (multiple-value-bind (re im^2)
      (um:nlet-tail iter ((a  2))
        ;; look for quadratic nonresidue (the imaginary base)
        ;; where we already know that x must be a quadratic residue
        (declare (integer a))
        (let ((v  (m- (m* a a) x)))
          (declare (integer v))
          (if (quadratic-residue-p v)
              (iter (1+ a))
            (values a v))
          ))
    (labels ((fq2* (a b)
               ;; complex multiplication over the field q^2
               (destructuring-bind (are aim) a
                 (destructuring-bind (bre bim) b
                   (list
                    (m+ (m* are bre)
                        (m* aim bim im^2))
                    (m+ (m* are bim)
                        (m* aim bre)))
                   )))
             (fq2sqr (a)
               (destructuring-bind (are aim) a
                 (list
                  (m+ (m* are are) (m* aim aim im^2))
                  (m* 2 are aim)))))
      
      ;; exponentiation in Fq^2: (a, sqrt(a^2 - x))^((q-1)/2)
      (let* ((xx   (list re 1))  ;; generator for Fq^2
             (exp  (m/2u))
             (n    (integer-length exp))
             (prec (coerce
                    (cons nil
                          (loop for ix from 1 below 16
                                for v = xx then (fq2* xx v)
                                collect v))
                    'vector))
             (ans   nil))
        
        (loop for pos from (* 4 (floor n 4)) downto 0 by 4 do
              (when ans
                (setf ans (fq2sqr ans)
                      ans (fq2sqr ans)
                      ans (fq2sqr ans)
                      ans (fq2sqr ans)))
              (let ((ix (ldb (byte 4 pos) exp)))
                (declare (fixnum ix))
                (unless (zerop ix)
                  (let ((v  (aref prec ix)))
                    (setf ans (if ans
                                  (fq2* ans v)
                                v))))
                ))
        (car ans)
        ))))

#|
(defvar *fq2-red*)

(defstruct fq2
  x y)

(defun fq2* (a b)
  ;; complex multiplication over the field q^2
  (um:bind* ((:struct-accessors fq2 ((ax x) (ay y)) a)
             (declare (integer ax ay))
             (:struct-accessors fq2 ((bx x) (by y)) b)
             (declare (integer bx by)))
    (make-fq2
     :x (m+ (m* ax bx)
            (m* ay by (cadr *fq2-red*)))
     :y (m+ (m* ax by)
            (m* ay bx))
     )))

(defun cipolla (x)
  (declare (integer x))
  ;; Cipolla method for finding square root of x over prime field m
  (let* ((*fq2-red* (um:nlet-tail iter ((a  2))
                      ;; look for quadratic nonresidue (the imaginary base)
                      ;; where we already know that x must be a quadratic residue
                      (declare (integer a))
                      (let ((v  (m- (m* a a) x)))
                        (declare (integer v))
                        (if (quadratic-residue-p v)
                            (iter (1+ a))
                          (list a v))
                        )))
         ;; exponentiation in Fq^2: (a, sqrt(a^2 - n))^((q-1)/2)
         (xx   (make-fq2
                :x (car *fq2-red*)
                :y 1))
         (exp  (m/2u))
         (n    (integer-length exp)))
    (declare (fixnum n)
             (integer exp))
    (do ((b  xx
             (fq2* b b))
         (p  (make-fq2 ;; start with complex unity
              :x 1
              :y 0))
         (ix 0    (1+ ix)))
        ((>= ix n) (fq2-x p))
      (declare (fixnum ix))
      (when (logbitp ix exp)
        (setf p (fq2* p b))))
    ))
|#

(defun get-msqrt-fn (base)
  (get-cached-symbol-data '*m* :msqrt base
                          (lambda ()
                            (cond
                             ((= 3 (mod base 4))
                              (let ((p (truncate (m+1) 4)))
                                (um:rcurry 'm^ p)))
                             #|
                             ((= 5 (mod base 8))
                              (let ((p (truncate (+ base 3) 8)))
                                  (um:rcurry 'm^ p)))
                             |#
                             (t 'fast-cipolla)))))

(defun msqrt (x)
  ;; assumes m is prime
  ;; a^(m-1) = 1 for m prime
  ;; a^m = a
  ;; a^(m+1) = a^2
  ;; a^((m+1)/4) = a^(1/2) -- works nicely when m = 3 mod 4
  ;; 1/2 = 2/4 = 3/6 = 4/8 = 5/10 = 6/12 = 7/14 = 8/16
  ;; in general:  for m = (2k+1) mod 4k, use (m + (2k-1))/4k, k = 1,2,...
  (declare (integer x))
  (let ((xx  (mmod x)))
    (declare (integer xx))
    (if (< xx 2)
        xx
      ;; else
      (cond ((let ((ix (isqrt xx)))
               (declare (integer ix))
               (and (= xx (* ix ix))
                    ix)))

            ((quadratic-residue-p xx)
             (funcall (get-msqrt-fn *m*) xx))
            
            (t (error "not a square"))
            ))
    ))

;; -----------------------------------------------------------


