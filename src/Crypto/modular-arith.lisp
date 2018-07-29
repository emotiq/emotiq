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

(defun mod-base ()
  *m*)

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
  (let ((ans (mmod arg)))
    (declare (integer ans))
    (dolist (opnd args)
      (declare (integer opnd))
      (setf ans (mmod (* ans opnd))))
    ans))

(defun msqr (x)
  (declare (integer x))
  (m* x x))

;; ------------------------------------------------------------

(defun m+ (&rest args)
  (mmod (apply '+ args)))

(defun m- (&rest args)
  (mmod (apply '- args)))

;; -----------------------------------------------------
;; Precomputed powers cache

(defclass window-cache ()
  ((precv  :reader   window-cache-precv ;; precomp x^n cache
           :initarg  :precv)
   (x^1    :reader   window-cache-x^1
           :initarg  :x^1)
   (op-sqr :reader   window-cache-op-sqr
           :initarg  :op-sqr)
   (op-mul :reader   window-cache-op-mul
           :initarg  :op-mul)))

(defun make-window-cache (&key nbits x^1 op-mul op-sqr)
  (let ((precv (make-array (ash 1 nbits) :initial-element nil)))
    (setf (aref precv 1) x^1) ;; slot 0 never accessed
    (make-instance 'window-cache
                   :precv  precv
                   :x^1    x^1
                   :op-sqr op-sqr
                   :op-mul op-mul)))

(defmethod get-prec ((wc window-cache) (ix integer))
  ;; compute powers of x^n, n = 1..(2^nbits-1) on demand
  (declare (fixnum ix))
  (with-accessors ((precv  window-cache-precv)
                   (x^1    window-cache-x^1)
                   (op-mul window-cache-op-mul)
                   (op-sqr window-cache-op-sqr)) wc
    (or (aref precv ix)
        (setf (aref precv ix)
              (if (oddp ix)
                  (funcall op-mul x^1 (get-prec wc (1- ix)))
                (funcall op-sqr (get-prec wc (ash ix -1))))
              ))))

;; -----------------------------------------------------
;; Generalized fixed-window exponentiation algorithm
;;
;; Used by both modular exponentiation, and Cipolla algorithm for
;; modular square roots.

(defmethod generalized-windowed-exponentiation (x n &key window-nbits op-mul op-sqr)
  ;; modular x^n using fixed-width window algorithm
  (let* ((ans   nil)
         (wc    (make-window-cache
                 :nbits  window-nbits
                 :x^1    x
                 :op-mul op-mul
                 :op-sqr op-sqr))
         (nbits (integer-length n)))
    (declare (fixnum nbits))
    (loop for pos fixnum from (* window-nbits (floor nbits window-nbits)) downto 0 by window-nbits do
          (when ans
            (loop repeat window-nbits do
                  (setf ans (funcall op-sqr ans))))
          (let ((bits (ldb (byte window-nbits pos) n)))
            (declare (fixnum bits))
            (unless (zerop bits)
              (let ((y (get-prec wc bits)))
                (setf ans (if ans
                              (funcall op-mul ans y)
                            y)))
              )))
    ans))
    
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
      (generalized-windowed-exponentiation x exp
                                           :window-nbits 4
                                           :op-mul       'm*
                                           :op-sqr       'msqr))
    ))

;; ------------------------------------------------------------

(defun minv (a)
  ;; modular inverse by Extended Euclidean algorithm
  (declare (integer a))
  (let* ((u  (mmod a))
         (v  *m*)
         (x1 1)
         (x2 0))
    (declare (integer u v x1 x2))
    (do ()
        ((= u 1) (mmod x1))
      (multiple-value-bind (q r) (truncate v u)
        (declare (integer q r))
        (let ((x (- x2 (* q x1))))
          (declare (integer x))
          (shiftf v u r)
          (shiftf x2 x1 x))
        ))))

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
    (declare (integer re im^2))
    (labels
        ;; complex multiplication over the field Fq^2
        ((fq2* (a b)
           (destructuring-bind (are . aim) a
             (declare (integer are aim))
             (destructuring-bind (bre . bim) b
               (declare (integer bre bim))
               (cons
                (m+ (m* are bre)
                    (m* aim bim im^2))
                (m+ (m* are bim)
                    (m* aim bre)))
               )))
         
         (fq2sqr (a)
           (destructuring-bind (are . aim) a
             (declare (integer are aim))
             (cons
              (m+ (m* are are)
                  (m* aim aim im^2))
              (m* 2 are aim)))))
      
      (car (generalized-windowed-exponentiation (cons re 1) (m/2u)
                                                :window-nbits  4
                                                :op-mul        #'fq2*
                                                :op-sqr        #'fq2sqr))
      )))

(defun get-msqrt-fn (base)
  (get-cached-symbol-data '*m* :msqrt base
                          (lambda ()
                            (cond
                             ((= 3 (mod base 4))
                              (let ((p (truncate (1+ base) 4)))
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


