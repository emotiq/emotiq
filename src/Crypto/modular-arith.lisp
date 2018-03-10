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
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 0) #+:LISPWORKS (FLOAT 0)))

;; -----------------------------------------------------

(defvar *m*  1)   ;; current modular base

(declaim (integer *m*)
         (inline mmod m-1 m/2l m+1 m/2u))

(defmacro with-mod (base &body body)
  `(let ((*m* ,base))
     ,@body))

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

;; -----------------------------------------------------
;; Prime-Field Arithmetic

(defun m^ (base exponent)
  ;; base^exponent mod modulus, for any modulus
  (declare (integer base exponent))
  (let ((x (mmod base)))
    (declare (integer x))
    (if (< x 2)  ;; x = 0,1
        x
      ;; else
      (let* ((exp (+ exponent (get-blinder (m-1))))
             (n   (integer-length exp)))
        (declare (fixnum n)
                 (integer exp))
        (do ((b  (+ x (get-blinder))
                 (m* b b))
             (p  1)
             (ix 0    (1+ ix)))
            ((>= ix n) p)
          (declare (integer b p)
                   (fixnum ix))
          (when (logbitp ix exp)
            (setf p (m* p b)))) ))
    ))

;; ------------------------------------------------------------

(defun mchi (x)
  ;; chi(x) -> {-1,+1}
  ;; = +1 when x is square residue
  (m^ x (m/2l)))

(defun quadratic-residue-p (x)
  ;; aka Legendre Symbol (x|m)
  (= 1 (mchi x)))

(defvar *fq2-red*)

(defstruct fq2
  x y)

(defun fq2* (a b)
  (um:bind* ((:struct-accessors fq2 ((ax x) (ay y)) a)
             (:struct-accessors fq2 ((bx x) (by y)) b))
    #-(or ccl)
    #|
While compiling FQ2* :
The DECLARE expression (DECLARE
                        (INTEGER
                         AX
                         BX
                         AY
                         BY)) is being treated as a form,
possibly because it's the result of macroexpansion. DECLARE expressions
can only appear in specified contexts and must be actual subexpressions
of the containing forms.
   [Condition of type CCL::COMPILE-TIME-PROGRAM-ERROR]
    |#
    (declare (integer ax bx ay by))
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
                      (declare (integer a))
                      (let ((v  (mmod (- (* a a) x))))
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
         (p  (make-fq2
              :x 1
              :y 0))
         (ix 0    (1+ ix)))
        ((>= ix n) (fq2-x p))
      (declare (fixnum ix))
      (when (logbitp ix exp)
        (setf p (fq2* p b))))
    ))

(defun get-msqrt-fn ()
  (get-cached-symbol-data '*m* :msqrt *m*
                          (lambda ()
                            (cond
                             ((= 3 (mod *m* 4))
                              (let ((p  (truncate (m+1) 4)))
                                (um:rcurry 'm^ p)))
                             (t 'cipolla)))))

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
      (let ((ix (isqrt xx)))
        (declare (integer ix))
        (cond ((= xx (* ix ix)) ix)
              ((quadratic-residue-p xx)
               (let* ((fn (get-msqrt-fn))
                      (xr (funcall fn xx)))
                 (declare (integer xr))
                 (assert (= xx (m* xr xr)))
                 xr))
              
              (t (error "not a square"))
              )))
    ))

;; ------------------------------------------------------------

(defun m* (arg &rest args)
  (declare (integer arg))
  (let* ((blinder (get-blinder)))
    (declare (integer blinder))
    (dolist (opnd args arg)
      (declare (integer opnd))
      (setf arg (mmod (* arg (+ opnd blinder)))))
    ))

;; ------------------------------------------------------------

(defun m+ (arg &rest args)
  (declare (integer arg))
  (let* ((blinder (get-blinder))
         (ans     (+ arg blinder)))
    (declare (integer blinder ans))
    (dolist (opnd args)
      (declare (integer opnd))
      (incf ans (+ opnd blinder)))
    (mmod ans)))

(defun m- (arg &rest args)
  (declare (integer arg))
  (let* ((blinder (get-blinder))
         (ans     (- arg blinder)))
    (declare (integer blinder ans))
    (if args
        (dolist (opnd args)
          (declare (integer opnd))
          (decf ans (+ opnd blinder)))
      (setf ans (- ans)))
    (mmod ans)))

;; ------------------------------------------------------------

(defun minv (a)
  (declare (integer a))
  (let* ((u  (mmod a))
         (v  *m*)
         (x1 1)
         (x2 0))
    (declare (integer u v x1 x2))
    (do ()
        ((= u 1) x1)
      (multiple-value-bind (q r) (truncate v u)
        (declare (integer q r))
        (let ((x (m- x2 (m* q x1))))
          (declare (integer x))
          (shiftf v u r)
          (shiftf x2 x1 x)) ))))


(defun m/ (arg &rest args)
  (declare (integer arg))
  (dolist (opnd args
                (if args arg (minv arg)))
    (declare (integer opnd))
    (setf arg (m* arg (minv opnd)))))

