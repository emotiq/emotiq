;; vec-repr.lisp -- Uniform and varied representation of UB8 Vectors
;;
;; DM/Emotiq 02/18
;; ----------------------------------------------------------------
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

(in-package :vec-repr)
;; ---------------------------------------------------------

;; This package describes interchangeable representations of vectors
;; of (unsigned-byte 8) values.
;;
;;            Class Hierarchy
;;            ---------------
;;
;;                 UB8V
;;                   |
;;      +-----+------+--------+-------+
;;      |     |      |        |       |
;;     LEV   BEV   BASE58   BASE64   HEX
;;
;; Class UB8V serves as an abstract superclass for all of these
;; parallel sub-classes. Any object of one of the subclasses can be
;; instantly converted into another parallel representation.
;;
;; Additionlly we make provision for conversion from bignum integers
;; to/from these vector representations.
;;
;; Finally, the Class UB8V-REPR is a mixin for future classes to use,
;; to indicate that they have the ability to produce a UB8V
;; representation, e.g., public keys, secret keys, compressed points
;; of Elliptic Curves, etc. This representation may be requested from
;; them with the UB8V-REPR method call.
;;
;; Type UB8 represents '(UNSIGNED-BYTE 8). Type UB8-VECTOR is defined
;; to represent any vector of actual UB8 values. Endian interpretation
;; is in the eye of the beholder.
;;
;; Each of the subclasses also has methods defined with the same name
;; as their class name, to perform conversions to their specific form
;; of representation. For example:
;;
;;    (base58 (lev #(1 2 3 4)))
;;  ==>
;;    #<BASE58 1111156wxj2 >
;;
;; These conversion operators may be applied to objects of any of the
;; parallel subclasses, as well as to INTEGER, LIST, and VECTOR. The
;; latter two must contain only elements of type UB8.
;;
;; The operators can also be applied to any class that inherits from
;; the mixin class UB8V-REPR and which implements a method by that
;; same name to return an instance of one of these parallel
;; subclasses. (c.f., PBC.LISP)

;; ----------------------------------------------------------
;; Declare Types UB8 and UB8-VECTOR

(deftype ub8 ()
  '(unsigned-byte 8))

(deftype ub8-vector (&optional nel)
  `(array ub8 (,nel)))

(defun make-ub8-vector (nel &rest args)
  (apply 'make-array nel
         :element-type 'ub8
         args))

;; ----------------------------------------------------------
;; from https://bitcointalk.org/index.php?topic=1026.0

(um:defconstant+ +alphabet-58+
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
(um:defconstant+ +len-58+
  (length +alphabet-58+)) ;; should be 58

(defun make-inverse-alphabet (alphabet)
  (let ((arr (make-ub8-vector 256
                         :initial-element 0)))
    (loop for c across alphabet
          for ix from 1
          do
          (setf (aref arr (char-code c)) ix))
    arr))
  
(defvar +inv-alphabet-58+
  (make-inverse-alphabet +alphabet-58+))

;; ----------------------------------------------------------

(um:defconstant+ +alphabet-64+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defvar +inv-alphabet-64+
  (make-inverse-alphabet +alphabet-64+))

;; ---------------------------------------------------------
;; UB8V - the top abstract class of objects which represent
;; UB8-VECTORs. Declare a single slot to hold the data. Object of
;; these classes are intended to be immuatable.  All subclasses share
;; this same slot.

(defclass ub8v ()
  ((val :reader  ub8v-vec
        :initarg :vec)))

;; ---------------------------------------------------------
;; Declare a useful mixin class to allow future classes to show a UB8V
;; representation

(defclass ub8v-repr ()
  ())

(defgeneric ub8v-repr (x)
  ;; subclasses of the mixin should return whatever representation
  ;; they need
  (:method ((x ub8v-repr))
   (error "Subclass responsibility")))

;; -----------------------------------------------

(defmethod print-object ((obj ub8v) out-stream)
  (if *print-readably*
      (progn
        (princ "#." out-stream)
        (prin1 `(,(class-name (class-of obj)) (hex-of-str ,(hex-str (hex obj))))
               out-stream))
    ;; else
    (format out-stream "#<~A ~A >"
            (class-name (class-of obj))
            (ub8v-vec obj))))

(defun hex-digit-char (c)
  (or (char<= #\0 #\9)
      (char<= #\a #\f)
      (char<= #\A #\F)))

(defun hex-of-str (str)
  (assert (every 'hex-digit-char str))
  (make-instance 'hex
                 :str str))

(defmethod print-object ((obj ub8v-repr) out-stream)
  ;; subclasses of UB8V-REPR mixin should feel free to override this
  (if *print-readably*
      (format out-stream "#.(make-instance '~W :value ~A)"
              (class-name (class-of obj))
              (with-output-to-string (s)
                (print-object (ub8v-repr obj) s)))
    ;; else
    (format out-stream "#<~A ~A >"
            (class-name (class-of obj))
            (ub8v-repr obj))))

;; ----------------------------------------------------------
;; Base58 encodes integers into character strings of the restricted
;; alphabet.

(defclass base58 (ub8v)
  ((val  :reader   base58-str
         :initarg  :str)
   ))

(defgeneric base58 (x))

(defmethod base58-str (x)
  (base58-str (base58 x)))

;; ----------------------------------------------------------
;; Base64 encodes UB8 vectors and integers into character strings of
;; the restricted alphabet. Encoding has a 6-character prefix that
;; represents the total number of bytes in the vector, up to 2^32
;; elements.

(defclass base64 (ub8v)
  ((val  :reader   base64-str
         :initarg  :str)
   ))

(defgeneric base64 (x))

(defmethod base64-str (x)
  (base64-str (base64 x)))

;; -----------------------------------------------------------
;; Hex-string representation, 1 char per 4-bit nibble

(defclass hex (ub8v)
  ((val  :reader hex-str
         :initarg :str)))

(defgeneric hex (x))

(defmethod hex-str (x)
  (hex-str (hex x)))

;; -----------------------------------------------------------
;; LEV-UB8 are little-endian vectors of UB8 elements

(defclass lev (ub8v)
  ((val  :reader   lev-vec
         :initarg  :vec)
   ))

(defgeneric lev (x))

(defmethod lev-vec (x)
  (lev-vec (lev x)))

;; -----------------------------------------------------------
;; BEV-UB8 are big-endian vectors of UB8 elements

(defclass bev (ub8v)
  ((val  :reader   bev-vec
         :initarg  :vec)
   ))

(defgeneric bev (x))

(defmethod bev-vec (x)
  (bev-vec (bev x)))

;; ---------------------------------------------------------
;; Integer to/from vector form - conversion starts at LSB and works
;; toward MSB. So FROR-END should be true for big-endian vectors, and
;; false for little-endian vectors.

(defun convert-vec-to-int (vec &key
                           (start    0)
                           (end      (length vec))
                           (from-end t))
  ;; from-end t is suitable for big-endian format
  (let ((val  0))
    (if from-end
        (do ((ix  (1- end)  (1- ix))
             (pos 0         (+ pos 8)))
            ((< ix start)  val)
          (setf val (dpb (aref vec ix) (byte 8 pos) val)))
      ;; else
      (do ((ix  start  (1+ ix))
           (pos 0      (+ 8 pos)))
          ((>= ix end)  val)
        (setf val (dpb (aref vec ix) (byte 8 pos) val)))
      )))

(defun convert-int-to-vec (val &key
                               (from-end t)
                               (nb  (max 1 (ceiling (integer-length val) 8))))
  ;; convert val to little-endian vector of UB8
  (let ((v  (make-ub8-vector nb
                             :initial-element 0)))
    (if from-end
        (do ((ix  (1- nb)  (1- ix))
             (pos 0        (+ 8 pos)))
            ((minusp ix)  v)
          (setf (aref v ix) (ldb (byte 8 pos) val)))
      ;; else
      (do ((ix  0   (1+ ix))
           (pos 0   (+ 8 pos)))
          ((>= ix nb)  v)
        (setf (aref v ix) (ldb (byte 8 pos) val)))
      )))

;; --------------------------------------------------------------
;; Integer conversions

(defmethod int ((x integer))
  x)

(defmethod int ((x lev))
  (convert-vec-to-int (lev-vec x)
                      :from-end nil))

(defmethod int ((x bev))
  (convert-vec-to-int (bev-vec x)))

(defmethod int (x)
  (int (bev x)))

(defmethod int ((x base58))
  (let* ((str  (base58-str x))
         (nstr (length str)))
    (um:nlet-tail iter ((pos    (1- nstr))
                        (val    0)
                        (base   1))
      (if (minusp pos)
          val
        (let* ((c  (char str pos))
               (v  (aref +inv-alphabet-58+ (char-code c))))
          (when (zerop v)
            (error "Invalid Base58 string: ~A" str))
          (iter (1- pos) (+ val (* base (1- v))) (* base +len-58+)))
        ))
    ))

;; ---------------------------------------------------
;; Base58 -- Integer value as Base58 string

(defmethod base58 ((x integer))
  ;; encode integer as a base58 string
  (make-instance 'base58 :str (integer-to-base58-with-pad x 0)))

(defmethod base58 ((x base58))
  x)

(defmethod base58 ((x vector))
  (let* ((length (length x))
         (int (int x))
         (npad
           ;; count the leading 0 bytes to pad:
           (loop for i from 0 below length
                 while (zerop (aref x i))
                 count t)))
    (make-instance
     'base58
     :str (integer-to-base58-with-pad int npad))))

(defun integer-to-base58-with-pad (x npad)
  "Return a string representation of integer X preceded by NPAD Base58
   encodings of 0 as leading zero 'padding'."
  (let ((cs nil))
    (um:nlet-tail iter ((v x))
                  (when (plusp v)
                    (multiple-value-bind (vf vr) (floor v +len-58+)
                      (push (char +alphabet-58+ vr) cs)
                      (iter vf))))
    (when npad
      (loop with char-for-0 = (char +alphabet-58+ 0)
            repeat npad
            do (push char-for-0 cs)))
    (coerce cs 'string)))

(defmethod base58 (x)
  (base58 (int x)))

;; -------------------------------------------------------------
;; Vector Conversions

(defmethod base64 ((x bev))
  ;; encode vector as a base64 (6-bit) string
  (let* ((v   (bev-vec x))
         (nb  (length v))
         (ns  (* 4 (ceiling nb 3)))
         (str (make-string ns)))
    (um:nlet-tail iter ((start  0)
                        (vstart 0)
                        (val    0)
                        (ct     0))
      (labels
          ((enc (pos)
             (setf (char str (+ start pos))
                   (aref +alphabet-64+ (ldb (byte 6 (* 6 (- 3 pos))) val)))
             ))
        (when (>= ct 3)
          (enc 0)
          (enc 1)
          (enc 2)
          (enc 3)
          (setf val 0
                ct  0
                start (+ start 4)))
        (if (< vstart nb)
            (iter start (1+ vstart)
                  (dpb (aref v vstart) (byte 8 (* (- 2 ct) 8)) val)
                  (1+ ct))
          (progn
            (case ct
              (1  (enc 0)
                  (enc 1)
                  (setf (char str (+ 2 start)) #\=
                        (char str (+ 3 start)) #\=))
              (2  (enc 0)
                  (enc 1)
                  (enc 2)
                  (setf (char str (+ 3 start)) #\=)))
            (make-instance 'base64
                           :str str))
          )))))

(defmethod base64 ((x base64))
  x)

(defmethod base64 (x)
  (base64 (bev x)))

;; -------------------------------------------------------------

(defmethod hex ((x bev))
  ;; encode vector as a hex string 
  (let* ((v   (bev-vec x))
         (nb  (length v))
         (str (make-string (* 2 nb))))
    (labels ((enc (val)
               (code-char
                (+ val
                   (if (< val 10)
                       #.(char-code #\0)
                     #.(- (char-code #\a) 10))))
               ))
    (loop for bx from (1- nb) downto 0
          for b = (aref v bx)
          for ix = (* 2 bx)
          do
          (setf (char str ix)      (enc (ldb '#.(byte 4 4) b))
                (char str (1+ ix)) (enc (ldb '#.(byte 4 0) b))))
    (make-instance 'hex
                   :str str))))

(defmethod hex ((x hex))
  x)

(defmethod hex (x)
  (hex (bev x)))

;; ------------------------------------------------------
;; LEV -- encode vector as little-endian (LSB first)

(defmethod lev ((x lev))
  x)

(defmethod lev ((x bev))
  (make-instance 'lev
   :vec (reverse (bev-vec x))))

(defmethod lev ((val integer))
  (make-instance 'lev
   :vec (convert-int-to-vec val :from-end nil)))

(defmethod lev ((x sequence))
  ;; LIST and VECTOR when can be coerced to UB8-VECTOR
  (make-instance 'lev
                 :vec (coerce x 'ub8-vector)))

(defmethod lev ((x ub8v-repr))
  (lev (ub8v-repr x)))

(defmethod lev (x)
  (make-instance 'lev
                 :vec (nreverse
                       (bev-vec (bev x)))))

(defun levn (x nb)
  ;; create a LEV with a specified number of UB8 bytes
  (let* ((lev (lev x))
         (nel (length (lev-vec lev))))
    (cond ((< nel nb)
           ;; extend with zero filled tail
           (let* ((diff (- nb nel))
                  (tail (make-ub8-vector diff
                                    :initial-element 0)))
             (make-instance 'lev
                            :vec (concatenate 'ub8-vector (lev-vec lev) tail))))
          ((> nel nb)
           ;; take from the LSB side
           (make-instance 'lev
                          :vec (subseq (lev-vec lev) 0 nb)))
          (t
           lev)
          )))

;; -----------------------------------------------------------
;; BEV -- encode vector as big-endian (MSB first)

(defmethod bev ((x bev))
  x)

(defmethod bev ((x ub8v-repr))
  (bev (ub8v-repr x)))

(defmethod bev ((x lev))
  ;; conversion of LEV to BEV
  (make-instance 'bev
                 :vec (reverse (lev-vec x))))

(defmethod bev ((x sequence))
  ;; LIST and VECTOR when can be coerced to UB8-VECTOR
  (make-instance 'bev
                 :vec (coerce x 'ub8-vector)))

(defmethod bev ((x integer))
  ;; convert integer to BEV vector
  (make-instance 'bev
                 :vec (convert-int-to-vec x :from-end t)))

(defmethod bev ((x base58))
  ;; convert Base58 string to BEV vector
  (bev (int x)))

(defmethod bev ((x base64))
  ;; convert Base64 string to BEV vector
  (let* ((str  (base64-str x))
         (nstr (length str))
         (vlen 0)
         (lst  (um:accum acc
                 (um:nlet-tail iter ((start  0)
                                     (val    0)
                                     (ct     0))
                   (labels ((decode (pos)
                              (acc (ldb (byte 8 (* 8 (- 2 pos))) val))
                              (incf vlen)))
                     (when (>= ct 4)
                       (decode 0)
                       (decode 1)
                       (decode 2)
                       (setf val 0
                             ct  0))
                     (if (< start nstr)
                         (let ((c (char str start)))
                           (unless (char= c #\=)
                             (let ((cv  (aref +inv-alphabet-64+ (char-code c))))
                               (when (zerop cv)
                                 (error "Invalid Base64 string: ~S" str))
                               (setf val (dpb (1- cv) (byte 6 (* 6 (- 3 ct))) val))
                               (incf ct)))
                           (iter (1+ start) val ct))
                       ;; else
                       (case ct
                         (2  (decode 0))
                         (3  (decode 0)
                             (decode 1)))
                       ))))))
    (make-instance 'bev
                   :vec (make-ub8-vector vlen
                                         :initial-contents lst))
    ))

(defmethod bev ((x hex))
  ;; convert hex string to BEV vector
  (let* ((str  (hex-str x))
         (ns   (length str))
         (vec  (make-ub8-vector (ceiling ns 2))))
    (labels ((decode (ch)
               (cond ((char<= #\0 ch #\9) (- (char-code ch) #.(char-code #\0)))
                     ((char<= #\A ch #\F) (- (char-code ch) #.(- (char-code #\A) 10)))
                     ((char<= #\a ch #\f) (- (char-code ch) #.(- (char-code #\a) 10)))
                     (t
                      (error "Invalid Hex string: ~S" str))
                     )))
      (um:nlet-tail iter ((vx  0)
                          (sx  0)
                          (val 0)
                          (ct  0))
        (when (>= ct 2)
          (setf (aref vec vx) val
                ct  0
                val 0)
          (incf vx))
        (when (< sx ns)
          (iter vx (1+ sx)
                (+ (ash val 4)
                   (decode (aref str sx)))
                (1+ ct))))
      (make-instance 'bev
                     :vec vec))))

(defun bevn (x nb)
  ;; construct a BEV with a specified number of UB8 bytes
  (let* ((bev  (bev x))
         (nel  (length (bev-vec bev))))
    (cond ((< nel nb)
           ;; prepend with zero filled prefix
           (let* ((diff (- nb nel))
                  (pref (make-ub8-vector diff
                                    :initial-element 0)))
             (make-instance 'bev
                            :vec (concatenate 'ub8-vector pref (bev-vec bev)))))
          ((> nel nb)
           ;; take a portion from the LSB side
           (make-instance 'bev
                          :vec (subseq (bev-vec bev) (- nel nb))))
          (t
           bev)
          )))

(defun int= (a b)
  "Poor way to do equality testing on UB8V items. This method fails to
account for vectors with leading null bytes. Okay for comparing
compressed ECC points - public keys, signatures. Not okay for hash
comparisons."
  (= (int a) (int b)))

(defun vec= (a b)
  "Easy way to do equality testing on UB8V items"
  (equalp (bev-vec a) (bev-vec b)))
