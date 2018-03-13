;; base58.lisp -- BTC-style base58 value encoding
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

(in-package :base58)

;; from https://bitcointalk.org/index.php?topic=1026.0

(defconstant +alphabet+
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
(defconstant +len+
  (length +alphabet+)) ;; should be 58

;; ----------------------------------------------------------

(defclass ub8v ()
  ((val :reader  ub8v-vec
        :initarg vec)))

;; ----------------------------------------------------------
;; Base58 encodes UB8 vectors and integers into character strings of
;; the restricted alphabet. Encoding has a 6-character prefix that
;; represents the total number of bytes in the vector, up to 2^32
;; elements.

(defclass base58 (ub8v)
  ((val  :reader   base58-str
         :initarg  :str)
   ))

(defun make-base58 (&key str)
  (make-instance 'base58
                 :str  str))

;; -----------------------------------------------------------
;; LEV-UB8 are little-endian vectors of UB8 elements

(defclass lev (ub8v)
  ((val  :reader   lev-vec
         :initarg  :vec)
   ))

(defun make-lev (&key vec)
  (make-instance 'lev
                 :vec vec))

;; -----------------------------------------------------------
;; BEV-UB8 are big-endian vectors of UB8 elements

(defclass bev (ub8v)
  ((val  :reader   bev-vec
         :initarg  :vec)
   ))

(defun make-bev (&key vec)
  (make-instance 'bev
                 :vec vec))

(defmethod print-object ((obj ub8v) out-stream)
  (format out-stream "#<~A ~A>"
          (class-name (class-of obj))
          (ub8v-vec obj)))

;; ---------------------------------------------------------
;; Encode to Base58 string

(defun sub-encode (val nc s)
  (um:nlet-tail iter ((v  val)
                      (ix 0))
    (when (or (plusp v)
              (and nc
                   (< ix nc)))
      (multiple-value-bind (vf vr) (floor v +len+)
        (princ (char +alphabet+ vr) s)
        (iter vf (1+ ix))))))

(defun convert-vec-to-int (vec)
  (let ((val 0))
    (loop for v across vec
          for pos from 0 by 8
          do
          (setf val (dpb v (byte 8 pos) val)))
    val))

;; ---------------------------------------------------

(defmethod to-base58 ((x lev))
  ;; encode vector in blocks of 512 bytes = 4096 bits = 700 chars of
  ;; Base58 encoding. Output has first 6 chars to encode 4-byte length
  ;; prefix.
  (let* ((v   (lev-vec x))
         (nb  (length v)))
    (make-base58
     :str (with-output-to-string (s)
            (sub-encode nb 6 s)
            (um:nlet-tail iter ((start 0))
              (unless (>= start nb)
                (let* ((end   (min nb (+ start 512)))
                       (chunk (subseq v start end)))
                  (sub-encode (convert-vec-to-int chunk)
                              (when (< end nb)
                                700)
                              s)
                  (iter end))))
            ))))

(defmethod to-base58 ((x bev))
  (to-base58 (to-lev x)))

(defmethod to-base58 ((val integer))
  (to-base58 (to-lev val)))

(defmethod to-base58 ((x base58))
  x)

;; -------------------------------------------------------------

(defun sub-decode (str)
  ;; decode little-endian base58 to integer value
  (let ((val  0))
    (loop for char across str
          for base = 1 then (* base +len+)
          do
          (let ((pos (position char +alphabet+)))
            (unless pos
              (error "Invalid base58 string: ~S" str))
            (incf val (* base pos))
            ))
    val))

(defun convert-int-to-vec (val)
  ;; convert val to little-endian vector of UB8
  (let* ((nb (ceiling (integer-length val) 8))
         (v  (make-array nb
                         :element-type '(unsigned-byte 8))))
    (loop for ix from 0 below nb
          for pos from 0 by 8
          do
          (setf (aref v ix) (ldb (byte 8 pos) val)))
    v))

;; ------------------------------------------------------

(defmethod to-lev ((v lev))
  v)

(defmethod to-lev ((x bev))
  (make-lev
   :vec (reverse (bev-vec x))))

(defmethod to-lev ((val integer))
  (make-lev
   :vec (convert-int-to-vec val)))

(defmethod to-lev ((x base58))
  (let* ((str  (base58-str x))
         (nb   (sub-decode (subseq str 0 6)))
         (nstr (length str))
         (vec  (make-array nb
                           :element-type '(unsigned-byte 8))))
    (um:nlet-tail iter ((start  6)
                        (vstart 0))
      (when (< start nstr)
        (let* ((end  (min nstr (+ start 700)))
               (vend (min nb (+ vstart 512)))
               (sub  (subseq str start end))
               (val  (sub-decode sub)))
          (loop for ix from vstart below vend
                for pos from 0 by 8
                do
                (setf (aref vec ix) (ldb (byte 8 pos) val)))
          (iter end vend))))
    (make-lev
     :vec vec)))

(defun to-levn (x nb)
  (let* ((lev (to-lev x))
         (nel (length (lev-vec lev))))
    (cond ((< nel nb)
           (let* ((diff (- nb nel))
                  (tail (make-array diff
                                    :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
             (make-lev
              :vec (concatenate 'vector (lev-vec lev) tail))))
          ((> nel nb)
           (make-lev
            :vec (subseq (lev-vec lev) 0 nb)))
          (t
           lev)
          )))

;; -----------------------------------------------------------

(defmethod to-bev ((x bev))
  x)

(defmethod to-bev ((val integer))
  (nto-bev (to-lev val)))

(defmethod to-bev ((x base58))
  (nto-bev (to-lev x)))

(defmethod to-bev ((x lev))
  (make-bev
   :vec (reverse (lev-vec x))))

(defmethod nto-bev ((x lev))
  (make-bev
   :vec (nreverse (lev-vec x))))

(defun to-bevn (x nb)
  (let* ((bev  (to-bev x))
         (nel  (length (bev-vec bev))))
    (cond ((< nel nb)
           (let* ((diff (- nb nel))
                  (pref (make-array diff
                                    :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
             (make-bev
              :vec (concatenate 'vector pref (bev-vec bev)))))
          ((> nel nb)
           (make-bev
            :vec (subseq (bev-vec bev) (- nel nb))))
          (t
           bev)
          )))

;; --------------------------------------------------------------

(defmethod to-int ((x integer))
  x)

(defmethod to-int ((x lev))
  (convert-vec-to-int (lev-vec x)))

(defmethod to-int ((x bev))
  (to-int (to-lev x)))

(defmethod to-int ((x base58))
  (to-int (to-lev x)))

;; -------------------------------------------------------------------
;; these compare operators are "bent" - doing just lexicographical
;; comparison, instead of magnitude comparison. Probably good enough
;; for most needs.

(defmethod ord:compare ((a base58) b)
  (ord:compare (base58-str a) (base58-str (to-base58 b))))

(defmethod ord:compare ((a lev) b)
  (ord:compare (lev-vec a) (lev-vec (to-lev b))))

(defmethod ord:compare ((a bev) b)
  (ord:compare (bev-vec a) (bev-vec (to-bev b))))

