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

(defpackage base58
  (:use :cl)
  (:export :encode
           :decode))

(in-package :base58)

;; from https://bitcointalk.org/index.php?topic=1026.0

(um:defconstant+ +alphabet+
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
(defconstant +len+
  (length +alphabet+)) ;; should be 58

(defun encode (val)
  ;; convert val to base58 representation of little-endian value
  (check-type val (integer 0))
  (with-output-to-string (s)
    (um:nlet-tail iter ((v  val))
      (when (plusp v)
        (multiple-value-bind (vf vr) (floor v +len+)
          (princ (char +alphabet+ vr) s)
          (iter vf))))
    ))

(defun decode (str)
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
