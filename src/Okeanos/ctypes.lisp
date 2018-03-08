;; ctypes.lisp
;; --------------------------------------------------------------------------------------
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2008 Refined Audiometrics Laboratory, LLC

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
;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

(defun align-2 (val)
  (declare (type integer val))
  (align-pwr2 val 2))

(defun align-4 (val)
  (declare (type integer val))
  (align-pwr2 val 4))

(defun align-8 (val)
  (declare (type integer val))
  (align-pwr2 val 8))

(defun align-16 (val)
  (declare (type integer val))
  (align-pwr2 val 16))

(defun align-page (val)
  (declare (type integer val))
  (align-pwr2 val +page-size+))

;; --------------------------------------------------------------

(fli:define-c-typedef int8    :int8)
(fli:define-c-typedef uint8   :uint8)
(fli:define-c-typedef int16   :int16)
(fli:define-c-typedef uint16  :uint16)
(fli:define-c-typedef int32   :int32)
(fli:define-c-typedef uint32  :uint32)
(fli:define-c-typedef int64   :int64)
(fli:define-c-typedef uint64  :uint64)
(fli:define-c-typedef flt32   :float)
(fli:define-c-typedef flt64   :double)
(fli:define-c-typedef cmplx32 mmf:complex_t)
(fli:define-c-typedef cmplx64 mmf:dcomplex_t)

;; -------------------------------------------

(fli:define-c-typedef ssize_t uint16)
(fli:define-c-typedef size_t  uint32)
(fli:define-c-typedef off_t   uint64)

(fli:define-c-typedef uuid_t  mmf:uuid_t)
(fli:define-c-typedef oid_t   uuid_t)
(fli:define-c-typedef ts_t    uuid_t)

;; --------------------------------------------

(fli:define-c-union w16-union
  (u   uint16)
  (i   int16)
  (a   (:c-array uint8 2)))

(fli:define-c-union w32-union
  (u   uint32)
  (i   int32)
  (f   flt32)
  (a   (:c-array uint8 4)))


(fli:define-c-union w64-union
  (u   uint64)
  (i   int64)
  (d   flt64)
  (ulh (:c-array uint32 2))
  (a   (:c-array uint8 8)))

(fli:define-c-union w128-union
  (ulh (:c-array uint32 4))
  (a   (:c-array uint8 16)))


