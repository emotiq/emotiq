;; ctypes.lisp
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

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


