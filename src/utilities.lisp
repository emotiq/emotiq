;;;; utilities.lisp

(in-package #:emotiq)





;;;; Utilities

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector ()
  '(simple-array octet))

(defun octet-vector-p (thing)
  (typep thing 'octet-vector))

(defun make-octet-vector (length)
  (make-array length :element-type 'octet))

(defmacro ovref (octet-vector index)
  "Macro (get'able and setf'able) to access octet-vector at index."
  `(aref (the octet-vector ,octet-vector) ,index))
