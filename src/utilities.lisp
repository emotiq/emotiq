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




;;;; Normalizing Strings as Base Strings and Simple Strings

(defun normalize-to-base-string (string)
  "If STRING is not a base string, return a copy (via
   copy-as-base-string) that is of type SIMPLE-BASE-STRING; otherwise,
   return STRING itself. It is an error for STRING to contain
   non-BASE-CHAR characters."
  (etypecase string
    (base-string string)
    (string (copy-as-base-string string))))

(defun copy-as-base-string (string)
  "Return a freshly consed string of type SIMPLE-BASE-STRING, i.e., an
  array with element-type BASE-CHAR, of the same (active) length as
  and with all the same (active) elements as STRING copied over to the
  same positions. It is an error for STRING to contain non-BASE-CHAR
  characters."
  (make-array
   (length string)
   :initial-contents string 
   :element-type 'base-char))



(defun normalize-to-simple-string (string)
  "If STRING is either not a simple string or is of a proper subtype
   of SIMPLE-STRING, return a copy that is of type simple-string;
   otherwise, return STRING itself."
  (etypecase string
    (base-string (copy-as-simple-string string))
    (simple-string string)      
    (string (copy-as-simple-string string))))

(defun copy-as-simple-string (string)
  "Return a freshly consed string of type SIMPLE-STRING, i.e., an
  array with element-type CHARACTER, of the same (active) length as
  and with all the same (active) elements as STRING copied over to the
  same positions."
  (make-array
   (length string)
   :initial-contents string 
   :element-type 'character))
