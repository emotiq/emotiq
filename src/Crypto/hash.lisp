;; hash.lisp -- Standardized hashing for crypto needs
;;
;; DM/Emotiq 03/18
;; ---------------------------------------------------------
#|
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

(in-package :hash)

;; -------------------------------------------------

(defclass hash (ub8v-repr)
  ((val :reader  hash-val
        :initarg :val)))

(defclass hash/ripemd/160 (hash)
  ())

(defclass hash/sha2/256 (hash)
  ())

(defclass hash/256 (hash)
  ())

(defclass hash/384 (hash)
  ())

(defclass hash/512 (hash)
  ())

;; -------------------------------------------------

(defmethod ub8v-repr ((x hash))
  (hash-val x))

(defmethod hash-bytes ((x hash))
  (bev-vec (bev (hash-val x))))

(defmethod hash-length ((x hash))
  (length (hash-bytes x)))

;; -------------------------------------------------
;; what to hash of various types

(defgeneric hashable (x)
  (:method ((x ub8v))
   (bev-vec (bev x)))
  (:method ((x ub8v-repr))
   (hashable (ub8v-repr x)))
  (:method ((x integer))
   (hashable (bev x)))
  (:method ((x sequence))
   (or (ignore-errors
         (coerce x 'ub8-vector))
       (call-next-method)))
  (:method ((x string))
   (hashable (map 'vector 'char-code x)))
  (:method ((x symbol))
   (hashable (symbol-name x)))
  (:method ((x pathname))
   (hashable (namestring x)))
  (:method (x)
   (loenc:encode x)))

;; -------------------------------------------------

(defun local-digest (dig-kind dig-class &rest args)
  (let ((dig (ironclad:make-digest dig-kind)))
    (dolist (arg args)
      (ironclad:update-digest dig (hashable arg)))
    (let ((hv  (ironclad:produce-digest dig)))
      (values (make-instance dig-class
                             :val (make-instance 'bev
                                                 :vec hv))
              (length hv)))))

(defun hash/ripemd/160 (&rest args)
  (apply 'local-digest :ripemd-160 'hash-ripemd/160 args))

(defun hash/sha2/256 (&rest args)
  (apply 'local-digest :sha256 'hash/sha2/256 args))

(defun hash/256 (&rest args)
  (apply 'local-digest :sha3/256 'hash/256 args))

(defun hash/384 (&rest args)
  (apply 'local-digest :sha3/384 'hash/384 args))

(defun hash/512 (&rest args)
  (apply 'local-digest :sha3 'hash/512 args))

(defun get-hash-nbytes (nb seed)
  ;; returns a vector of nb raw-bytes
  (cond
   ((> nb 64)
    (let ((bytes (make-ub8-vector nb)))
      (um:nlet-tail iter ((start 0))
        (if (< start nb)
            (let ((end  (min (+ start 64) nb)))
              (replace bytes (hash-bytes (hash/512 start seed))
                       :start1 start
                       :end1   end)
              (iter end))
          bytes))
      ))
   ((= nb 64)
    (hash-bytes (hash/512 seed)))
   ((> nb 32)
    (let ((bytes (hash-bytes (hash/512 seed))))
      (subseq bytes 0 nb)))
   ((= nb 32)
    (hash-bytes (hash/256 seed)))
   (t
    (let ((bytes (hash-bytes (hash/256 seed))))
      (subseq bytes 0 nb)))
   ))

(defmethod hash-check (item (expected string))
  (string-equal expected (hex-str (hash/256 item))))

(defmethod hash= ((hash1 hash) (hash2 hash))
  (vec= hash1 hash2))

(defmethod print-object ((obj hash) out-stream)
  (if *print-readably*
      (call-next-method)
    (format out-stream "#<~A ~A >"
            (class-name (class-of obj))
            (short-str (hex-str obj)))
    ))

