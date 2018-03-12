
(defpackage :encode-check
  (:use :common-lisp)
  (:export
   :check))

(in-package :encode-check)

(defun check (pair)
  (assert (equalp (loenc:encode (car pair)) (cdr pair)))
  (assert (equalp (car pair) (loenc:decode (cdr pair)))))

(defun gen-checks (items)
  (loop for item in items collect (cons item (loenc:encode item))))

(defvar *check-data-file*  "~/Documents/encoding-check.lisp")

(defstruct thing a b c)

(defun gen-check-file ()
  (with-open-file (f
		   *check-data-file*
		   :direction :output
		   :if-exists :rename
		   :if-does-not-exist :create)
    (with-standard-io-syntax
      (pprint (gen-checks
	       (list 1 2 3 #\a #\b #\c
		     (expt 2 100) '(1 2 3) #(1 2 3)
		     "this is a test"
		     (make-thing :a 1 :b 2 :c :three)))
	      f))))

(defun get-check-data ()
  (with-open-file (f
		   *check-data-file*
		   :direction :input)
    (read f)))
  
(defun recheck ()
  (dolist (pair (get-check-data))
    (check pair))
  :okay)

#|
(recheck)
|#