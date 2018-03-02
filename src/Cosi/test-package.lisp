(in-package cl-user)

(defpackage :package-a
  (:use :common-lisp)
  (:export
   :a
   :b
   :c))

(defpackage :package-b
  (:use :common-lisp)
  (:import-from :package-a
   :b)
  (:export
   :a
   :b
   :c))

(defpackage :package-c
  (:use :common-lisp)
  (:import-from :package-a
   :b)
  (:import-from :package-b
   :b)
  (:export
   :a
   :b
   :c))

(define-test package-test
    (let ((packages (mapcar #'find-package '(:a :b: :c))))
      ;;; TODO: What do we wish to test about the package declarations?
      (lisp-unit:assert-true
       (= (length packages) 3))))
            
