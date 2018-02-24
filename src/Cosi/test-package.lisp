
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

