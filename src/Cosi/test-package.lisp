(in-package cosi-test)

;;; TODO
;;; Hmm… LISP-UNIT tests signalling an error on compilation
;;; borks the whole test suite invocation.  Double plus ungood.
;;; Probably a good idea to install handlers in the test harness
;;; invocation.
#+(or)
(define-test package-test
  (let ((packages
         (handler-case
             (progn 
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
                 t)
           (simple-error (e)
             (values nil (format nil "~a" e))))))
    (when package
      (let ((pkgs (mapcar #'find-package '(:package-a :package-b: :package-c))))
      ;;; TODO: What do we wish to test about the package declarations?
        (lisp-unit:assert-true
         (= (length pkgs) 3))))))

            
