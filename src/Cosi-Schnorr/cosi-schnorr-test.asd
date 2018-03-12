;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "cosi-schnorr-test"
  :depends-on (cosi-schnorr lisp-unit)
  :perform (test-op (o c) (symbol-call :lisp-unit :run-tests
                                       :all :cosi-test))
  :components ((:module package
                        :pathname "./"
                        :components ((:file "package")))
               (:module tests
                        :pathname "./"
                        :depends-on (package)
                        :components ((:file "test-package")))))


  
  


