;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "cosi-bls-test"
  :depends-on (cosi-bls lisp-unit)
  :perform (test-op (o c) (symbol-call :lisp-unit :run-tests
                                       :all :cosi-test))
  :components ((:module package
                        :pathname "./"
                        :components ((:file "package")))))


  
  


