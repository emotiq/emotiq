(defsystem "cosi-bls-tests"
  :depends-on (lisp-unit
               cosi-bls)
  :perform  (test-op (o s)
                     (symbol-call :lisp-unit :run-tests
                                  :all :cosi-tests))
  :components ((:module package
                :pathname "t/"
                :components ((:file "package")))
               (:module tests
                :pathname "t/"
                :depends-on (package)
                :components ((:file "cosi-tests")))))

  
