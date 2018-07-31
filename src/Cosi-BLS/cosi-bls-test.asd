(defsystem "cosi-bls-test"
  :depends-on (lisp-unit
               cosi-bls)
  :perform  (test-op (o s)
                     (symbol-call :lisp-unit :run-tests
                                  :all :cosi-bls-test))
  :components ((:module package
                :pathname "test/"
                :components ((:file "package")))
               (:module tests
                :pathname "test/"
                :depends-on (package)
                :components ((:file "cosi-tests")
                             (:file "address-tests")
                             (:file "ranges")
                             (:file "tx-tests")))))

  
