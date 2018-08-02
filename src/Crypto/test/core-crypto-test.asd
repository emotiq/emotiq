(defsystem "core-crypto-test"
  :depends-on (crypto-pairings
               lisp-unit)
  :perform (test-op (o s)
             (symbol-call :lisp-unit :run-tests
                          :all :core-crypto-test))
  :components ((:module package
                :pathname "./"
                :components ((:file "package")))
               (:module tests
                :depends-on (package)
                :pathname "./"
                :components ((:file "hash")
                             (:file "test-modmath")
                             (:file "test-vecrepr")
                             (:file "test-hash")
                             (:file "test-edwards"))
                )))


                       
