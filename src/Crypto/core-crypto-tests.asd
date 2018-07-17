
(asdf:defsystem "core-crypto-tests"
  :depends-on (lisp-unit
               core-crypto)
  :perform  (test-op (o s)
                     (symbol-call :lisp-unit :run-tests
                                  :all :crypto/test))
  :components ((:module package
                :pathname "core-crypto-tests/"
                :components ((:file "package")))
               (:module tests
                :pathname "core-crypto-tests/"
                :depends-on (package)
                :components ((:file "test-modmath")
                             (:file "test-vecrepr")
                             (:file "test-hash")
                             (:file "test-edwards"))
                )))
