
(asdf:defsystem "crypto-pairings-tests"
  :depends-on (lisp-unit
               core-crypto)
  :perform  (test-op (o s)
                     (symbol-call :lisp-unit :run-tests
                                  :all :crypto/test))
  :components ((:module package
                :pathname "crypto-pairings-tests/"
                :components ((:file "package")))
               (:module tests
                :pathname "tests/"
                :depends-on (package)
                :components ((:file "test-pairings"))
                )))
