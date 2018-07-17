
(asdf:defsystem "crypto-pairings-tests"
  :depends-on (lisp-unit
               core-crypto)
  :perform  (test-op (o s)
                     (symbol-call :lisp-unit :run-tests
                                  :all :crypto-pairings-tests))
  :components ((:module package
                :pathname "crypto-pairings-tests/"
                :components ((:file "package")))
               (:module tests
                :pathname "crypto-pairings-tests/"
                :depends-on (package)
                :components ((:file "crypto-pairings-test"))
                )))
