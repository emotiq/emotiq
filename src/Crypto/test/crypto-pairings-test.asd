(defsystem "crypto-pairings-test"
  :depends-on (crypto-pairings
               lisp-unit)
  :perform (test-op (o s)
             (symbol-call :lisp-unit :run-tests
                          :all :crypto-pairings-test))
  :components ((:module package
                :pathname "./"
                :components ((:file "package")))
               (:module tests
                :depends-on (package)
                :pathname "./"
                :components ((:file "crypto-pairings-test")))))
