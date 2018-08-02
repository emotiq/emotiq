(defsystem "blockchain-test"
  :depends-on (lisp-unit
               emotiq/core)
  :perform  (test-op (o s)
                     (symbol-call :lisp-unit :run-tests :all :blockchain-test))
  :components ((:module tests
                :pathname "./"
                :serial t
                :components ((:file "package")
                             (:file "sim-elections")
                             (:file "genesis-spend-tests")))))

  
