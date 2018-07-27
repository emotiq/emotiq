(defsystem "blockchain-test"
  :depends-on (lisp-unit
               emotiq/sim
               emotiq/blockchain)
  :perform  (test-op (o s)
                     (symbol-call :lisp-unit :run-tests :all :blockchain-test))
  :components ((:module package
                :pathname "./"
                :components ((:file "package")))
               (:module tests
                :pathname "./"
                :depends-on (package)
                :components ((:file "genesis-spend-tests")))))

  
