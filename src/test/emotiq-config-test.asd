(defsystem "emotiq-config-test"
  :version "0.1.0"
  :description "Emotiq"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on (lisp-unit
               emotiq/config)
  :perform (test-op (o s)
                    (symbol-call :lisp-unit :run-tests
                                 :all :emotiq/config/t))
  :components ((:module package
                :pathname "./"
                :components ((:file "package")))
               (:module tests
                :depends-on (package)
                :pathname "./"
                :components ((:file "emotiq-config")))))

