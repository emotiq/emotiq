(defsystem "emotiq-config-test"
  :version "0.1.0"
  :description "Emotiq"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on (lisp-unit
               emotiq/config/generate)
  :perform (test-op (o s)
                    (symbol-call :lisp-unit :run-tests
                                 :all :emotiq-config-test))
  :components ((:module package
                :pathname "./"
                :components ((:file "package")))
               (:module tests
                :depends-on (package)
                :pathname "./"
                :components ((:file "emotiq-config")))))
