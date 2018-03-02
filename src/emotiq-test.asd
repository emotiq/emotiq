;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "emotiq-test"
  :version "0.0.1"
  :description "Emotiq Tests"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on (emotiq lisp-unit)
  :perform (test-op (o s)
             (symbol-call :lisp-unit :run-tests
                          :all :emotiq-tests))
  :components ((:module package
                        :pathname "tests/"
                        :components ((:file "package")))
               (:module tests
                        :depends-on (package)
                        :pathname "tests/"
                        :components ((:file "emotiq-tests")))))



