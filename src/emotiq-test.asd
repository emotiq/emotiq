;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "emotiq-test"
  :version "0.0.3"
  :description "Emotiq Tests"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on (lisp-unit
               emotiq/utilities
               emotiq/blockchain)
  :perform (test-op (o s)
             (symbol-call :lisp-unit :run-tests
                          :all :emotiq-test))
  :components ((:module package
                        :pathname "tests/"
                        :components ((:file "package")))
               (:module tests
                        :depends-on (package)
                        :pathname "tests/"
                        :components ((:file "emotiq-test")
                                     (:file "blockchain-test")))))



