;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "emotiq-test"
  :version "0.1.0"
  :description "Emotiq Tests"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on (lisp-unit
               emotiq/utilities)
  :perform (test-op (o s)
             (symbol-call :lisp-unit :run-tests
                          :all :emotiq-test))
  :components ((:module package
                        :pathname "test/"
                        :components ((:file "package")))
               (:module tests
                        :depends-on (package)
                        :pathname "test/"
                        :components ((:file "emotiq-test")))))



