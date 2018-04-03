;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "gossip-tests"
  :version "0.0.1"
  :description "Gossip Tests"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on (gossip lisp-unit)
  :serial t
  :perform (test-op (o s)
             (symbol-call :lisp-unit :run-tests
                          :all :gossip-test))
  :components ((:module package
                        :pathname "tests/"
                        :components ((:file "package")))
               (:module tests
                        :depends-on (package)
                        :pathname "tests/"
                        :components ((:file "gossip-tests")))))