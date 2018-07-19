;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "gossip-test"
  :version "0.1.0"
  :description "Gossip Test"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on (gossip lisp-unit)
  :serial t
  :perform (test-op (o s)
             (symbol-call :lisp-unit :run-tests
                          :all :gossip-test))
  :components ((:module package
                        :pathname "test/"
                        :components ((:file "package")))
               (:module tests
                        :depends-on (package)
                        :pathname "test/"
                        :components ((:file "gossip-test")))))
