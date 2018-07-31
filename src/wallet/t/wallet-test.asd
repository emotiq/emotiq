;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "wallet-test"
  :version "0.0.1"
  :description "Emotiq"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on (lisp-unit
               emotiq/wallet)
  :perform (test-op (o s)
                    (symbol-call :lisp-unit :run-tests
                                 :all :wallet-test))
  :components ((:module package :pathname "./"
                        :components ((:file "package")))
               (:module tests :pathname "./"
                        :depends-on (package)
                        :components ((:file "internal")
                                     (:file "wallet")))))





