;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "emotiq-tests"
  :version "0.0.1"
  :description "Emotiq Tests"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on (emotiq lisp-unit)
  :perform (test-op (o s)
             (symbol-call :lisp-unit :run-tests
                          :all (slot-value s 'asdf/component:name))) ;; REALLY?
                                                                     ;; TODO:
                                                                     ;; implement
                                                                     ;; a
                                                                     ;; reasonable
                                                                     ;; stringify
  :pathname #P"tests/"
  :serial t
  :components ((:file "package")
               (:file "emotiq-tests")))
