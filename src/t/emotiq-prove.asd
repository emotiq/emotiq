;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "emotiq-prove"
  :version "0.0.2"
  :description "Emotiq"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on (prove
               emotiq)
  :defsystem-depends-on (prove-asdf) ;;; FIXME
  :perform (test-op (o s)
              (symbol-call :prove :run s))
  :components ((:module tests :pathname "./"
                        :components ((:test-file "base")
                                     (:test-file "tests")))))
