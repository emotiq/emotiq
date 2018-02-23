;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "emotiq"
  :version "0.0.1"
  :description "Emotiq"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on (ironclad
               bordeaux-threads)
  :serial t
  :in-order-to ((test-op (test-op "emotiq/t")))
  :components ((:file "package")
               (:file "utilities")
               (:file "external")))

(defsystem "emotiq/t"
  :version "0.0.1"
  :description "Emotiq"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :defsystem-depends-on (prove-asdf)
  :depends-on (prove
               emotiq)
  :perform (test-op (o s)
              (symbol-call :prove :run s))
  :components ((:module tests :pathname "t/"
                        :components ((:test-file "base")
                                     (:test-file "tests")))))
