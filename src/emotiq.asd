;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "emotiq"
  :description "Emotiq"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on (#:ironclad #:bordeaux-threads)
  :serial t
  :in-order-to ((test-op (test-op "emotiq-tests")))
  :components ((:file "package")
               (:file "utilities")
               (:file "external"))
  :in-order-to ((test-op (test-op "emotiq-tests"))))
