;;;; emotiq-tests.asd

(defsystem "emotiq-tests"
  :description "Emotiq Tests"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on (#:emotiq #:lisp-unit)
  :pathname #P"tests/"
  :serial t
  :components ((:file "package")
               (:file "emotiq-tests")))
