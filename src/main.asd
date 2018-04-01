;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "main"
  :version "0.0.3"
  :description "Emotiq"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on (emotiq/delivery
               crypto-pairings)
  :components ((:file "main")))



