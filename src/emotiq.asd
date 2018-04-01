;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "emotiq"
  :version "0.0.3"
  :description "Emotiq"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on nil ;; DO NOT add dependencies here; create a secondary
                  ;; system to encapsulate them which depends-on this
                  ;; one.
  :in-order-to ((test-op (test-op "emotiq-test")))
  :components ((:module package
                :pathname "./"
                :components ((:file "package")))))

(defsystem "emotiq/delivery"
  :depends-on (emotiq)
  :components ((:module delivery
                :pathname "./"
                :components ((:file "production")))))

(defsystem "emotiq/utilities"  
  :depends-on (emotiq
               ironclad
               bordeaux-threads)
  :components ((:module source
                :pathname "./"
                :serial t
                :components ((:file "utilities")
                             (:file "external")))))

(defsystem "emotiq/blockchain"
  :depends-on (emotiq/utilities)
  :components ((:module source
                :pathname "./"
                :serial t
                :components ((:file "blockchain")))))
                                       



