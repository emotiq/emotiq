;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "emotiq"
  :version "0.0.3"
  :description "Emotiq"
  :author "Copyright (c) 2018 Emotiq AG"
  :license "MIT (see LICENSE.txt)"
  :depends-on nil ;; DO NOT add dependencies here; create a secondary
                  ;; system to encapsulate them which :DEPENDS-ON this
                  ;; one.
  :in-order-to ((test-op (test-op "emotiq-test")))
  :components ((:module package
                :pathname "./"
                :components ((:file "package")))))

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
                                       
(defsystem "emotiq/startup"
  :depends-on (emotiq/utilities crypto-pairings core-crypto)
  :components ((:module source
                :pathname "./"
                :serial t
                :components ((:file "startup")))))
                                       
(defsystem "emotiq/chain"
  :depends-on (emotiq
               ads-clos)
  :components ((:module source
                :pathname "./"
                :components ((:file "chain")))))


