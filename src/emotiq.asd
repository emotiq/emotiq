;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "emotiq"
  :version "0.0.5"
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

(defsystem "emotiq/delivery"
  :depends-on (emotiq)
  :components ((:module delivery
                :pathname "./"
                :components ((:file "production")))))

(defsystem "emotiq/utilities"  
  :depends-on (emotiq/delivery
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
  ;; add gossip here soon:
  :depends-on (emotiq/blockchain crypto-pairings core-crypto)
  :components ((:module source
                :pathname "./"
                :serial t
                :components ((:file "startup")))))

(defsystem "emotiq/wallet"
  :depends-on (emotiq
               ironclad
               lisp-object-encoder
               cosi-bls)
  :in-order-to ((test-op (test-op "emotiq-wallet-test")))
  :components ((:module source
                :pathname "./"
                :components ((:file "wallet")))))

(defsystem "emotiq/cli"
  :depends-on (emotiq/wallet
               cl-ppcre
               bordeaux-threads)
  :components ((:module source
                :pathname "./"
                :components ((:file "cli")))))

(defsystem "emotiq/node"  ;; a live node
  :depends-on (emotiq/cli)
  :components ((:module source
                :pathname "./"
                :components ((:file "node")))))

(defsystem "emotiq/sim"  ;; a simulated node
  :depends-on (emotiq/cli)
  :components ((:module source
                :pathname "./"
                :components ((:file "handler")
                             (:file "election-sim")
                             (:file "node-sim")))))


