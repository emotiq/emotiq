;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "emotiq"
  :version "0.1.0"
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

(defsystem "emotiq/logging"
  :depends-on (emotiq
               simple-date-time)
  :components ((:file "note")))

(defsystem "emotiq/delivery"
  :depends-on (emotiq
               simple-date-time)
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

(defsystem "emotiq/filesystem"
  :depends-on (uiop
               emotiq)
  :components ((:module source
                :pathname "./"
                :serial t
                :components ((:file "filesystem")))))

(defsystem "emotiq/txn"
  :depends-on (emotiq
               cosi-bls)
  :components ((:module source
                :pathname "./"
                :serial t
                :components ((:file "txn")))))
                
(defsystem "emotiq/startup"
  :depends-on (emotiq/node
               actors
               emotiq/wallet
               emotiq-rest
               websocket/wallet
	       emotiq/tracker)
  :components ((:module source
                :pathname "./"
                :serial t
                :components ((:file "startup")))))

(defsystem "emotiq/wallet"
  :depends-on (emotiq/logging
               ironclad
               quri
               lisp-object-encoder
               cosi-bls)
  :in-order-to ((test-op (test-op "wallet-test")))
  :components ((:module source
                :pathname "wallet/"
                :serial t
                :components ((:file "name")
                             (:file "transaction")
                             (:file "file")
                             (:file "wallet")))))

(defsystem "emotiq/cli"
  :depends-on (emotiq/wallet
               cl-ppcre
               bordeaux-threads)
  :components ((:module source
                :pathname "./"
                :components ((:file "cli")))))

(defsystem "emotiq/node"  ;; a live node
  :depends-on (gossip
               emotiq/logging)
  :components ((:module source
                :pathname "./"
                :components ((:file "node")))))

(defsystem "emotiq/sim"  ;; a simulated network entirely within the local process
  :depends-on (emotiq/cli
               alexandria
	       emotiq/tracker
               emotiq/txn)  
  :in-order-to ((test-op (test-op "emotiq-sim-test")))
  :components ((:module source
                :pathname "./"
                :components ((:file "handler")
                             (:file "election-sim")
                             (:file "node-sim" :depends-on (election-sim))))))

(defsystem "emotiq/tracker"
  :depends-on (emotiq
               actors
               emotiq/logging)
  :components ((:module source
                :pathname "./"
                :serial t
                :components ((:file "state-tracker")))))

(defsystem "emotiq/ate"
  :depends-on (emotiq emotiq/tracker emotiq/sim emotiq/startup gossip)
  :components ((:module source
                :pathname "./"
                :serial t
                :components ((:file "ate")))))

(defsystem "emotiq/config"
  :depends-on (cl-json
               emotiq/logging
               emotiq/filesystem
               lisp-object-encoder
               useful-macros
               gossip/config)
  :in-order-to ((test-op (test-op "emotiq-config-test")))
  :components ((:module source
                :pathname "./"
                :serial t
                :components ((:file "stakes")
                             (:file "genesis")
                             (:file "config")))))

(defsystem "emotiq/config/generate"
  :depends-on (emotiq/config
               cosi-bls)
  :in-order-to ((test-op (test-op "emotiq-config-test")))
  :components ((:module source
                :pathname "./"
                :serial t
                :components ((:file "generate")))))
