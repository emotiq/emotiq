(defsystem "gossip"
  :name "Gossip"
  :description "Gossip protocols"
  :author "Shannon Spires <svs@emotiq.ch>"
  :version "0.2.3"
  :maintainer "Shannon Spires <svs@emotiq.ch>"
  :depends-on (gossip/config
               emotiq/logging
               quicklisp
	       uiop
               mpcompat
               key-value-store
               actors
               crypto-pairings
               usocket
               illogical-pathnames)
  :in-order-to ((test-op (test-op "gossip-test")))
  :serial t
  :components ((:file "package")
               #+OPENMCL
               (:file "ccl-sockets-patch")
               #+(and OPENMCL EASYGUI) (:file "hemlock-log-streams")
               (:file "macros")
               (:file "gossip-transport")
               (:file "monads")
               (:file "gossip")
               (:file "logging")
               (:file "gossip-api")
               (:file "http-fetch")
               (:file "graphviz")
               (:file "gossip-startup")))

(defsystem "gossip/config"
  :depends-on (emotiq/filesystem
               emotiq/logging
               crypto-pairings)
    :in-order-to ((test-op (test-op "gossip-config-tests")))
  :components ((:module package :pathname "."
                        :components ((:file "package")))
               (:module config :pathname "config/"
                        :depends-on (package)
                        :components ((:file "read")
                                     (:file "generate")))))



  
