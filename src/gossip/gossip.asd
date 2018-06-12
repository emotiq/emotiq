(defsystem "gossip"
  :name "Gossip"
  :description "Gossip protocols"
  :author "Shannon Spires <svs@emotiq.ch>"
  :version "0.2.2"
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
  :in-order-to ((test-op (test-op "gossip-tests")))
  :serial t
  :components ((:file "package")
               #+OPENMCL
               (:file "ccl-sockets-patch")
               #+(and OPENMCL EASYGUI)
               (:file "hemlock-log-streams")
               (:file "socket-actors")
               (:file "monads")
               (:file "logging")
               (:file "gossip")
               (:file "gossip-api")
               (:file "http-fetch")
               (:file "graphviz")
               (:file "gossip-startup")))

(defsystem "gossip/config"
  :depends-on (emotiq/filesystem
               crypto-pairings)
  :components ((:module package :pathname "."
                        :components ((:file "package")))
               (:module config :pathname "config/"
                        :depends-on (package)
                        :components ((:file "read")
                                     (:file "generate")))))



  
