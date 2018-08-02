(defsystem "gossip"
  :name "Gossip"
  :description "Gossip protocols"
  :author "Shannon Spires <svs@emotiq.ch>"
  :version "0.2.3"
  :maintainer "Shannon Spires <svs@emotiq.ch>"
  :depends-on (emotiq/config
               emotiq/logging
               quicklisp
	       uiop
               mpcompat
               key-value-store
               actors
               crypto-pairings
               usocket)
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
                      

