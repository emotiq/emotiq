(asdf::defsystem :gossip
  :name "Gossip"
  :description "Gossip protocols"
  :author "Shannon Spires <svs@emotiq.ch>"
  :version "0.1"
  :maintainer "Shannon Spires <svs@emotiq.ch>"
  :depends-on (:quicklisp
	       :uiop
               :mpcompat
               :key-value-store
               :actors
               :crypto-pairings
               :usocket
               :illogical-pathnames)
  :serial t
  :components ((:file "package")
               #+OPENMCL (:file "ccl-sockets-patch")
               #+(and OPENMCL EASYGUI) (:file "hemlock-log-streams")
               (:file "socket-actors")
               (:file "monads")
               (:file "logging")
               (:file "gossip")
               (:file "http-fetch")
               (:file "graphviz")
               (:file "gossip-startup")))

(defsystem "gossip/config"
  :depends-on (gossip
               alexandria
               crypto-pairings)
  :components ((:module config :pathname "config/"
                        :components ((:file "generate")))))


  
