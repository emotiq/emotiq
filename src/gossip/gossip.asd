(asdf::defsystem :gossip
  :name "Gossip"
  :description "Gossip protocols"
  :author "Shannon Spires <svs@emotiq.ch>"
  :version "0.1"
  :maintainer "Shannon Spires <svs@emotiq.ch>"
 ; :licence "BSD 3-clause"
  :depends-on (:quicklisp
	       :uiop
               :mpcompat
               :key-value-store
               :actors ; should be loaded by cosi-BLS
               :cosi-BLS
               :illogical-pathnames)
  :serial t
  :components ((:file "package")
               #+OPENMCL (:file "ccl-sockets-patch")
               (:file "gossip-transport")
               (:file "socket-actors")
               (:file "gossip")
               (:file "http-fetch")
               (:file "graphviz")
               (:file "gossip-startup")))
