(asdf::defsystem :gossip
  :name "Gossip"
  :description "Gossip protocols"
  :author "Shannon Spires <svs@emotiq.ch>"
  :version "0.1"
  :maintainer "Shannon Spires <svs@emotiq.ch>"
 ; :licence "BSD 3-clause"
  :depends-on (:uiop
               #+LISPWORKS :CFFI
               :mpcompat
               :key-value-store
               :actors ; should be loaded by cosi-BLS
               :cosi-BLS)
  :serial t
  :components ((:file "package")
               (:file "gossip")
               (:file "graphviz")))
