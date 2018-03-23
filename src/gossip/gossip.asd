(asdf::defsystem :gossip
  :name "Gossip"
  :description "Gossip protocols"
  :author "Shannon Spires <svs@emotiq.ch>"
  :version "0.1"
  :maintainer "Shannon Spires <svs@emotiq.ch>"
 ; :licence "BSD 3-clause"
  :depends-on (:uiop :mpcompat :key-value-store)
  :serial t
  :components ((:file "package")
               (:file "simple-gossip")
               (:file "graphviz")))
