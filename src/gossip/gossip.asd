(asdf::defsystem :gossip
  :name "Gossip"
  :description "Gossip protocols"
  :author "Shannon Spires <svs@emotiq.ch>"
  :version "0.1"
  :maintainer "Shannon Spires <svs@emotiq.ch>"
 ; :licence "BSD 3-clause"
  :depends-on (:uiop
               :mpcompat
               :key-value-store
               :actors)
  :serial t
  :components ((:file "package")
               (:file "gossip")
               #+(and :CLOZURE :DARWIN) (:file "ccl-darwin-special")
               (:file "graphviz")))
