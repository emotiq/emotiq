(defsystem "gossip-config-tests"
  :defsystem-depends-on (prove-asdf)
  :depends-on (gossip/config
               prove
               alexandria)
  :perform (test-op (o s)
              (symbol-call :prove :run s))
  :components ((:test-file "generate-node")))

  
