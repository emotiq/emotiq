(defsystem "emotiq-rest"
  :depends-on (emotiq/wallet
               emotiq/logging
               emotiq/tracker
               emotiq/sim
               restas 
               cosi-bls
               parenscript
               cl-json
               cl-who
               simple-date-time)
  :components ((:module package
                        :pathname "./"
                        :components ((:file "package")))
               (:module util
                        :pathname "./"
                        :depends-on (package)
                        :components ((:file "util")))
               (:module server
                        :pathname "./"
                        :depends-on (util)
                        :components ((:file "server")))
               (:module routes
                        :pathname "./"
                        :depends-on (util)
                        :serial t 
                        :components  ((:file "api-0.0.1")
                                      (:file "client-0.0.1")                               
                                      (:file "dictionary-0.0.1")
                                      (:file "wallet-0.0.1")
                                      (:file "node-0.0.1")
                                      (:file "simulator-0.0.1")
                                      (:file "emotiq-0.0.1")))))




