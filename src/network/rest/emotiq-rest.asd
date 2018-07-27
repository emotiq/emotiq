(defsystem "emotiq-rest"
  :depends-on (emotiq/logging
               emotiq/tracker
               emotiq/tracker
               model/wallet
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
                                      (:file "client-0.0.1")                                                                   (:file "dictionary-0.0.1")
                                      (:file "wallet-0.0.1")
                                      (:file "node-0.0.1")
                                      (:file "emotiq-0.0.1")))))

(defsystem "emotiq-rest/config"
  :depends-on (cl-json)
  :components ((:file "package")
               (:file "config" :depends-on (package))))


