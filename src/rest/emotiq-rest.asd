(defsystem "emotiq-rest"
  :depends-on (restas
               cosi-bls
               jonathan
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
                        :components ((:file "wallet-1.0.0")))))



