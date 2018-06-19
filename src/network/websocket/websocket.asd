(defsystem websocket)

(defsystem websocket/demo/websocket-driver
  :depends-on (websocket-driver
               clack)
  :components ((:file "websocket-driver-demo")))

(defsystem websocket/demo/hunchensocket
  :depends-on (hunchensocket)
  :components ((:file "hunchensocket-demo")))

(defsystem websocket/wallet/hunchensocket
  :depends-on (hunchensocket
               simple-date-time)
  :components ((:file "hunchensocket-serve")))             

(defsystem websocket/wallet
  :depends-on (model/wallet
               emotiq/logging
               cl-who
               parenscript
               cl-json
               hunchensocket
               simple-date-time)
  :components ((:module package
                        :pathname "./"
                        :components ((:file "package")))
               (:module async
                        :pathname "./"
                        :depends-on (package)
                        :components ((:file "async")))
               (:module source
                        :pathname "./"
                        :depends-on (async)
                        :components ((:file "serve")))))



