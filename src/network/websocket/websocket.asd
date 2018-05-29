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
  :depends-on (emotiq/wallet
               cl-who
               parenscript
               cl-json
               hunchensocket
               simple-date-time)
  :components ((:module package
                        :pathname "./"
                        :components ((:file "package")))
               (:module mock
                        :pathname "./"
                        :depends-on (package)
                        :components ((:file "mock")))
               (:module source
                        :pathname "./"
                        :depends-on (mock)
                        :components ((:file "serve")))))



