(defsystem websocket
  :depends-on (websocket/wallet))

(defsystem websocket/research/websocket-driver
  :depends-on (websocket-driver
               clack)
  :components ((:file "websocket-driver-demo")))

(defsystem websocket/research/hunchensocket
  :depends-on (hunchensocket)
  :components ((:module source  :pathname "research/"
                        :components ((:file "hunchensocket-demo")
                                     (:file "hunchensocket-serve")))))
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
               (:module serve
                        :pathname "./"
                        :depends-on (async)
                        :components ((:file "serve")))))



