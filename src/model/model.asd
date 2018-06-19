(defsystem model)

(defsystem model/wallet
  :depends-on (cl-json
               emotiq/wallet)
  :components ((:module package :pathname "./"
                        :components ((:file package)))
               (:module source :pathname "./"
                        :depends-on (package)
                        :components ((:file "mock")
                                     (:file "dictionary")
                                     (:file "wallet")))))


