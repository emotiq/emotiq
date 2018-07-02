(defsystem model
  :components 
  ((:module package :pathname "./"
            :components ((:file "package")))))

(defsystem model/wallet
  :depends-on (model
               cl-json
               emotiq/wallet)
  :components ((:module source :pathname "./"
                        :components ((:file "mock")
                                     (:file "model")
                                     (:file "dictionary")
                                     (:file "wallet")))))


