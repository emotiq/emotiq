(restas:define-module :route.emotiq/0/0/1
  (:nicknames :route.emotiq/0/0
              :route.emotiq/0)
  (:use :cl :emotiq-rest))

(in-package :route.emotiq/0/0/1)

(restas:mount-module -client- (:route.client/0)
  (:url "/client/"))

(restas:mount-module -wallet- (:route.wallet/0)
  (:url "/wallet/"))

(restas:mount-module -simulator- (:route.simulator/0)
  (:url "/simulator/"))

(restas:mount-module -node- (:route.node/0)
  (:url "/node/"))

(restas:mount-module -dictionary- (:route.dictionary/0)
  (:url "/dictionary/"))

(restas:define-route
 %api
 ("/"
  :content-type "text/html")
 (as-html
   (:html
    (:body
     (:div :id "navigation"
           (:h1 "Wallet")
           (:div
            :id "client"
            (:a :href (restas:genurl '-client-.%index) "Client"))

           (:div
            :id "wallet"
            (:a :href (restas:genurl '-wallet-.%api) "Wallet"))

           (:div
            :id "simulator"
            (:a :href (restas:genurl '-simulator-.%api) "Simulator"))

           (:div
            :id "node"
            (:a :href (restas:genurl '-node-.%api) "Node"))

           (:div
            :id "dictionary"
            (:a :href (restas:genurl '-dictionary-.%api) "Dictionary")))))))


 



