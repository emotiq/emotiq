(restas:define-module :route.emotiq/0/0/1
  (:nicknames :route.emotiq/0/0
              :route.emotiq/0)
  (:use :cl :emotiq-rest))

(in-package :route.emotiq/0/0/1)

(restas:mount-module -client- (:route.client/0)
  (:url "/client/"))

(restas:mount-module -wallet- (:route.wallet/0)
  (:url "/wallet/"))

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
           (:h1 "Emotiq REST API")
           (:div
            :id "client"
            (:a :href (restas:genurl '-client-.%index) "Client"))

           (:div
            :id "wallet"
            (:a :href (restas:genurl '-wallet-.%api) "Wallet"))

           (:div
            :id "dictionary"
            (:a :href (restas:genurl '-dictionary-.%api) "Dictionary")))))))


 



