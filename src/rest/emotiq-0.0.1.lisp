(restas:define-module :route.emotiq/0/0/1
  (:nicknames :route.emotiq/0/0
              :route.emotiq/0)
  (:use :cl :emotiq-rest))

(in-package :route.emotiq/0/0/1)

#+(or)
(restas:mount-module -api- (:route.api/0)
  (:url "/api/"))

(restas:mount-module -wallet- (:route.wallet/0)
  (:url "/wallet/"))

(restas:mount-module -dictionary- (:route.dictionary/0)
  (:url "/dictionary/"))

(restas:define-route %api ("/"
                             :content-type "text/html")
    (as-html
      (:html
       (:body
        (:div :id "navigation"
              (:h1 "Wallet")
              (:div
               :id "wallet"
               (:a :href (restas:genurl '-wallet-.%api) "[API]"))

              (:div
               :id "Dictionary"
               (:a :href (restas:genurl '-dictionary-.%api) "[API]"))

              #+(or)
              (:h1 "API")
              #+(or)
              (:div
               :id "api"
               (:a :href (restas:genurl '-api-.%index) "[Index]")))))))




