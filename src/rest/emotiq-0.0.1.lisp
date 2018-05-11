(restas:define-module :route.emotiq/0/0/1
  (:nicknames :route.emotiq/0/0
              :route.emotiq/0)
  (:use :cl :emotiq-rest))

(in-package :route.emotiq/0/0/1)

(restas:mount-module -wallet- (:route.wallet/0)
  (:url "/wallet/"))

(restas:mount-module -api- (:route.api/0)
  (:url "/api/"))

(restas:define-route %index ("/"
                             :content-type "text/html")
    (as-html
      (:html
       (:body
        (:div :id "navigation"
              (:h1 "Wallet")
              (:div
               :id "wallet"
               (:a :href (restas:genurl '-wallet-.%index) "[Index]"))

              (:h1 "API")
              (:div
               :id "api"
               (:a :href (restas:genurl '-api-.%index) "[Index]")))))))




