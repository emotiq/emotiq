(restas:define-module :route.wallet/0/0/1
  (:nicknames :route.wallet/0/0
              :route.wallet/0)
  (:use :cl :emotiq-rest))

(in-package :route.wallet/0/0/1)

(restas:define-route
 %api
 ("/api/"
  :content-type "text/html")
 (as-html
   (:html
    (:body
     (:h1 "Wallet API")
     (:ul
      (:li
       (:a :href (restas:genurl '%enumerate-wallets)
           "[/] enumerate wallets")))
     (:ul
      (:li
       (:a :href (restas:genurl '%get-wallet :name "My%20Wallet")
           "[/wallet/My%20Wallet]")))
     (:ul
      (:li
       (:a :href (restas:genurl '%get-wallet-addresses :name "My%20Wallet")
           "[/wallet/My%20Wallet/address/]")))))))

(restas:define-route
 %enumerate-wallets
 ("/"
  :content-type "application/json")
 (cl-json:encode-json-to-string  (emotiq/wallet:enumerate-wallets)))

(restas:define-route
 %get-wallet
 ("/:name"
  :content-type "application/json")
 (let ((wallet (emotiq/wallet:get-wallet-named (hunchentoot:url-decode name))))
   (cl-json:encode-json-to-string wallet)))

(restas:define-route
 %get-wallet-addresses
 ("/:name/address/"
  :content-type "application/json")
 (let ((wallet (emotiq/wallet:get-wallet-named (hunchentoot:url-decode name))))
   (cl-json:encode-json-to-string
    (list (emotiq/wallet:primary-address wallet)))))




   

 



  
  


