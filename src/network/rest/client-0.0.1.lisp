(restas:define-module :route.client/0/0/1
  (:nicknames :route.client/0/0
              :route.client/0)
  (:use :cl :emotiq-rest))

(in-package :route.client/0/0/1)

(restas:define-route
 %index
 ("/"
  :content-type "text/html")
 (as-html
   (:html
    (:head
     (:script :src (restas:genurl '%client.js)))
    (:body
     (:h1
      "Websocket client interface testing")
     (:p "This page has loaded Javascript stub functions to assist in
testing the WebSocket implementation.  Opening a Javascript console
gives the toplevel definitions of the following functions:")

     (:dl
      (:dt "connect()")
      (:dd "Open the websocket connection.  Call this to initialize the connection or if one gets errors about not being connected.")
      (:dt "consensus()")
      (:dd "Subscribe to the node consensus message endpoint.")
      (:dt "wallet()")
      (:dd "Return information about the default wallet.")
      (:dt "keyphrase()")
      (:dd "Return the recovery phrase of the default wallet.")
      (:dt "transactions()")
      (:dd "Return all transactions associated with the default wallet."))))))
           
(restas:define-route
 %client.js
 ("/client.js"
  :content-type "application/javascript")
 (ps:ps-compile-file
  (asdf:system-relative-pathname :emotiq-rest "client.pscript"))
 )


 
