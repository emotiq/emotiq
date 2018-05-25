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
     "Websocket client interface test."))))

(restas:define-route
 %client.js
 ("/client.js"
  :content-type "application/javascript")
 (ps:ps-compile-file
  (asdf:system-relative-pathname :emotiq-rest "client.pscript"))
 )


 
