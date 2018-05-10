(restas:define-module :route.wallet/0/0/1
  (:nicknames :route.wallet/0/0
              :route.wallet/0)
  (:use :cl :emotiq-rest))

(in-package :route.wallet/0/0/1)

(restas:define-route %index ("/"
                             :content-type "text/html")
  (as-html
    (:html
     (:body
      (:h1 "Wallet REST interface"
           (:ul
            (:li (:a :href "dictionary.js" "English dictionary"))))))))

(restas:define-route %dictionary.js ("/dictionary.js"
                                     :content-type "application/javascript")
  (jonathan:to-json `(:dictionary ,(cosi-keying::import-wordlist "english.txt"))))

                                  
  


