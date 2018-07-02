(restas:define-module :route.dictionary/0/0/1
  (:nicknames :route.dictionary/0/0
              :route.dictionary/0)
  (:use :cl :emotiq-rest))

(in-package :route.dictionary/0/0/1)

(restas:define-route
 %api
 ("/api/"
  :content-type "text/html")
 (as-html
   (:html
    (:body
     (:h1 "Dictionary API")
     (:ul
      (:li
       (:a :href (restas:genurl '%index) "[/] List dictionaries"))
      (:li
       (:a :href (restas:genurl '%en) "[/en/] English dictionary")))))))

(restas:define-route %index ("/"
                             :content-type "application/javascript")
  (cl-json:encode-json-to-string
   (model/wallet:enumerate-dictionaries)))

(restas:define-route %en ("/en"
                                        :content-type "application/javascript")
  (cl-json:encode-json-to-string
   (model/wallet:get-dictionary "en")))


