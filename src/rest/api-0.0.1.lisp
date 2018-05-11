(restas:define-module :route.api/0/0/1
  (:nicknames :route.api/0/0
              :route.api/0)
  (:use :cl :emotiq-rest))

(in-package :route.api/0/0/1)


(restas:define-route %swaggen.json ("swagger.json"
                                    :content-type "application/javascript")
  (reify (asdf:system-relative-pathname :emotiq-rest "swagger.json")))

(restas:define-route %wildcard ("*p" :method :get)
  (reify (merge-pathnames
          (assemble-path p)
          ;;; FIXME: use configuration option
          #p"~/work/swagger-ui/dist/")))




                                    
