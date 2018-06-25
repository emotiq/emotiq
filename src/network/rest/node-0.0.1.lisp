(restas:define-module :route.node/0/0/1
  (:nicknames :route.node/0/0
              :route.node/0)
  (:use :cl :emotiq-rest))

(in-package :route.node/0/0/1)

(restas:define-route %api
    ("/api/"
     :content-type "text/html")
  (as-html
    (:html
     (:body
      (:h1 "Node API")
      (:ul
       (:li
        (:a :href (restas:genurl '%tracker)
            "[Get status from the tracker]")))))))

(restas:define-route %tracker
    ("/"
     :method :get
     :content-type "text/plain")
  (format nil "~a"
          (emotiq/tracker:query-current-state)))

(restas:define-route %node
    ("/:node/"
     :method :delete
     :content-type "application/javascript")
  "Unimplemented termination of node")


     
