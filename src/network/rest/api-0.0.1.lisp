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
          ;;; FIXME: write configuration code to assemble the
          ;;; necessary local artifacts from
          ;; <git+https://github.com/swagger-api/swagger-ui.git>
          #p"~/work/swagger-ui/dist/")))

(defvar *swagger-ui-dist* (asdf:system-relative-pathname :emotiq "../var/swagger-ui/"))
(defvar *swagger-ui-uri* "https://github.com/swagger-api/swagger-ui.git")

(defun download-artifacts ()
  (unless (probe-file *swagger-ui-dist*)
    (format *standard-output* "~&Cloning Swagger UI artifacts~&from: <~a>~&  to: <file:~a>…"
            *swagger-ui-uri* *swagger-ui-dist*)
    (prog1 
        (uiop:run-program
         (format nil "git clone ~a" *swagger-ui-uri*
                 :output :string :error-output :string
                 :directory *swagger-ui-dist*))
      (format *standard-output* " done!~&"))))
  
  




                                    
