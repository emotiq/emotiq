(restas:define-module :route.simulator/0/0/1
  (:nicknames :route.simulator/0/0
              :route.simulator/0)
  (:use :cl :emotiq-rest))


(in-package :route.simulator/0/0/1)

(restas:define-route %api
    ("/api/"
     :content-type "text/html")
  (as-html
    (:html
     (:body
      (:h1 "Simulator API")
      (:ul
       (:li
        (:p "[Make a new simulation]"
            (:form :method :post
                   :action (restas:genurl '%create)
                   (:input :type "submit" :value "Create")))))
       (:ul
        (:li
         (:a :href (restas:genurl '%wind-down-sim :name "Simulator" :id 0)
             "[Stop the simulator]")))))))

;; TODO: delete this kludge and replace with wind-down-sim which unitializes pr
(defparameter *simulator-p* nil) 

(restas:define-route %create ("/"
                             :method :post
                             :content-type "text/plain")
  ;;; TODO: redirect to single simulator reference
  (let ((output (make-string-output-stream)))
    (let ((*error-output* output)
          (*standard-output* output))
      (when (not *simulator-p*)  ;; prevent multiple initializations, TODO: kludge, fix this
        (emotiq/sim:initialize)
        (setf *simulator-p* t))
      (values
       output
       (emotiq/sim:run-new-tx)))))

(restas:define-route %wind-down-sim
    ("/:id/"
     :method :delete
     :content-type "text/plain")
  (declare (ignore id))
  (gossip:shutdown-gossip-server)
  (actors:kill-executives)
  (setf *simulator-p* nil))







