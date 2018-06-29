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
        (:span "[Run a new simulation]"
            (:form :method :post
                   :action (restas:genurl '%create)
                   (:input :type "submit" :value "Create"))))
       (:li
        (:a :href (restas:genurl '%results)
            "[Output from simulation]")))
      (:h2 "See Also")
      (:p "The running simulator will also update the node tracker information")
      (:p
       (:a :href "../../node/api/"
                  ;; HACK: under RESTAS we seemingly cannot
                  ;; automatically generate a reference to a route
                  ;; that has been mounted as a sibling, so we cheat
                  ;; by just specifying the relative path.
           #+(or) (restas:genurl 'route.emotiq/0::-node-.%api)
           "[Node API]"))))))

(defparameter *simulator-output* nil)

(restas:define-route %create ("/"
                             :method :post
                             :content-type "text/plain")
  (when *simulator-output*
    (setf (hunchentoot:return-code*) hunchentoot:+http-conflict+)
    (return-from %create "Only one simulator can be created."))
  (bt:make-thread #'run-simulator)
  (setf (hunchentoot:return-code*) hunchentoot:+http-created+)
  (restas:genurl* '%results))

(defun run-simulator ()
  (setf *simulator-output* (make-string-output-stream))
  (let ((emotiq:*notestream* *simulator-output*))
    (multiple-value-bind (state tracking-actor)
        (emotiq/sim:initialize)
      (let ((emotiq/tracker:*state* state)
            (emotiq/tracker:*tracking-actor* tracking-actor))
        (emotiq/sim:run-new-tx)))))

(defvar *simulator-results* "") 

(restas:define-route %results ("/0"
                               :content-type "text/plain")
  (when *simulator-output*
    (let ((new-output (get-output-stream-string *simulator-output*)))
      (when new-output
        (setf *simulator-results*
              (concatenate 'string
                           (format nil "~&~%current state = ~A~%~%example = ~A~%~%"
                                   (emotiq/tracker:query-current-state)
                                   (emotiq/tracker:example))
                           *simulator-results*
                           new-output)))
      *simulator-results*)))

(restas:define-route %wind-down-sim
    ("/:id"
     :method :delete
     :content-type "text/plain")
  (declare (ignore id))
  (unless *simulator-output*
    (setf (hunchentoot:return-code*) hunchentoot:+http-conflict+)
    (return-from %wind-down-sim "No simulator available to delete."))
  (gossip:shutdown-gossip-server)
  (actors:kill-executives)
  (setf *simulator-output* nil
        *simulator-results* ""))










