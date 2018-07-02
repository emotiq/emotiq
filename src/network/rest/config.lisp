(in-package :emotiq-rest/config)

(defclass rest-server ()
  ((prototype
    :initform (make-instance 'cl-json:prototype
                             :lisp-package "emotiq-rest/config"
                             :lisp-class "rest-server"))
   (startup-p
    :initarg :startup-p
    :accessor startup-p
    :initform t)
   (port
    :initarg :port
    :accessor port
    :initform 3140)))






