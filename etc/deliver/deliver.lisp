(in-package "CL-USER")
(load-all-patches)
;; QuickLisp should be properly configured by now
(load "~/quicklisp/setup.lisp")

(defparameter cl-user::*performing-binary-build* :performing-binary-delivery)

(ql:quickload :emotiq/startup)

(deliver 'emotiq:start "emotiq" 0 :multiprocessing t :console t)
