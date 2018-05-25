(in-package "CL-USER")
(load-all-patches)
;; QuickLisp should be properly configured by now
(load "~/quicklisp/setup.lisp")

(push :delivery *features*)
(format *standard-output* "~&features: ~A~&" *features*)
(ql:quickload :emotiq/startup)

(deliver 'emotiq:start "emotiq" 0 :multiprocessing t :console t)
