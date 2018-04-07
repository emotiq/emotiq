(in-package "CL-USER")
(load-all-patches)
(load "~/quicklisp/setup.lisp")

(ql:quickload :emotiq/startup)

(deliver 'emotiq:start "emotiq" 0 :multiprocessing t :console t)
