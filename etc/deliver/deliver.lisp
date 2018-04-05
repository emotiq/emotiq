(in-package "CL-USER")
(load-all-patches)
(load "~/quicklisp/setup.lisp")
(ql:quickload :emotiq)
(ql:quickload :emotiq/startup)

(deliver 'emotiq:production-start "emotiq" 0 :multiprocessing t :console t)
