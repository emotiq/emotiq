(in-package "CL-USER")
(load-all-patches)
(load "~/quicklisp/setup.lisp")
(ql:quickload :emotiq)
(ql:quickload :crypto-pairings)
(ql:quickload :core-crypto)
(ql:quickload :main)

(deliver 'emotiq:production-start "emotiq" 0 :multiprocessing t :console t)
