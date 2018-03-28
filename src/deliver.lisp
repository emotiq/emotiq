(in-package "CL-USER")

(load-all-patches)

(load "~/quicklisp/setup.lisp")
(ql:quickload :emotiq)
(ql:quickload :crypto-pairings)
(ql:quickload :simple-test)
(ql:quickload :core-crypto)

(deliver 'simple-test "emotiq" 0 :multiprocessing t :console t)
