(in-package "CL-USER")

(load-all-patches)

(load "~/quicklisp/setup.lisp")
(ql:quickload :emotiq)
(ql:quickload :crypto-pairings)
(ql:quickload :simple-test)
(ql:quickload :core-crypto)

;; when actually delivering, change the symbol 'simple-test to whatever needs to be called at the top
;; level when the binary is executed

(deliver 'simple-test "emotiq" 0 :multiprocessing t :console t)
