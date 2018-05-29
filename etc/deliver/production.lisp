(in-package "CL-USER")
(load-all-patches)
;; QuickLisp should be properly configured by now
(load "~/quicklisp/setup.lisp")

(ql:quickload :swank) ;; HACK: Needed for implicit dependency in the
                      ;; RESTAS dependency of EMOTIQ-REST
(ql:quickload :emotiq/startup)


(deliver 'emotiq:start "emotiq" 0 :multiprocessing t :console t)
