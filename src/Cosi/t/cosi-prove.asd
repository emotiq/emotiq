;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

;;; TODO figure out how to get ql:quickload to satisfy
;;; :defsystem-depends-on clause *before* the ASDF definition is
;;; interpreted.
(defsystem "cosi-prove"
  :defsystem-depends-on (prove-asdf) 
  :depends-on (prove cosi)
  :perform (test-op (o c)
              (uiop:symbol-call :prove-asdf :run-test-system c))
  :components ((:module range
                        :pathname "./"
                        :components ((:test-file "base")
                                     (:test-file "ranges-timing")))))
