(defsystem "transport"
  :name "Transport"
  :description "Transport of asynchronous messages over a network."
  :author "Luke Gorrie <luke@emotiq.ch>"
  :version "0.1.0"
  :maintainer "Luke Gorrie <luke@emotiq.ch>"
  :depends-on (mpcompat usocket cl-async useful-macros)
  :serial t
  :components ((:file "package")
               (:file "transport")
               (:file "transport-blocking")
               (:file "transport-async")
               (:file "transport-test"))
#|
  XXX Get lisp-unit working
  :perform (test-op (o s)
                    (symbol-call :lisp-unit :run-tests :all :transport))
|#
  )

