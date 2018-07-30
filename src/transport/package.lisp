(defpackage :transport
  (:use :cl)
  (:export
   ;; Public API:
   #:start-transport
   #:stop-transport
   #:transmit
   ;; Generic functions for backend to extend:
   #:start
   #:stop
   #:transmit-message
   ;; Support functions for backend to call:
   #:+magic+
   #:my-endpoint
   #:run-message-received-hook
   #:run-peer-up-hook
   #:run-peer-down-hook
   #:run-transport-failed-hook
   #:log-event
   ))

(defpackage :transport/blocking
  (:use :cl :transport))

(defpackage :transport/async
  (:use :cl :transport))

(defpackage :transport/test
  (:use :cl :transport)
  (:export #:test-open-close #:test-mesh))

