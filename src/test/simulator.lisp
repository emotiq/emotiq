(in-package :emotiq-sim-test)

(define-test simulated-chain ()
  (assert-true (emotiq/sim:initialize :nodes 3 :new-configuration-p t))
  (assert-true (emotiq/sim:run-new-tx))
  (assert-true (> (length (emotiq/sim:blocks))
                  1)))

             
