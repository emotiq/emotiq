(in-package :emotiq-sim-test)

(defun simulated-chain-fn ()
  (and (emotiq/sim:initialize :nodes 3 :new-configuration-p t)
       (emotiq/sim:run-new-tx)
       (> (length (emotiq/sim:blocks))
          1)))

(define-test simulated-chain ()
  (assert-true (simulated-chain-fn)))

#|
(define-test simulated-chain ()
  (assert-true (emotiq/sim:initialize :nodes 3 :new-configuration-p t))
  (assert-true (emotiq/sim:run-new-tx))
  (assert-true (> (length (emotiq/sim:blocks))
                  1)))
|#

             
