(in-package :emotiq-sim-test)

(define-test simulated-chain ()
  (assert-true (emotiq/sim:initialize :nodes 3 :new-configuration-p t))
  (assert-true (emotiq/sim:run-new-tx))
  ;; --- NEED TO WAIT HERE.. ---
  ;; run-new-tx exits early due to async Actors taking over for the work.
  ;; (blocks) contains nothing until the run gets further along
  (sleep 10)
  (assert-true (> (length (emotiq/sim:blocks))
                  1)))

             
