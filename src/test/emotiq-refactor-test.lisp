(in-package :emotiq-refactor-test)

(define-test blockchain-test ()
  (blockchain-test::run)
  (assert-eq (cosi::with-current-node node:*top-node*
               (blockchain:count-transactions))
             3))
