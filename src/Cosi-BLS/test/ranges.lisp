(in-package :cosi-bls-test)

(define-test random-range-proof
  (let ((n (random (ash 1 64)))) ;; 0 <= n < 2^64
    (assert-true (range-proofs:make-range-proof n))))
