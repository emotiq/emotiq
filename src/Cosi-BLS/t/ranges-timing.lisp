(in-package cl-user)

(prove:plan 2)
(prove:is-error
 (let ((n (random (ash 1 64)))) ;; 0 <= n < 2^64
   (let* ((proof (range-proofs:make-range-proof n)))
     (prove:ok
      (range-proofs:validate-range-proof proof)
      "Testing VALIDATE-RANGE-PROOF…")))
 'error
 "Expecting badly hooked up make-range-prover to error…")

#|     TODO: finish transcribing…
(prove:plan 1)
(let ((niter 1000))
  (let ((result (multiple-value-list 
                 (time (loop repeat niter do
                            (tst (random-between 0 #.(1- (ash 1 64))) :nbits 64))))))
    
(defun proof-timing-test (&optional (niter 1000))
  (time (loop repeat niter do
              (let ((prover (make-range-prover :nbits 64)))
                (funcall prover (random-between 0 #.(1- (ash 1 64))))))))

(defun verifier-timing-test (&optional (niter 1000))
  (let* ((prover (make-range-prover :nbits 64))
         (proof  (funcall prover (random-between 0 #.(1- (ash 1 64))))))
    (time (loop repeat niter do
                (validate-range-proof proof)))))
|#

(prove:finalize)
