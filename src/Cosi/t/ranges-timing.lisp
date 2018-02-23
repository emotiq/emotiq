(in-package cl-user)

(prove:plan 1)
(let ((n 128) ;; ?? What is a reasonable range of values
      (nbits 64))
  (let* ((prover (range-proofs:make-range-prover :nbits nbits))
         (proof (funcall prover n)))
    (prove:ok
     (range-proofs:validate-range-proof proof)
     "Testing VALIDATE-RANGE-PROOF…")))

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
