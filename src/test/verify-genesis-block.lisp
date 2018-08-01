(in-package :emotiq-config-generate-test)

(defun create-and-check-genesis-block ()
  (let ((d (emotiq/filesystem:new-temporary-directory)))
    (let* ((nodes
            (emotiq/config/generate::generate-keys
             '((:hostname "127.0.0.1" :ip "127.0.0.1"))))
           (stakes
            (emotiq/config/generate::generate-stakes
             (mapcar (lambda (plist)
                       (getf plist :public))
                     nodes)))
           (coinbase-public-address
            (getf (first nodes) :public))
           (keypair (pbc:make-keying-triple
                     coinbase-public-address
                     (getf (first nodes) :private)))
           (genesis-block              
              (cosi/proofs:create-genesis-block coinbase-public-address stakes)))
        (values
         (cosi-simgen:with-block-list ((list genesis-block))
           (cosi/proofs/newtx:get-balance (emotiq/txn:address keypair)))
         d
         keypair 
         coinbase-public-address))))

(defun test-base58 (&optional (niter 100))
  (loop repeat niter do
        (let* ((val (random (ash 1 256)))
               (b58  (vec-repr:base58-str val))
               (b582 (vec-repr:base58-str val)))
          (assert (string= b58 b582)))))

(defun run-verify-genesis-block-generate ()
  (let ((iterations 100))
    (emotiq:note "Generating ~a genesis blocks" iterations)
    (loop
      :repeat iterations
      :do (progn
            (multiple-value-bind (amount directory keypair coinbase-public-address)
                (create-and-check-genesis-block)
              (assert (equal amount
                             (cosi/proofs/newtx:initial-total-coin-amount)))
              (assert (equal (vec-repr:int (pbc:keying-triple-pkey keypair))
                             coinbase-public-address)))
            (format t ".")))))
  
(define-test verify-genesis-block-generate ()
  (let ((iterations 100))
    (emotiq:note "Generating ~a genesis blocks" iterations)
    (loop
      :repeat iterations
      :do (progn
            (multiple-value-bind (amount directory keypair coinbase-public-address)
                (create-and-check-genesis-block)
              (assert-true (equal amount
                                  (cosi/proofs/newtx:initial-total-coin-amount)))
              (assert-true (equal (vec-repr:int (pbc:keying-triple-pkey keypair))
                                  coinbase-public-address)))
            (format t ".")))))


