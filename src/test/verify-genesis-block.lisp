(in-package :emotiq-config-generate-test)

(defun create-and-check-genesis-block ()
  (let* ((nodes
          (emotiq/config/generate::generate-keys
           '((:hostname "127.0.0.1" :ip "127.0.0.1"))))
         (stakes
          (emotiq/config/generate::generate-stakes
           (mapcar (lambda (plist)
                     (getf plist :public))
                   nodes)))
         (coinbase-pkey
          (getf (first nodes) :public))
         (coinbase-keypair
          (pbc:make-keying-triple
           (getf (first nodes) :public)
           (getf (first nodes) :private)))
         (genesis-block
          (cosi/proofs:create-genesis-block coinbase-pkey stakes)))
    (values
     (cosi-simgen:with-block-list ((list genesis-block))
       (cosi/proofs/newtx:get-balance (emotiq/txn:address coinbase-keypair)))
     coinbase-keypair
     coinbase-pkey)))

(define-test verify-genesis-block-generate ()
  (let ((iterations 100))
    (emotiq:note "Generating ~a genesis blocks" iterations)
    (loop
      :repeat iterations
      :do (progn
            (multiple-value-bind (amount coinbase-keypair coinbase-public-address)
                (create-and-check-genesis-block)
              (assert-true (equal (vec-repr:int (pbc:keying-triple-pkey coinbase-keypair))
                                  (vec-repr:int coinbase-public-address)))
              (assert-true (equal amount
                                  (cosi/proofs/newtx:initial-total-coin-amount))))
            (format t ".")))))
