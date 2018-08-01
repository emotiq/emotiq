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
           (address-for-coins
            (getf (first nodes) :public))
           (keypair (pbc:make-keying-triple (getf (first nodes) :public) (getf (first nodes) :private)))
           (genesis-block              
              (cosi/proofs:create-genesis-block address-for-coins stakes
              )))

      (let ((amount (cosi-simgen:with-block-list ((list genesis-block))
           (cosi/proofs/newtx:get-balance (emotiq/txn:address keypair)))))
        (unless (equal amount (cosi/proofs/newtx:initial-total-coin-amount))
          (format t "bad amount ~A~&" amount)
          (cosi-simgen:with-block-list ((list genesis-block))
           (cosi/proofs/newtx:get-balance (emotiq/txn:address keypair)))))

        (values
         (cosi-simgen:with-block-list ((list genesis-block))
           (cosi/proofs/newtx:get-balance (emotiq/txn:address keypair)))
         d))))

(define-test verify-genesis-block-generate ()
  (progn 
    (pbc:init-pairing)
    (emotiq:note "Generating 100 genesis blocks~%")
    (cosi-simgen::with-current-node cosi-simgen::*my-node*
      (loop
       :repeat 100
       :do (progn
             (multiple-value-bind (amount directory)
                 (create-and-check-genesis-block)
               (if (equal amount (cosi/proofs/newtx:initial-total-coin-amount))
                   (format t "~&OK~&")
                 (format t "~&zero~&"))
               (assert-true (equal amount
                                   (cosi/proofs/newtx:initial-total-coin-amount))))
             (format t "."))
       ))
    ))
