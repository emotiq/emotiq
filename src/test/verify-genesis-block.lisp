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
        (values
         (cosi-simgen:with-block-list ((list genesis-block))
           (cosi/proofs/newtx:get-balance (emotiq/txn:address keypair)))
         d))))

(define-test verify-genesis-block-generate ()
  (progn 
    (emotiq:note "Generating 100 genesis blocks~%")
    (loop
      :repeat 100
      :do (progn
            (multiple-value-bind (amount directory)
                (create-and-check-genesis-block)
              (assert-true (equal amount
                                  (cosi/proofs/newtx:initial-total-coin-amount))))
            (format t "."))
    ))
)
