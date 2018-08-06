(in-package :emotiq-config-generate-test)
 
(defun create-genesis-block ()
  (let ((d (emotiq/filesystem:new-temporary-directory)))
    (let* ((nodes
            (emotiq/config/generate::generate-keys
             emotiq/config/generate::*eg-config-zerotier*))
           (stakes
            (emotiq/config/generate::generate-stakes
             (mapcar (lambda (plist)
                       (getf plist :public))
                     nodes)))
           (public-key-for-coins
            (getf (first nodes) :public))
           (coinbase-keypair
            (pbc:make-keying-triple
             public-key-for-coins (getf (first nodes) :private)))
           (configuration 
            (emotiq/config/generate::make-configuration
             (first nodes)
             :address-for-coins public-key-for-coins
             :stakes stakes)))
      (emotiq/config/generate::generate-node d configuration
                                             :key-records nodes)
      (let* ((genesis-block
              (emotiq/config:get-genesis-block
               :root d))
             (keypair
              (emotiq/config:get-nth-key 0 :root d)))
        (values
         (cosi-simgen:with-block-list ((list genesis-block))
           (cosi/proofs/newtx:get-balance (emotiq/txn:address keypair)))
         d
         coinbase-keypair)))))

(defun verify-genesis-block (&key (root (emotiq/fs:etc/)))
  (let* ((genesis-block
          (emotiq/config:get-genesis-block
           :root root))
         (keypair
          (emotiq/config:get-nth-key 0 :root root)))
    (values
     (cosi-simgen:with-block-list ((list genesis-block))
       (cosi/proofs/newtx:get-balance (emotiq/txn:address keypair)))
     root)))

(define-test genesis-block ()
   (multiple-value-bind (coinbase-amount directory coinbase-paid-to-keypair)
       (create-genesis-block)
     (emotiq:note "Created genesis block with coinbase paid ~a EMTQ to ~a~%~tin '~a'."
                  coinbase-amount
                  (emotiq/txn:address coinbase-paid-to-keypair)
                  directory)
     (multiple-value-bind (bal root)
         (verify-genesis-block)
       (emotiq:note "genesis balance ~A" bal)
       (assert-true (equal coinbase-amount bal))
       (assert-true (equal coinbase-amount
                           (cosi/proofs/newtx:initial-total-coin-amount))))))




  
