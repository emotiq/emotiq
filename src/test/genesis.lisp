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
           (address-for-coins
            (getf (first nodes) :public))
           (configuration 
            (emotiq/config/generate::make-configuration
             (first nodes)
             :address-for-coins address-for-coins
             :stakes stakes)))
      (emotiq/config/generate::generate-node d configuration
                                             :key-records nodes)
      (let* ((genesis-block
              (emotiq/config:get-genesis-block
               :configuration configuration
               :root d))
             (keypair
              (emotiq/config:get-nth-key 0 :root d)))
        (values
         (cosi-simgen:with-block-list ((list genesis-block))
           (cosi/proofs/newtx:get-balance (emotiq/txn:address keypair)))
         d)))))

(defun verify-genesis-block (&key (root (emotiq/fs:etc/)))
  (let* ((genesis-block
          (emotiq/config:get-genesis-block
           :configuration (emotiq/config:settings :root root)
           :root root))
         (keypair
          (emotiq/config:get-nth-key 0 :root root)))
    (values
     (cosi-simgen:with-block-list ((list genesis-block))
       (cosi/proofs/newtx:get-balance (emotiq/txn:address keypair)))
     root)))

(define-test genesis-block ()
  (multiple-value-bind (coinbase-amount directory)
      (create-genesis-block)
    (emotiq:note "Created genesis block with coinbase-amount ~a in '~a'."
                 coinbase-amount directory)
    (assert-true (equal coinbase-amount
                        (cosi/proofs/newtx:initial-total-coin-amount)))))




  
