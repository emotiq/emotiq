(in-package :emotiq-config-generate-test)
 
(defun create-genesis-block ()
  (let ((directory
         (emotiq/filesystem:new-temporary-directory)))
    (let* ((nodes (emotiq/config/generate::generate-keys
                   emotiq/config/generate::*dns-ip-zt.emotiq.ch*))
           (stakes (emotiq/config/generate::generate-stakes
                    (mapcar (lambda (plist)
                              (getf plist :public))
                            nodes)))
           (configuration 
            (emotiq/config/generate::make-configuration (first nodes) nodes stakes)))
      (emotiq/config/generate::generate-node directory configuration
                                             :key-records nodes)
      (let* ((genesis-block
              (emotiq/config:get-genesis-block
               :configuration configuration
               :root directory))
             (keypair
              (emotiq/config:get-nth-key 0 :root directory)))
        (values
         (cosi-simgen:with-block-list ((list genesis-block))
           (cosi/proofs/newtx:get-balance (emotiq/txn:address keypair)))
         directory)))))

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
     (assert-true (equal coinbase-amount
                         (cosi/proofs/newtx:initial-total-coin-amount)))))




  
