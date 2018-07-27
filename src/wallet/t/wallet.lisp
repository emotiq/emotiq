(in-package :wallet-test)

;;;; Test the publicly exported interface of the wallet

(define-test create-wallet ()
  (let* ((existing-wallet-names (emotiq/wallet:enumerate-wallets))
         (new-wallet-name
          (loop
             :for name = (format nil "New wallet ~a" (random (expt 2 256)))
             :when (not (emotiq/wallet:get-wallet-named name))
             :return name))
         (wallet (emotiq/wallet:create-wallet :name new-wallet-name)))
    (assert-true (not (null wallet)))
    (assert-true (= (length (emotiq/wallet:enumerate-wallets))
                    (1+ (length existing-wallet-names))))))

             
