(in-package :emotiq/wallet-test)

(define-test wallet-serialization ()
   (let ((wallet (emotiq/wallet:make-wallet))
         (path #p"/var/tmp/wallet.emotiq"))
    
