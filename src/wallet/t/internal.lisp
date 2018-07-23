(in-package :wallet-test)

;;;; Test aspects of the internal interface

(define-test wallet-passphrase-keying ()
  (let ((wallet (make-wallet)))
    (assert-true (typep wallet
                        'emotiq/wallet::wallet))
    (let ((passphrase "k1llr0y")
          (salt (ironclad:make-random-salt)))
      (let ((keying (emotiq/wallet::derive-aes256-keying passphrase salt)))
        ;; "Derived AES256 keying is 48 unsigned bytes…"
        (assert-true (typep keying
                            '(simple-array (unsigned-byte 8) (48))))))))

(define-test keying-triple-seeding ()
  (let* ((seed (ironclad:make-random-salt 32))
         (wallet-1 (emotiq/wallet::make-wallet-from-seed seed))
         (wallet-2 (emotiq/wallet::make-wallet-from-seed seed)))
    ;;; "Wallets from the same seed have the same public keys…")
    (assert-true (hash:hash=
                  (hash:hash/256
                   (pbc:keying-triple-pkey
                    (emotiq/wallet:keying-triple wallet-1)))
                  (hash:hash/256
                   (pbc:keying-triple-pkey
                    (emotiq/wallet:keying-triple wallet-2)))))
    ;;; "Wallets from the same seed have the same private keys…"))
    (assert-true (hash:hash=
                  (hash:hash/256
                   (pbc:keying-triple-skey
                    (emotiq/wallet:keying-triple wallet-1)))
                  (hash:hash/256
                   (pbc:keying-triple-skey
                    (emotiq/wallet:keying-triple wallet-2)))))))

 
