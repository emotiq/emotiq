(in-package :emotiq/wallet)
;;; Test aspects of the internal interface

(prove:plan 2)

(let ((wallet (make-wallet)))
  (prove:is-type wallet
                 'wallet)
  (let ((passphrase "k1llr0y")
        (salt (ironclad:make-random-salt)))
    (let ((keying (derive-aes256-keying passphrase salt)))
      (prove:is-type keying
                     '(simple-array (unsigned-byte 8) (48))
                     "Derived AES256 keying is 48 unsigned bytes…"))))

(prove:plan 2)
(let* ((seed (ironclad:make-random-salt 32))
       (wallet-1 (emotiq/wallet::make-wallet-from-seed seed))
       (wallet-2 (emotiq/wallet::make-wallet-from-seed seed)))
  (prove:is (hash:hash=
             (hash:hash/256
              (pbc:keying-triple-pkey (emotiq/wallet:keying-triple wallet-1)))
             (hash:hash/256
              (pbc:keying-triple-pkey (emotiq/wallet:keying-triple wallet-2))))
            "Wallets from the same seed have the same public keys…")
  (prove:ok (hash:hash=
             (hash:hash/256
              (pbc:keying-triple-skey (emotiq/wallet:keying-triple wallet-1)))
             (hash:hash/256
              (pbc:keying-triple-skey (emotiq/wallet:keying-triple wallet-2))))
            "Wallets from the same seed have the same private keys…"))

(prove:finalize)
