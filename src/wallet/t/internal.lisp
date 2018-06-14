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

(prove:finalize)
