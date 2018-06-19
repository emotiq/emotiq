(in-package :model/wallet)

;;; Objects than can be serialized via CL-JSON
(defun recovery-phrase (&key (wallet-named nil))
  (declare (ignore wallet-named))
  `(:object
    (:address . ,(emotiq/wallet:primary-address (emotiq/wallet::wallet-deserialize)))
    (:keyphrase . (:list ,@(emotiq/wallet::key-phrase (emotiq/wallet::wallet-deserialize))))))

(defun enumerate-wallets ()
  (emotiq/wallet:enumerate-wallets))
  
(defun get-wallet (name)
  `(:object
    (:address . ,(emotiq/wallet:primary-address (emotiq/wallet:get-wallet-named name)))
    (:amount . 0)))

(defun get-wallet-addresses (name)
  (let ((wallet (emotiq/wallet:get-wallet-named name)))
    `(:array
      ,(emotiq/wallet:primary-address wallet))))






