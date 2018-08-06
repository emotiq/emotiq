(in-package :model/wallet)

(defun recovery-phrase (&key (wallet-named nil))
  "Return the recovery phrase for WALLET-NAMED"
  (declare (ignore wallet-named))
  `(:object
    (:address . ,*wallet-address*)
              ;;,(emotiq/wallet:primary-address (emotiq/wallet::wallet-deserialize))
    (:keyphrase . (:list ,@(emotiq/wallet::key-phrase (emotiq/wallet::wallet-deserialize))))))

(defun enumerate-wallets ()
  "Enumerate all wallets available at the node"
  (emotiq/wallet:enumerate-wallets))
  
(defun get-wallet (name)
  "Return the representation of wallet with NAME at the node"
  (declare (ignore name))
  `(:object 
    (:address . ,*wallet-address*)
    (:amount . ,*amount*)))

(defun get-wallet-addresses (name)
  "Return the addresses used by wallet NAME"
  (let ((wallet (emotiq/wallet:get-wallet-named name)))
    `(:array
      ,(emotiq/wallet:primary-address wallet))))

