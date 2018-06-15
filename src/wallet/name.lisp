(in-package emotiq/wallet)

(defvar *default-wallet-name* "My Wallet")

(defun get-wallet-named (name)
  (values
   (wallet-deserialize :path (emotiq-wallet-path :name name))
   name))

                      

