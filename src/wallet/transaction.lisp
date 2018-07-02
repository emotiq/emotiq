(in-package :emotiq/wallet)

(defun submit-transaction (transaction
                           &key
                             (wallet-name *default-wallet-name*)
                             (address nil))
  "Submit TRANSACTION from ADDRESS at WALLET-NAME to block chain"
  ;;; TODO something
  (values
   transaction
   wallet-name
   address))

(defun get-transaction (id
                        &key
                          (wallet-name *default-wallet-name*)
                          (address nil))
  "Query for the status of transaction ID from the perspective of WALLET-NAME with ADDRESS"
  (values
   id
   wallet-name
   address))
  
                             
