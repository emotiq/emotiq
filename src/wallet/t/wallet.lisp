(in-package :cl-user)
;;;; Test the publicly exported interface of the wallet

(prove:plan 2)
(let* ((existing-wallet-names (emotiq/wallet:enumerate-wallet-names))
       (new-wallet-name
        (loop
           :for name = (format nil "New wallet ~a" (random (expt 2 256)))
           :when (not (emotiq/wallet:get-wallet-named name))
           :return name))
       (wallet (emotiq/wallet:create-wallet :name new-wallet-name)))
  (prove:ok wallet
            (format nil "Created new wallet named ~a" new-wallet-name))
  (prove:ok (= (length (emotiq/wallet:enumerate-wallet-names))
               (1+ (length existing-wallet-names)))
            "New wallet appears in enumeration."))

(prove:finalize)
    
