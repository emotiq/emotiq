(defpackage :emotiq/model/wallet
  (:use :cl)
  (:nicknames :model/wallet)
  (:export
   #:mock
   #:transactions-from-chain
   #:recovery-phrase
   #:get-wallet
   #:enumerate-wallets
   #:get-wallet-addresses)
  (:export
   #:enumerate-dictionaries
   #:get-dictionary))

  

