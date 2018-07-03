(defpackage :emotiq/model/wallet
  (:use :cl)
  (:nicknames :model/wallet)
  (:export
   #:mock
   #:transactions
   #:recovery-phrase
   #:get-wallet
   #:enumerate-wallets
   #:get-wallet-addresses)
  (:export
   #:enumerate-dictionaries
   #:get-dictionary))

  

