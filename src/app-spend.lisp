(in-package :emotiq/app)

(defun submit-new-transaction (&key (from emotiq/app::*alice*)
                                    (address emotiq/app::*james*)
                                    (amount 10)
                                    (fee 10))
  "return a transaction as json, from and address are emotiq/app::account"
  (declare (ignorable from address amount))
  (emotiq-rest:as-json
   (convert-one-spend-transaction-to-alist-from-tuple
    (list
     (spend from address amount :fee fee)
     0 ;; timestamp - invalid invalid until published
     0 ;; epoch - invalid until published
     :spend
     fee))))