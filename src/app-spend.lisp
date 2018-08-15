(in-package :emotiq/app)

(defun submit-new-transaction (&key (from (emotiq/app::account-triple *alice*)) (address (emotiq/app::account-pkey *james*)) amount)
  "return a transaction as json"
  (declare (ignorable from address amount))
  (let ((standard-fee 10))
    (emotiq-rest:as-json
     (convert-one-spend-transaction-to-alist-from-tuple
      (list
       (spend from address amount :fee standard-fee)
       0 ;; timestamp - invalid invalid until published
       0 ;; epoch - invalid until published
       :spend
       standard-fee)))))