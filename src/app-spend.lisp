(in-package :emotiq/app)

(defun submit-new-transaction (&key from address amount)
  "return a transaction as json"
  (declare (ignorable from address amount))
  (let ((fee 10))
    (emotiq-rest:as-json
     (convert-one-spend-transaction-to-alist-from-tuple
      (list
       (spend *alice* *james* 90 :fee fee)
       0 ;; timestamp - invalid invalid until published
       0 ;; epoch - invalid until published
       :spend
       fee)))))