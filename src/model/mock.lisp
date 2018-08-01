;;;; Wherein we mock the reports of consensus status from a modeled EMTQ chain
(in-package :model/wallet)

(defun mock (notify-hook)
  "Run a service that sends json messages to the function of one argument specified via NOTIFY-HOOK"
  (let ((epoch (+ 1000 (random 10000)))
        (iterations-until-sync 2)
        (i 0))
    (flet ((advance ()
             (setf epoch
                   (+ epoch (random 4)))
             (setf *local-epoch*
                   (if (> i iterations-until-sync)
                       epoch
                       (+ *local-epoch*
                          (random (- epoch *local-epoch*)))))
             (setf i (1+ i))))
      (loop
         :do (let ((notification
                    `(:object
                      (:epoch . ,epoch)
                      (:local-epoch . ,*local-epoch*)
                      (:synchronized . ,(cl-json:json-bool (= *local-epoch* epoch))))))
               (funcall notify-hook notification))
         :do (sleep (random 2))
         :do (advance)))))

(defun current-posix-time ()
  (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))

(defun new-transaction-id ()
  (gensym))

(defun submit-new-transaction (&key (address "Payee address") (amount 1000))
  (setf *amount* (+ *fee*
                    (- *amount* amount)))
  (let ((txn (create-new-spend-transaction :address address :amount amount)))
    (setf *transactions* `(:array
                           ,@(rest *transactions*)
                           ,txn))))

(defun create-new-spend-transaction (&key (address "Payee address") (amount 1000))
  `(:object (:id . ,(new-transaction-id))
            (:timestamp . ,(current-posix-time))
            (:type . "spend")
            (:epoch . ,*local-epoch*)
            (:fee . ,*fee*)
            (:inputs
             . (:array
                (:object
                 (:cloaked . (:false))
                 (:address . ,*wallet-address*)
                 (:amount . ,amount))))
            (:outputs
             . (:array
                (:object
                 (:cloaked . (:false))
                 (:address . ,address)
                 (:amount . ,amount))
                (:object
                 (:cloaked . (:false))
                 (:address . "my wallet address")
                 (:amount . 1))))))

(defun transactions ()
  *transactions*)

(defparameter *local-epoch*
  0)
(defparameter *amount*
  (expt 10 11))
(defparameter *wallet-address*
  "ytD4ccGjrA1hRgYXiEvA8GMS8s9NK553Q2")
(defparameter *fee*
  10)
(defparameter *transactions*
  (let ((address "ytD4ccGjrA1hRgYXiEvA8GMS8s9NK553Q2"))
    `(:array
      (:object (:id . "C9746DE0C63763A650FCA")
               (:timestamp . 1527591324) (:type . "spend") (:epoch . 314) (:fee . 10)
               (:inputs .
                        (:array
                         (:object
                          (:cloaked . (:false))
                          (:address . ,address)
                          (:amount . 10000))))
               (:outputs .
                         (:array
                          (:object
                           (:cloaked . (:false))
                           (:address . ,address)
                           (:amount . 9000))
                          (:object
                           (:cloaked . (:false))
                           (:address . "hRgYXiEvA8GMS8s9NK553Q2123123")
                           (:amount . 1000)))))
      (:object (:id . "277A27C6C9746DE0C63763A650FCA")
               (:timestamp . 1527691324) (:type . "spend") (:epoch . 315) (:fee . 10)
               (:inputs .
                        (:array
                         (:object
                          (:cloaked . (:false))
                          (:address . ,address)
                          (:amount . 500))))
               (:outputs .
                         (:array
                          (:object
                           (:cloaked . (:false))
                           (:address . "hRgYXiEvA8GMS8s9NK553Q2123123")
                           (:amount . 400))
                          (:object
                           (:cloaked . (:false))
                           (:address . ,address)
                           (:amount . 100)))))
      (:object (:id . "277A27C6C9746DE0C63763A6CA")
               (:timestamp . 1527691324) (:type . "spend") (:epoch . 315) (:fee . 10)
               (:inputs .
                        (:array
                         (:object
                          (:cloaked . (:false))
                          (:address . ,address)
                          (:amount . 50000))))
               (:outputs .
                         (:array
                          (:object
                           (:cloaked . (:false))
                           (:address . "ypsrE7nAMx96T5QavqLxoLVp26MMpGEQSZ")
                           (:amount . 40000))
                          (:object
                           (:cloaked . (:false))
                           (:address . "hRgYXiEvA8GMS8s9NK553Q2123123")
                           (:amount . 4000))
                          (:object
                           (:cloaked . (:false))
                           (:address . ,address)
                           (:amount . 100)))))
      (:object (:id . "9746DE0C63763A650FCAFF277A27C6C")
               (:timestamp . 1527791324) (:type . "spend") (:epoch . 320) (:fee . 10)
               (:inputs .
                        (:array
                         (:object
                          (:cloaked . (:false))
                          (:address . ,address)
                          (:amount . 10000))))
               (:outputs .
                         (:array
                          (:object
                           (:cloaked . (:false))
                           (:address . "ywo9Y6yRrHhKqsJyuVcEWj5H1df1MKZMbe")
                           (:amount . 9000))
                          (:object
                           (:cloaked . (:false))
                           (:address . ,address)
                           (:amount . 1000))))))))
