(in-package :websocket/wallet)

(defvar *consensus-thread* nil)

(defun consensus (resource response)
  (let ((epoch (random 10000))
        (local-epoch 0)
        (iterations 10))
    (flet ((advance ()
             (setf epoch
                   (+ epoch (random 2)))
             (setf local-epoch
                   (+ local-epoch
                      (random (- epoch local-epoch))))))
      (loop 
         :for i :upto iterations
         ;;; Ensure that synchronization succeeds on the last iteration of the mock
         :when (= i iterations)
         :do (setf local-epoch epoch)
         :do
         (progn 
           (setf (slot-value response 'result)
                 `(:object
                   (:epoch . ,epoch)
                   (:local-epoch . ,local-epoch)
                   (:synchronized . ,(cl-json:json-bool (= local-epoch epoch)))))
           (broadcast
            resource
            (json:with-explicit-encoder
              (cl-json:encode-json-to-string response)))
           (sleep (random 5))
           (advance))))))

(defun transactions ()
  (let ((address (emotiq/wallet:primary-address (emotiq/wallet::wallet-deserialize))))
    `(:array
      (:object (:id . "C9746DE0C63763A650FCA")
               (:timestamp . 1527591324) (:type . "spend") (:epoch . 314) (:fee . 10)
               (:inputs .
                        (:array
                         (:object
                          (:cloaked . (:false))
                          (:address . "780C0C4D916FAF3357277A27C6C9746DE0C63763A650FCA37C8F1BF9440F017E")
                          (:amount . 10000))))
               (:outputs .
                         (:array
                          (:object
                           (:cloaked . (:false))
                           (:address . ,address)
                           (:amount . 9000))
                           (:object
                            (:cloaked . (:false))
                            (:address . "780C0C4D916FAF3357277A27C6C9746DE0C63763A650FCA37C8F1BF9440F017E")
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
                           (:address . "277A27C6C9746DE0C63763A650FCA37C8F1BF9440F017E780C0C4D916FAF3357")
                           (:amount . 400))
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
                           (:address . "277A27C6C9746DE0C63763A650FCA37C8F1BF9440F017E780C0C4D916FAF3357")
                           (:amount . 9000))
                          (:object
                           (:cloaked . (:false))
                           (:address . ,address)
                           (:amount . 1000))))))))



