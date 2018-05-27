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


        

     
            
