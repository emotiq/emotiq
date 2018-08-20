(in-package :wallet-test)

;;;; Test the publicly exported interface of the wallet

;;; TODO Seems substantially more complicated than it should be 
(defmacro with-wallet-directory ((directory-form) &body body)
  (let ((original-fdefinition-name (gensym))
        (symbol-name (gensym))
        (directory-form-name (gensym)))
    `(let* ((,symbol-name
             'emotiq/fs:emotiq/wallet/)
            (,original-fdefinition-name
             (symbol-function ,symbol-name))
            (,directory-form-name ,directory-form))
       (setf (symbol-function ,symbol-name)
             (lambda () ,directory-form-name))
       (multiple-value-prog1
           (progn ,@body)
         (setf (symbol-function ,symbol-name) ,original-fdefinition-name)))))

(define-test wallet-creation ()
  (with-wallet-directory ((emotiq/fs:new-temporary-directory))
    (let* ((new-wallet-name
            (loop
               :for name = (format nil "New wallet ~a" (random (expt 2 256)))
               :when (not (emotiq/wallet:get-wallet-named name))
               :return name))
           (wallet (emotiq/wallet:create-wallet :name new-wallet-name)))
      (assert-true (not (null wallet)))
      (multiple-value-bind (wallet-names name-pathname-alist)
          (emotiq/wallet:enumerate-wallets)
        (assert-true
         (find new-wallet-name wallet-names :test 'string-equal))
        (loop
           :for (name . pathname) :in name-pathname-alist
           :doing (assert-true pathname))))))




