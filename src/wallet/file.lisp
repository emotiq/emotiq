(in-package emotiq/wallet)

(defun emotiq-wallet-path (&key (name *default-wallet-name*))
  "Return pathname of wallet with NAME"
  (merge-pathnames
   (format nil "wallet/~a/~a"
           (wallet-name-to-pathname-name name)
           "emotiq.wallet")
   (emotiq/filesystem:emotiq/user/root/)))


;;; We want to allow "human readable" wallet names that have a
;;; one-to-one mapping to valid directory names.  Using URI encoding
;;; ensures that we can have whitespace and slash characters in the
;;; filename.
(defun wallet-name-to-pathname-name (name)
  (quri:url-encode name))

(defun pathname-name-to-wallet-name (name)
  (quri:url-decode name))

(defun rename-wallet (from-name to-name)
  "Rename wallet FROM-NAME into TO-NAME

Returns nil if unsuccessful."
  (let ((from (emotiq-wallet-path :name from-name))
        (to (emotiq-wallet-path :name to-name)))
    (unless (probe-file from)
      (emotiq:note "Not renaming.~&No such wallet named '~a' already exists as '~a'."
              from-name
              from)
      (return-from rename-wallet nil))
    (when (probe-file to)
      (emotiq:note "Not renaming.~&A wallet named '~a' already exists as '~a'."
              to-name
              to)
      (return-from rename-wallet nil))
    (ensure-directories-exist to)
    (prog1
        (rename-file from to)
      (uiop:delete-empty-directory
       (make-pathname :name nil
                      :type nil
                      :defaults from)))))

(defun enumerate-wallets ()
  "Return a list of wallet names persisted on the local node"
  (let ((directories
         (directory 
          (merge-pathnames (make-pathname :directory '(:relative :up :wild))
                           (make-pathname :name nil :type nil
                                          :defaults (emotiq-wallet-path))))))
    (loop
       :for directory :in directories
       :collecting (pathname-name-to-wallet-name
                    (first (last (pathname-directory directory)))))))

;;;; TODO: ensure we encrypt when putting to disk
;;;; TODO: return the encrypted form of the wallet
(defun wallet-serialize (wallet &key (path (emotiq-wallet-path)))
  "Serialize WALLET object to PATH"
  (ensure-directories-exist path)
  (with-open-file
      (o path :direction :output :element-type '(unsigned-byte 8))
    (emotiq:note "Serializing wallet to ~a" path)
    (lisp-object-encoder:serialize wallet o)))
    
(defun wallet-deserialize (&key (path (emotiq-wallet-path)))
  "Deserialize wallet from file at PATH

Return nil if wallet cannot be found."
  (unless (probe-file path)
    (return-from wallet-deserialize
      (values nil
              path)))
  (with-open-file
      (o path :direction :input :element-type '(unsigned-byte 8))
    (lisp-object-encoder:deserialize o)))



