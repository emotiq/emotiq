(in-package emotiq/wallet)

(defclass wallet ()
  ((salt
    :initarg :salt :accessor salt)
   (keying-triple
    :initarg :keying-triple :accessor keying-triple)
   (encrypted-private-key-p
    :initarg :encrypted-private-key-p :accessor encrypted-private-key-p)))

(defun make-wallet ()
  (make-instance 'wallet
                 :salt (ironclad:make-random-salt 16)
                 :keying-triple (cosi-keying:make-random-keypair)
                 :encrypted-private-key-p nil))

(defun emotiq/user/root ()
  #+linux
  (merge-pathnames ".emotiq/" (user-homedir-pathname))
  #+darwin
  (merge-pathnames "Emotiq/"
                   (merge-pathnames "Library/Application Support/"
                                    (user-homedir-pathname))))

(defvar *default-wallet-name* "My Wallet")

(defun create-wallet (&key
                        (name *default-wallet-name*)
                        (force nil))
  "Create an on disk wallet with NAME

If the wallet already exists, it will not be overwritten unless FORCE
is non-nil.

The default name for a wallet is *DEFAULT-WALLET-NAME*."
  (let ((path (emotiq-wallet-path :name name)))
    (when (and (probe-file path)
               (not force))
      (format *standard-output* "Not overwriting existing wallet at '~a'." path)
      (return-from create-wallet nil))
    (let ((wallet (make-wallet)))
      (values
       name
       (wallet-serialize wallet :path path)))))

(defun get-wallet-named (name)
  (wallet-deserialize :path (emotiq-wallet-path :name name)))

(defun rename-wallet (from-name to-name)
  "Rename wallet FROM-NAME into TO-NAME

Returns nil if unsuccessful."
  (let ((from (emotiq-wallet-path :name from-name))
        (to (emotiq-wallet-path :name to-name)))
    (unless (probe-file from)
      (format *standard-output* "Not renaming.~&No such wallet named '~a' already exists as '~a'."
              from-name
              from)
      (return-from rename-wallet nil))
    (when (probe-file to)
      (format *standard-output* "Not renaming.~&A wallet named '~a' already exists as '~a'."
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
          (merge-pathnames "../*/"
                           (make-pathname :name nil :type nil
                                          :defaults (emotiq-wallet-path))))))
    (loop
       :for directory :in directories
       :collecting (pathname-name-to-wallet-name
                    (first (last (pathname-directory directory)))))))

;;; We want to allow "human readable" wallet names that have a
;;; one-to-one mapping to valid directory names.  Using URI encoding
;;; ensures that we can have whitespace and slash characters in the
;;; filename.
(defun wallet-name-to-pathname-name (name)
  (quri:url-encode name))

(defun pathname-name-to-wallet-name (name)
  (quri:url-decode name))

(defun emotiq-wallet-path (&key (name *default-wallet-name*))
  "Return pathname of wallet with NAME"
  (merge-pathnames
   (format nil "wallet/~a/~a"
           (wallet-name-to-pathname-name name)
           "emotiq.wallet")
   (emotiq/user/root)))
   
(defun wallet-serialize (wallet &key (path (emotiq-wallet-path)))
  "Serialize WALLET object to PATH"
  (ensure-directories-exist path)
  (with-open-file
      (o path :direction :output :element-type '(unsigned-byte 8))
    (format *standard-output* "Serializing wallet to ~a" path)
    (lisp-object-encoder:serialize wallet o)))
    
(defun wallet-deserialize (&key (path (emotiq-wallet-path)))
  "Deserialize wallet from file at PATH"
  (unless (probe-file path)
    (error "No wallet found at ~a." path))
  (with-open-file
      (o path :direction :input :element-type '(unsigned-byte 8))
    (lisp-object-encoder:deserialize o)))

(defun primary-address (wallet)
  (vec-repr:hex-str
   (vec-repr:hex
   (pbc:keying-triple-pkey
    (emotiq/wallet:keying-triple wallet)))))

(defun key-phrase (wallet)
  (cosi-keying:convert-int-to-wordlist
   (vec-repr::convert-vec-to-int
    (vec-repr:bev-vec (secret-key wallet))
    :end 31)))
   
(defun public-key (wallet)
  (pbc:public-key-val
   (pbc:keying-triple-pkey
    (emotiq/wallet:keying-triple wallet))))

(defun secret-key (wallet)
  (pbc:secret-key-val
   (pbc:keying-triple-skey
    (emotiq/wallet:keying-triple wallet))))

#+(or)
(defun aes256-key (passphrase salt)
  (let ((kdf
         (ironclad:make-kdf :pbkdf2 :digest 'ironclad:sha3)))
    (ironclad:derive-key kdf passphrase salt 1024 32)))

#+(or)
(defun encrypt-wallet (wallet passphrase)
  (unless (not (encrypted-private-key-p wallet))
    (error "Private key already recorded as encrypted."))
  (let ((wallet-key (aes256-key passphrase (salt wallet)))
        (pkey (slot-value (slot-value wallet 'keying-triple) 'pbc-interface::pkey) 'pbc-interface::val))))

#+(or)
(defun decrypt-wallet (wallet passphrase))
