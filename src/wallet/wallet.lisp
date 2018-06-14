(in-package emotiq/wallet)

(defclass wallet ()
  ((salt
    :initarg :salt :accessor salt)
   (keying-triple
    :initarg :keying-triple :accessor keying-triple)
   (encrypted-private-key
    :initarg :encrypted-wallet-secret
    :accessor encrypted-wallet-secret)
   (encrypted-private-key-p
    :initarg :encrypted-private-key-p :accessor encrypted-private-key-p)))

(defun make-wallet ()
  (make-instance 'wallet
                 :salt (ironclad:make-random-salt 16)
                 :keying-triple (cosi-keying:make-random-keypair)
                 :encrypted-private-key-p nil))

(defun copy-wallet (wallet)
  (with-slots (salt keying-triple encrypted-private-key-p encrypted-wallet-secret)
      wallet
    (make-instance 'wallet
                   :salt salt
                   :keying-triple keying-triple
                   :encrypted-wallet-secret encrypted-wallet-secret
                   :encrypted-private-key-p encrypted-private-key-p)))

(defvar *default-wallet-name* "My Wallet")

(defun create-wallet (&key
                        (name *default-wallet-name*)
                        (force nil))
  "Create an on disk wallet with NAME

If the wallet already exists, it will not be overwritten unless FORCE
is non-nil.

The default name for a wallet is *DEFAULT-WALLET-NAME*."
  (when (get-wallet-named name)
    (unless force
      (emotiq:note "Not overwriting existing wallet at '~a'." path)
      (return-from create-wallet nil)))
  (let ((path (emotiq-wallet-path :name name)))
    (let ((wallet (make-wallet)))
      (values
       name
       (wallet-serialize wallet :path path)))))

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

(defun enumerate-wallet-names ()
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

;;; We want to allow "human readable" wallet names that have a
;;; one-to-one mapping to valid directory names.  Using URI encoding
;;; ensures that we can have whitespace and slash characters in the
;;; filename.
(defun wallet-name-to-pathname-name (name)
  (quri:url-encode name))

(defun pathname-name-to-wallet-name (name)
  (quri:url-decode name))

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
    (emotiq:note "No wallet found at ~a." path)
    (return-from wallet-deserialize nil))
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

(defun ensure-octet-sequence (value)
  (cond
    ((typep value '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) *))
     value)
    ((some (lambda (type)
             (subtypep (type-of value) type))
           '((base-string *)
             (simple-array character *)))
     (ironclad:ascii-string-to-byte-array value))
    (t
     (error "Cannot convert ~a with type ~a to octet sequence." value (type-of value)))))
               
(defun derive-aes256-keying (passphrase salt)
  "Returns 48 bytes: 32 bytes for key; 16 bytes for IV."
  (let ((kdf (ironclad:make-kdf :pbkdf2 :digest 'ironclad:sha3)))
    (ironclad:derive-key
     kdf
     (ensure-octet-sequence passphrase)
     (ensure-octet-sequence salt)
     1024 48)))

;;; FIXME:  only encrypts the first 48 bytes of the secret key
(defun encrypt-wallet (wallet passphrase)
  "Encrypt all secrets in WALLET with PASSPHRASE.

Returns an in-memory copy of wallet stripped of private key."
  (unless (not (encrypted-private-key-p wallet))
    (error "Private key already recorded as encrypted."))
  (let* ((aes256-keying (derive-aes256-keying passphrase (salt wallet)))
         (key (subseq aes256-keying 0 32))
         (iv (subseq aes256-keying 32 48))
         (cipher (ironclad:make-cipher :aes :key key :initialization-vector iv :mode :cbc)) 
         (secret-key (secret-key wallet))
         (encrypted-key (make-array (let* ((length (length secret-key)))
                                      (+ length (mod length 16)))
                                    :element-type (unsigned-byte *)))
         (result (copy-wallet wallet)))
    (with-slots (encrypted-private-key-p encrypted-wallet-secret)
        result
      (ironclad:encrypt cipher
                        (vec-repr:bev-vec secret-key)
                        encrypted-wallet-secret)
      (setf encrypted-private-key-p t))
    result))
     
#+(or)
(defun decrypt-wallet (wallet passphrase))
