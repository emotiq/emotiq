(in-package emotiq/wallet)

(defclass wallet ()
  ((seed
    :documentation "32 byte for seed of wallet"
    :initarg :seed :accessor seed)
   (salt
    :documentation "16 bytes of salt for the passphrase"
    :initarg :salt :accessor salt)
   (keying-triple
    :initarg :keying-triple :accessor keying-triple)
   (encrypted-private-key
    :initarg :encrypted-wallet-secret
    :accessor encrypted-wallet-secret)
   (encrypted-private-key-p
    :initarg :encrypted-private-key-p :accessor encrypted-private-key-p)))

(defun make-wallet ()
  (let ((seed (ironclad:make-random-salt 32)))
    (make-wallet-from-seed seed)))

(defun make-wallet-from-seed (seed)
  (make-instance 'wallet
                 :seed seed
                 :salt (ironclad:make-random-salt 16)
                 :keying-triple (cosi-keying:make-deterministic-keypair seed)
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
      (emotiq:note "Not overwriting existing wallet named '~a'." name)
      (return-from create-wallet nil)))
  (let ((path (emotiq-wallet-path :name name)))
    (let ((wallet (make-wallet)))
      (values
       name
       (wallet-serialize wallet :path path)))))

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

(defun primary-address (wallet)
  (emotiq/txn:address
   (pbc:keying-triple wallet)))

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
  (let* ((aes256-keying
          (derive-aes256-keying passphrase (salt wallet)))
         (key
          (subseq aes256-keying 0 32))
         (iv
          (subseq aes256-keying 32 48))
         (cipher
          (ironclad:make-cipher :aes :key key :initialization-vector iv
                                :mode :cbc)) 
         (secret-key
          (secret-key wallet))
         (encrypted-secret-key
          (make-array (let* ((length (length secret-key)))
                                      (+ length (mod length 16)))
                                    :element-type '(unsigned-byte *)))
         (result (copy-wallet wallet)))
    (with-slots (encrypted-private-key-p encrypted-wallet-secret)
        result
      (ironclad:encrypt cipher
                        (vec-repr:bev-vec secret-key)
                        encrypted-secret-key)
      (setf encrypted-private-key-p t
            encrypted-wallet-secret encrypted-secret-key))
    result))
     
#+(or)
(defun decrypt-wallet (wallet passphrase))
