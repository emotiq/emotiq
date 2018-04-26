;;;; Simple wallet; unencrypted for now!

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
  (merge-pathnames "~/.emotiq/" (user-homedir-pathname))
  #+darwin
  (merge-pathnames "Emotiq/"
                   (merge-pathnames "Library/Application Support/"
                                    (user-homedir-pathname))))

(defun emotiq-wallet-path ()
  (merge-pathnames "emotiq.wallet" (emotiq/user/root)))
   
(defun wallet-serialize (wallet &key (path (emotiq-wallet-path)))
  (ensure-directories-exist path)
  (with-open-file
      (o path :direction :output :element-type '(unsigned-byte 8))
    (format *standard-output* "Serializing wallet to ~a" path)
    (lisp-object-encoder:serialize wallet o)))
    
(defun wallet-deserialize (&key (path (emotiq-wallet-path)))
  (unless (probe-file path)
    (error "No wallet found at ~a." path))
  (with-open-file
      (o path :direction :input :element-type '(unsigned-byte 8))
    (lisp-object-encoder:deserialize o)))

(defun create-wallet (&key
                        (path (emotiq-wallet-path))
                        (force nil))
  (when (and (probe-file path)
             (not force))
    (format *standard-output* "Not overwriting wallet keys at '~a'." path)
    (return-from create-wallet nil))
  (let ((wallet (make-wallet)))
    (wallet-serialize wallet :path path)))

(defun open-wallet (&key
                        (path (emotiq-wallet-path)))
  (unless (probe-file path)
    (format *standard-output* "Wallet has not been create (use create-wallet) '~a'." path)
    (return-from open-wallet nil))
  (let ((wallet (wallet-deserialize)))
    (declare (ignore wallet))))

(defun see-genesis () ;; triviality for debug, not exported
  (let ((context (emotiq::start-blockchain-context)))
    (emotiq:with-blockchain-context (context)
			     (let ((genesis-block (emotiq::make-genesis-block)))
			       (format *standard-output*
				       "~&Here is the first transaction of the genesis block:~%  ~a~%"
				       genesis-block)))))

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