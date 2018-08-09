;;;; address.lisp

(in-package :cosi/proofs)



;;;; Addresses: Public Key Hashes in Emotiq

;;; Hashed public keys are presented as `addresses'.  An `address' for a public
;;; key is in general produced as follows:
;;;
;;; First, it is hashed irreversibly, and then encoded in a user friendly
;;; manner, that is, shorter; with characters easily typed, read, and
;;; distinguishable by humans in nearly all computing environments; with a
;;; checksum to catch the vast majority of simple mistakes in typing; and with a
;;; "version" (or "network") prefix that helps identify which use it is for (and
;;; also which cryptocurrency it's for (e.g., where Bitcoin uses 0x00, Litecoin
;;; uses 0x30).

;;; As many other cryptocurrencies have done, we simply adopt the Bitcoin
;;; specification for addresses, known as Base58Check Encoding. The only thing
;;; that we makes our addresses distinct is that we use unique version/network
;;; prefix. (We hope ours is unique. There does not seem to be any central
;;; registry.  If there is, we do not know of one at this time, May 27, 2018.)

;;; So for the Bitcoin address the details are as follows. First we hash the
;;; public key with SHA2/256. The resulting hash is shortened with a second hash
;;; of RIPEMD160, getting bits down to 160 or 20 bytes (octets).  The version
;;; prefix octet is prepended on to the front of the resulting hash result,
;;; viewed as an octet vector.  The resulting vector V gets a 4-element checksum
;;; vector appended containing elements created as follows: take a double
;;; sha2/256 hash of V, and take the first 4 elements of the result viewed as an
;;; octet vector, and then append that octet vector at the end of V. Finally,
;;; that entire result is base58-encoded as a string, yielding the final result.
;;;
;;; Here is a more concise summary:
;;;
;;;   Public Key => SHA-2/256 => RIPEMD160 => Base58Check Encode version prefix

;;; The byte (octet) layout ends up as follows:
;;;
;;;   [version] [RIPEMD160 hash] [checksum]
;;;       1           20              4


;;; The constants +mainnet-version-for-public-key-to-address+ and
;;; +testnet-version-for-public-key-to-address+ are to be used for mainnet and
;;; testnet deployments, respectively, as the version (network) prefix octet.

;;; The values chosen are somewhat arbitrary but with the following constraints:
;;;
;;;   (1) cannot conflict with prominent well-known Bitcoin values, notably
;;;
;;;         0x00 0x03 0x6F 0x80 0x0142 0x6F 0x00;
;;;
;;;   (2) ideally, ought not to conflict other prominant cryptocurrencies'
;;;   established mainnet p2pkh version prefixes, notably
;;;
;;;     Lightcoin: 176; Doge: 30; Dash: 76
;;;
;;;   (3) initial character of resulting address string must be unique between
;;;   our mainnet and testnet, e.g., mainnet starts with "2" and testnet starts
;;;   with "y".

;;; The parameter *default-net-for-public-key-to-address*, which defaults to
;;; :TEST, says which one is to be preferred if neither is specified.  It should
;;; be one of two values: :TEST or :MAIN. Although the default for this is
;;; currently :TEST, this may be changed in the future to :MAIN.

(defparameter +mainnet-version-for-public-key-to-address+ #xE0
  ;; 224 decimal (address starts with "2", ... [others?])
  "Version prefix octet for mainnet public key hash address creation.")

(defparameter +testnet-version-for-public-key-to-address+ #x8d
  ;; 141 decimal (address starts with "y", ... [others?])
  "Version prefix octet for mainnet public key hash address creation.")

(defparameter *default-net-for-public-key-to-address* :test
  "Which net to prefer creating addresses for public keys, :test or :main.")

;; Figure out the how you figure out the possible starting characters and
;; document them! Also, check if there are any conflicts with Bitcoin or other
;; currencies to be concerned with. NB: currently defaulting to testnet. Later,
;; consider changing later in project to mainnet. -mhd, 5/27/18

(defun encode-address (hash160 version-octet)
  "Takes HASH160, a 20 byte RIPEMD160 hash of a public key, and a version octet,
  returns a corresponding string in Bitcoin address Base58check format."
  (let* ((prefix+data
           (concatenate
            'ub8-vector `#(,version-octet) (hash:hash-bytes hash160)))
         (checksum-vec
          (checksum-hash-address prefix+data))
         ;; tack that checksum onto the end of the prefix+data
         (prefix+data+checksum
           (concatenate 'ub8-vector prefix+data checksum-vec)))
    (pbc:addr prefix+data+checksum)))


(defun checksum-hash-address (prefix+data)  
  "Get a checksum for PREFIX+DATA, an octet vector composed of a single octet
   with the version prefix followed by the 20 octets of hash data. This returns
   an octet vector of the first 4 bytes of the double sha2/256 hash of
   prefix+data."
  (subseq (hash:hash-bytes
           (hash:hash/sha2/256 (hash:hash/sha2/256 prefix+data)))
          0 4))



(defmethod public-key-to-address ((public-key pbc:public-key) &key net override-version)
  "Produce an address for PUBLIC-KEY. Keyword :NET can be either :MAIN for
   mainnet, :TEST for testnet, or nil to default.  The version prefix is usually
   determined by the net. However, if OVERRIDE-VERSION is specified non-nil, it
   should be a version prefix octet to be used, and in that case it is used
   instead. This is intended to be used as a testing and debugging feature."
  (encode-address
   (hash:hash/ripemd/160 (hash:hash/sha2/256 public-key))
   (or override-version
       (ecase (or net *default-net-for-public-key-to-address*)
         (:main +mainnet-version-for-public-key-to-address+)
         (:test +testnet-version-for-public-key-to-address+)))))

;; Note: the :override-version keyword lets you take a public key and version
;; prefix and test results from any crypto currency and compare results. See
;; t/address-tests.lisp for examples of usage and additional test notes.


;; Background and Additional notes:

;; Useful reference: a survey of various cryptocurrencies and their address formats
;; 
;;   https://blockgeeks.com/guides/blockchain-address-101/
;;
;; Here's what they are for Bitcoin:
;;
;; Bitcoin Address: 0x00
;; Pay to Script Hash address: 0x03
;; Bitcoin testnet address: 0x6F
;; Private key WIF: 0x80
;; BIP-38 Encrypted private key: 0x0142

;; Here's a longer exhaustive list:
;;
;;   https://en.bitcoin.it/wiki/List_of_address_prefixes
;;
;; Bitcoin reference on Base 58:
;;
;;   https://en.bitcoin.it/wiki/Base58Check_encoding#Creating_a_Base58Check_string
