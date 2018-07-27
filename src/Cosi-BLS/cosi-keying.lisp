;; cosi-keying.lisp -- Keying for Cosi networks
;;
;; DM/Emotiq 02/18
;; ----------------------------------------------------------------
#|
The MIT License

Copyright (c) 2018 Emotiq AG

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :cosi-keying)

;; NOTE: The adopted standard for Emotiq (for now) is:
;;    Hashes are hex strings, Binary data are base58 strings. And
;;    while keying uses hashing internally, published keying info is
;;    binary, not hashes. So key info will be presented as base64
;;    strings.
;;
;;    Users are identified by public key.

;; ---------------------------------------------------

;; ---------------------------------------------------
;; BLS Signatures

(defmethod cosi-sign (msg (pkey pbc:public-key) (skey pbc:secret-key))
  (pbc:sign-message msg pkey skey))

(defmethod cosi-validate-signed-message ((sm pbc:signed-message))
  (values (pbc:signed-message-msg sm)
          (pbc:check-message sm)))

;; --------------------------------------------
;; Keying for attribution. User keys, and signatures. You really don't
;; need such an elaborate system when generating keys for internal
;; math operations like for range proofs and such. And range proofs
;; also present special problems with respect to Elligator encodings.

(defun make-deterministic-keypair (seed)
  ;; WARNING!! This version is for testing only. Two different users
  ;; who type in the same seed will end up with the same keying. We
  ;; can't allow that in the released system.
  (pbc:make-key-pair seed))

;; --------------------------------------------------------------

(defun get-seed (seed)
  (vec-repr:bev
   (if seed
       (hash:hash/256 (vec-repr:lev
                  (loenc:encode seed)))
     (ctr-drbg 256))))

(defun get-salt (salt)
  (let ((pref (loenc:encode "salt")))
    (hash:hash-val
     (hash:hash/256
      (if salt
          (concatenate 'vector
                       pref 
                       (vec-repr:lev-vec (vec-repr:levn salt 32)))
        pref)))))

;; --------------------------------------------------------------
;; User level access functions for keying

(defun make-random-keypair (&optional seed salt)
  ;; seed can be anything at all, any Lisp object
  ;; This is the normal entry point when making new user keys.
  (let* ((seed  (get-seed seed))
         (salt  (get-salt salt))
         (rseed (ironclad:pbkdf2-hash-password (vec-repr:bev-vec seed)
                                               :salt       (vec-repr:bev-vec salt)
                                               :digest     :sha3
                                               :iterations 2048)))
    (make-deterministic-keypair rseed)))

(defmethod make-public-subkey ((pkey pbc:public-key) sub-seed)
  ;; Need to do one at a time, since unique may have created an index
  ;; value, and you can't retrace the path without that index value.
  ;; That is to say, you really do need to keep a record of all the
  ;; private keys along the way...
  (pbc:make-public-subkey pkey sub-seed))

(defmethod make-secret-subkey ((skey pbc:secret-key) sub-seed)
  (pbc:make-secret-subkey skey sub-seed))
  
(defmethod validate-pkey ((pkey pbc:public-key) (psig pbc:signature))
  ;; only works on primary keys. Can't validate subkeys because
  ;; signatures require secret key.
  (pbc:check-public-key pkey psig))
        
;; ------------------------------------------------------

;; Bloom filter sizing.... Suppose we want false positives rate
;; P_false < 0.001, number of hashes needed is K = -log2 p = 9.97 =
;; 10, and bits/item M/N = -1.44 Log2 p = 14.35. Suppose further, that
;; we plan for N = 1M items in the filter.
;;
;; We can chop up the bits of a hash like SHA256 for use as the
;; separate hash values, easier to do if we just use whole octets from
;; the hash directly. And keeping M as a power of 2, enables just
;; masking the hash value with 2^m-1 for indexing into the bit table..
;;
;; For 1M items, we need M = 14.35M bits => 16M or 24 bit addressing.
;; With 10 hashes, we need 240 bits of hash overall. And the bit table
;; will occupy 2 MB of memory.
;;
;; Pkeys are all at least 249 bits, so we are okay with just using the
;; Pkey as the source of hash values for the Bloom filter. We just
;; need to convert them into a little-endian vector of bytes.
;;
;; Note that no space is saved if we settle for p = 0.01 instead of
;; 0.001. The only thing that happens with higher false positive
;; probability is that we need only 7 hashes instead of 10.
;;

(defvar *pkeys* (maps:empty)) ;; for now...

(defmethod lookup-pkey ((pkey pbc:public-key))
  ;; return t if pkey found and valid, nil if not found or invalid
  (let ((psig (maps:find (vec-repr:int pkey) *pkeys*)))
    (when psig
      (validate-pkey pkey psig))))


(defun NYI (&rest args)
  (error "Not yet implemented ~A" args))

(defun #1=populate-pkey-database ()
  ;; this should populate *pkeys* and *pkey-filter* from the blockchain at startup
  (NYI '#1#))

(defun #1=refresh-pkey-database ()
  ;; periodically refresh the database from blockchain updates
  (NYI '#1#))


