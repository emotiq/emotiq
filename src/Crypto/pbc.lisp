(in-package :pbc)

(defun make-key-pair-uuid ()
  "Make a pairing based curve keypair seeded via uuid:make-v1-uuid"
  ;;; Unclear at this point what seeding via a UUID
  ;;; Originally the method used by gossip/config to create keypairs for test network
  (pbc:make-key-pair (list :lisp-authority (uuid:make-v1-uuid))))

(defun make-keying-triple (public-key secret-key &optional signature)
  "DEPRECATED Make a PBC:KEYING-TRIPLE from its integer representation of PUBLIC-KEY and SECRET-KEY"
  (make-instance 'pbc:keying-triple
                 :pkey public-key
                 :skey secret-key
                 :sig (or signature (pbc:sign-hash (hash:hash/256 public-key) secret-key))))
