(in-package :pbc)

(defun make-keying-triple (public-key secret-key &optional signature)
  "Make a PBC:KEYING-TRIPLE from its integer representation of PUBLIC-KEY and SECRET-KEY"
  (let* ((pkey (pbc:public-key public-key))
         (skey (pbc:secret-key secret-key)))
    (make-instance 'pbc:keying-triple
                   :pkey pkey
                   :skey skey
                   :sig (or signature (pbc:sign-hash (hash:hash/256 pkey) skey)))))

(defun make-keying-integers ()
  "Makes a public/private keypair seeded via UUID:MAKE-V1-UUID

Returns a list of of the generated public and private keys as integers"
  (let ((keypair (pbc:make-key-pair (list :lisp-authority (uuid:make-v1-uuid)))))
    (list
     (vec-repr:int (pbc:keying-triple-pkey keypair))    ;; public is first
     (vec-repr:int (pbc:keying-triple-skey keypair))))) ;; private is second



