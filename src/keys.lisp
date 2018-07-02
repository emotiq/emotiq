(in-package :emotiq/config)

(defun keys/generate (records)
  "Generate keys for a list of alist RECORDS, destructively"
  (loop
     :with key-integers = (make-key-integers)
     :for record :in records
     :doing (push `(:public
                    . ,(format nil "~a" (first key-integers)))
                  record)
     :doing (push `(:private
                    . ,(format nil "~a" (second key-integers)))
                  record)
     :collecting record))


(defun make-key-integers ()
  "Makes a public/private keypair seeded via UUID:MAKE-V1-UUID

Returns a list of of the generated public and private keys."
  (let ((keypair (pbc:make-key-pair (list :lisp-authority (uuid:make-v1-uuid)))))
    (list
     ; public is first
     (vec-repr:int
      (pbc:keying-triple-pkey keypair))
     ; private is second
     (vec-repr:int
      (pbc:keying-triple-skey keypair)))))
