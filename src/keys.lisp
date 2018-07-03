(in-package :emotiq/config)

(defun keys/generate (records)
  "Generate keys for a list of plist RECORDS

The keys of the RECORDS plist are interpreted by gossip/config"
  (loop
     :with key-integers = (make-key-integers)
     :for record :in records
     :collecting (append record
                         ;;; HACK adapt to gossip/config needs
                         (unless (find :gossip-server-port record)
                           (list :gossip-server-port 65002))
                         (list :public (first key-integers))
                         (list :private (second key-integers)))))

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

  
