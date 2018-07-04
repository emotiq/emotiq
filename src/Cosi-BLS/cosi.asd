(defsystem cosi
  :depends-on nil)

(defsystem cosi/package
  :depends-on (actors
               core-crypto
               ads-clos
               crypto-pairings
               emotiq/logging
               ironclad
               lisp-object-encoder
               useful-macros
               usocket
               trivial-garbage
               gossip)
  :components ((:module package
                        :pathname "./"
                        :components ((:file "package")))))

(defsystem cosi/bls/dependencies
  :depends-on (actors
               core-crypto
               ads-clos
               crypto-pairings
               emotiq/logging
               ironclad
               lisp-object-encoder
               useful-macros
               usocket
               trivial-garbage
               gossip)
  :components nil)

(defsystem cosi/bls
  :depends-on (cosi/package
               cosi/bls/dependencies)
  :components (#+ccl
               (:module clozure
                        :pathname "./"
                        :components ((:file "clozure")))
               (:module source
                        :pathname "./"
                        :components ((:file "cosi-blkdef")
                                     (:file "cosi-keying")
                                     (:file "cosi-netw-xlat")))))

(defsystem cosi/simgen
  :depends-on (cosi/package
               cosi/proofs
               cosi/transactions
               cosi/proofs/newtx
               emotiq/config/stakes)
  :components ((:file "cosi-construction")
               (:file "cosi-handlers")
               (:file "mvp-election-beacon")))

(defsystem cosi/transactions
  :depends-on (cosi/bls
               cosi/proofs/range)
  :components ((:file "transaction")))


(defsystem cosi/proofs/range
  :depends-on (cosi/package)
  :components ((:file "range-proofs")))


(defsystem cosi/proofs
  :depends-on (cosi/package)
  :components ((:file "address")
               (:file "block")))

(defsystem cosi/proofs/newtx
  :depends-on (cosi/proofs)
  :components ((:file "new-transactions")))





