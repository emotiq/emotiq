;;;; package.lisp

(defpackage #:emotiq
  (:use #:cl)
  (:export #:octet #:octet-vector #:octet-vector-p
           #:make-octet-vector #:ovref
           #:octet-vector-to-hex-string #:hex-string-to-octet-vector
           #:string-to-octet-vector)
  (:export #:hash-digest-vector-to-hex-string
           #:sha-256-string #:sha-256-vector 
           #:sha3-512-string #:sha3-512-vector)
  (:export))


(defpackage emotiq/research/chain
    (:use :cl)
    (:export
     #:blockchain-block
     #:transaction

     #:sha-256-string-for-transaction

     #:get-coinbase-amount

     #:make-genesis-transaction
     #:make-genesis-block

     #:start-blockchain
     #:restart-blockchain
     #:reset-blockchain

     #:test-blockchain))
