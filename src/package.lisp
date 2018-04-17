;;;; package.lisp

(defpackage #:emotiq
  (:use #:cl)
  ;; We don't need CL:BLOCK, want to use BLOCK in our package.
  (:shadow #:block)
  (:export #:octet #:octet-vector #:octet-vector-p
           #:make-octet-vector #:ovref
           #:octet-vector-to-hex-string #:hex-string-to-octet-vector
           #:string-to-octet-vector)
  (:export #:hash-digest-vector-to-hex-string
           #:sha-256-string #:sha-256-vector 
           #:sha3-512-string #:sha3-512-vector
           #:hash-160-string #:hash-160-vector
           #:hash-256-string #:hash-256-vector
           #:keygen #:sign-message #:verify-signature)
  (:export #:hash-digest-vector-to-hex-string
           #:sha-256-string #:sha-256-vector 
           #:sha3-512-string #:sha3-512-vector
           #:hash-160-string #:hash-160-vector
           #:hash-256-string #:hash-256-vector
           #:keygen #:sign-message #:verify-signature)
  (:export
   #:hash-pointer-of-previous-block 
   #:hash-pointer-item
   #:hash-pointer-of-transaction
   #:transaction-id
   #:do-blockchain
   #:print-blockchain-info
   #:print-block-info
   #:start-blockchain-context
   #:with-blockchain-context
   #:next-transaction
   #:next-coinbase-transaction
   #:genesis-block #:last-block #:last-transaction
   #:initial-total-coin-amount
   #:*minter-0-pkey-hash* #:*minter-0-pkey* #:*minter-0-skey*
   #:get-utxos-per-account #:get-balance)
  (:export
   #:block #:protocol-version #:epoch #:prev-block #:prev-block-hash
   #:merkle-root-hash #:block-timestamp #:transactions
   #:validator-keys-joining #:validator-keys-leaving
   #:create-block #:hash-block #:serialize-block 
   #:compute-merkle-root-hash)
  (:export
   #:production-p
   #:main
   #:start))
