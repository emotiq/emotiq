;;;; package.lisp

(defpackage #:emotiq
  (:use #:cl)
  ;; We don't need CL:BLOCK, want to use BLOCK in our package.
  (:shadowing-import-from #:cl #:block)
  (:export #:octet #:octet-vector #:octet-vector-p
           #:make-octet-vector #:ovref
           #:octet-vector-to-hex-string #:hex-string-to-octet-vector
           #:string-to-octet-vector

           #:normalize-to-simple-base-string #:copy-as-simple-base-string
           #:normalize-to-simple-string #:copy-as-simple-string)
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
   #:hash-pointer-item
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
   #:emotiq/log/)
  
  (:export
   #:start-node)
  (:export
   #:*notestream*
   #:note)
  (:export
   #:production-p
   #:main
   #:start))
   
(defpackage emotiq/cli
  (:use #:cl)
  (:export
   #:main
   #:spawn-cli))

(defpackage emotiq/wallet
  (:use #:cl)
  (:export
   #:get-wallet-named

   #:create-wallet

   #:rename-wallet

   #:enumerate-wallets

   #:*default-wallet-name*
   
   #:emotiq-wallet-path

   #:primary-address
   #:hexify
   
   #:wallet #:make-wallet
   #:salt #:keying-triple #:encrypted-private-key-p))

(defpackage emotiq/sim
  (:use #:cl)

  (:export
   #:initialize
   
   #:run
   #:run-new-tx

   #:blocks
   #:nodes
   #:keys-and-stakes
   
   #:create-transaction
   #:force-epoch-end

   
   #:*user-1* #:*user-2* #:*user-3*
   #:*tx-1* #:*tx-2* 

   #:eassert

   #:prdebug

   #:node-repl))
  
(defpackage emotiq/elections
  (:use #:cl)
  (:export 
   #:set-nodes
   #:make-election-beacon
   #:fire-election
   #:kill-beacon
   #:hold-election))

(defpackage emotiq/filesystem
  (:use #:cl)
  (:nicknames #:emotiq/fs #:emotiq/path)
  (:export
   #:emotiq/user/root/
   #:etc/
   #:var/log/))

(defpackage emotiq/tracker
  (:use #:cl)
  (:export
   #:*state*
   #:*tracking-actor*
   #:start-tracker
   #:query-current-state
   #:query-raw-nodes
   #:get-notes
   #:get-all-info
   #:get-mempool
   #:get-blockchain
   #:track))

(defpackage emotiq/ate
  (:use #:cl)
  (:export
   #:begin-sim
   #:wind-down-sim
   #:introspect))

