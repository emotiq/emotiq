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
           )
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
   #:x11-display-p)

  (:export
   #:start-node)
  (:export
   #:*notestream*
   #:note
   #:em-warn)
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
   #:submit-transaction
   #:get-transaction)
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
  (:nicknames #:emotiq/fs
              #:emotiq/path)
  (:export
   #:*subpath*
   #:subpath

   #:new-temporary-directory
   
   #:emotiq/user/root/
   #:emotiq/wallet/
   #:tmp/
   #:etc/
   #:var/log/))

(defpackage emotiq/txn
  (:use #:cl)
  (:export
   #:*transaction-fee*
   #:address
   #:get-utxos
   #:make-spend-transaction))

(defpackage emotiq/tracker
  (:use #:cl)
  (:export
   #:start-tracker
   #:query-current-state
   #:track))

(defpackage emotiq/config
  (:use #:cl)
  (:export
   #:*nodes-dns-ip*
   #:*max-stake*

   #:*conf-filename*
   #:*hosts-filename*
   #:*local-machine-filename*
   #:*stakes-filename*
   #:*keypairs-filename*
   #:*genesis-block-filename*

   #:generated-directory
   #:get-genesis-block

   #:gossip-get-values
   
   #:get-nth-key
   #:get-stakes

   #:get-keypairs
   #:local-machine ;; aka gossip's idea of its configuration

   #:settings
   #:setting))

(defpackage emotiq/config/generate
  (:use #:cl)
  (:export
   #:*eg-config-zerotier*
   #:*eg-config-localhost*

   #:+default-configuration+

   #:generate-network
   #:generate-keys
   #:generate-stake

   #:ensure-defaults))

(defpackage emotiq/app
  (:use #:cl)
  (:export
   #:account
   #:make-account
   #:wait-for-node-startup
   #:publish-transaction
   #:send-all-genesis-coin-to
   #:spend
   #:get-transactions
   #:get-balance
   #:get-all-transactions-to-given-target-account
   #:get-all-transactions-from-given-target-account
   #:get-transactions
   #:get-address-of-sender
   #:amount-in-input
   #:get-txid
   #:get-tx-type
   #:get-epoch
   #:fee-paid-for-transaction
   #:payee-address
   #:amount-paid
   #:address-of-sender
   #:amount-in-input
   #:address-of-genesis
   #:*blocks*
   #:*node*
   #:*genesis*
   #:*alice*
   #:*bob*
   #:*mary*
   #:*james*))

(defpackage emotiq/random
  (:use #:cl)
  (:export 
   #:init-random))