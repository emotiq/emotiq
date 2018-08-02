(defpackage :emotiq/node
  (:use #:cl #:alexandria)
  (:nicknames #:node)
  (:export
   #:node
   #:ip
   #:real-ip
   #:pkey-zkp
   #:skey
   #:pkey
   #:parent
   #:other-members
   #:bitpos
   #:stake
   #:blockchain
   #:blocks
   #:mempool
   #:utxos
   #:current-leader
   #:current-beacon
   #:election-calls
   #:local-epoch
   #:had-work-p
   #:tx-changes
   #:byzantine-p
   #:corrupted-p
   #:cpu-load
   #:self
   #:short-id
   #:make-tx-changes
   #:current-node
   #:*top-node*
   #:*current-node*
   #:*blockchain*
   #:*blocks*
   #:*mempool*
   #:*utxos*
   #:*leader*
   #:*tx-changes*
   #:*had-work-p*
   #:*election-calls*
   #:*beacon*
   #:*local-epoch*))

(defpackage emotiq/block
  (:use #:cl #:alexandria)
  (:nicknames #:block)
  (:export
   #:eblock
   #:make-block
   #:prev-block-hash
   #:timestamp
   #:leader-pkey
   #:election-proof
   #:signature
   #:signature-bitmap
   #:witnesses
   #:witnesses-and-stakes
   #:transactions
   #:merkle-root-hash
   #:input-script-merkle-root-hash
   #:witness-merkle-root-hash
   #:transactions
   #:make-genesis-block
   #:hash
   #:compute-merkle-root-hash
   #:compute-input-script-merkle-root-hash
   #:compute-witness-merkle-root-hash
   #:serialize
   #:serialize-header
   #:update-signature!
   #:new-transactions
   #:check-transactions-hash
   #:check-transactions))

(defpackage emotiq/blockchain
  (:use #:cl #:alexandria)
  (:nicknames #:blockchain)
  (:export
   #:do-blockchain
   #:do-transactions
   #:do-all-transactions
   #:count-transactions
   #:get-utxos
   #:dump))

(defpackage emotiq/mempool
  (:use #:cl #:alexandria)
  (:nicknames #:mempool)
  (:export
   #:add-transaction
   #:remove-transaction
   #:get-transactions
   #:clear-block-transactions))
    
(defpackage emotiq/transaction
  (:use #:cl #:alexandria)
  (:nicknames #:txn)
  (:import-from #:emotiq/blockchain
                #:do-blockchain
                #:do-transactions
                #:do-all-transactions)
  (:export
   #:*minimum-transaction-fee*
   #:transaction
   #:transaction-input
   #:transaction-output
   #:id
   #:id=
   #:id-string
   #:hash
   #:type
   #:outputs
   #:inputs
   #:output-address
   #:output-lock-script
   #:output-amount
   #:output-proof
   #:output-message
   #:input-id
   #:input-index
   #:input-unlock-script
   #:witness-data-p
   #:sum-outputs
   #:utxo-p
   #:precedes-p
   #:coinbase-input-p
   #:compute-fee
   #:make-genesis-transaction
   #:make-spend-transaction
   #:make-collect-transaction
   #:validate
   #:in-legal-money-range-p
   #:in-legal-stake-range-p
   #:dump))

(defpackage emotiq/transaction/script
  (:use #:cl #:alexandria)
  (:nicknames #:txn/script)
  (:shadow #:eval #:apply #:error)
  (:export
   #:get-lock-script
   #:get-unlock-script
   #:get-lock-script-for-type
   #:get-unlock-script-for-type
   #:eval
   #:apply
   #:error
   #:run))
