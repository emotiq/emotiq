(defpackage :cosi-simgen
  (:use
   :cl
   :alexandria
   :crypto/modular-arith
   :vec-repr
   :hash)
  (:import-from :actors
   :=bind
   :=values
   :=defun
   :=lambda
   :=funcall
   :=apply
   :pmapcar
   :smapc
   :spawn
   :current-actor
   :send
   :recv
   :retry-recv
   :become
   :par
   :do-nothing
   :make-actor
   :set-executive-pool
   :with-borrowed-mailbox
   :pr)

  (:import-from :node
                :*current-node*
                :current-node
                :*blockchain*     
                :*blocks* 
                :*mempool*        
                :*utxos*     
                :*leader*         
                :*tx-changes*     
                :*had-work-p*
                :*election-calls*
                :*beacon*
                :*local-epoch*)
                
  (:export
   :*current-node*
   :current-node
   :gossip-neighborcast
   ;; :node
   ;; :node-pkey
   ;; :node-skey
   ;; :node-stake
   ;; :node-self
   ;; :node-blockchain
   ;; :node-blockchain-tbl
   ;; :node-mempool
   ;; :node-utxo-table
   ;; :node-current-leader
   ;; :*my-node*
   ;; :*top-node*
   ;; :*leader*
   ;; :*blockchain*
   ;; :*blockchain-tbl*
   ;; :*mempool*
   ;; :*utxo-table*
   ;; :*ip-node-tbl*
   ;; :*pkey-node-tbl*
   ;; :*pkey-skey-tbl*
   ;; :*node-bit-tbl*
   :send
   :reply
   :node-dispatcher
   :*cosi-prepare-timeout*
   :*cosi-commit-timeout*
   :leader-exec
   :*default-data-file*
   :*default-key-file*
   :generate-tree
   :reconstruct-tree
   :init-sim
   :reset-nodes
   :forwarding
   :startup-elections
   ;; :short-id
   ;; :node-id-str
   :set-nodes
   :get-witness-list
   ))
