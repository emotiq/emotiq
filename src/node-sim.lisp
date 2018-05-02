;; top-level glue code for a simulation node
 
(in-package :cosi-simgen)

(defun node (&optional
             (name "Alice")
             (ipstr "127.0.0.1"))  ;; specify an IP (as string) if you want something other than 127.0.0.1

  (cosi-init ipstr)
  (cosi-generate)
  (init-sim)
  (reset-blockchain)

  (let ((node (start-node)))
    
    (with-wallet (wallet (emotiq/cli::open-wallet name))

      (let ((blk (create-fresh-block node)))
        (add-genesis-utxo-to-block blk 1000)
        (spend-genesis blk 1000 genesis-utxo wallet)
        (seal-block blk)
        (send-block-to-blockchain blk)))
      
      ;; at this point, the wallet should hold all 1000 Emtq's (unspent)
      ;; given to it by the genesis utxo

      )
    
    (emotiq/cli:wallet-at-command-line "Alice")
    
    )
  )




(defun start-node ()
  (spawn 

(defun node-dispatcher (node &rest msg)
  (let ((*current-node* node))
    (um:dcase msg
      ;; ----------------------------
      ;; user accessible entry points - directed to leader node
      
      (:cosi-sign-prepare (reply-to msg)
       (node-compute-cosi node reply-to :prepare msg))
      
      (:cosi-sign-commit (reply-to msg)
       (node-compute-cosi node reply-to :commit msg))
      
      (:cosi-sign (reply-to msg)
       (node-compute-cosi node reply-to :notary msg))
      
      (:new-transaction (msg)
       (node-check-transaction msg))
      
      (:validate (reply-to sig bits)
       (node-validate-cosi reply-to sig bits))
      
      (:public-key (reply-to)
       (reply reply-to :pkey+zkp (node-pkeyzkp node)))
      
      (:add/change-node (new-node-info)
       (node-insert-node node new-node-info))
      
      (:remove-node (node-ip)
       (node-remove-node node node-ip))
      
      (:election (new-leader-ip)
       (node-elect-new-leader new-leader-ip))
      
      ;; -------------------------------
      ;; internal comms between Cosi nodes
      
      (:signing (reply-to consensus-stage msg seq)
       (case consensus-stage
         (:notary 
          (node-cosi-notary-signing node reply-to
                                    consensus-stage msg seq))
         (otherwise
          (node-cosi-signing node reply-to
                             consensus-stage msg seq))
         ))
      
      ;; -----------------------------------
      ;; for sim and debug
      
      (:make-block ()
       (leader-exec node))

      (:genesis-utxo (utxo)
       (record-new-utxo (bev (txout-hashlock utxo))))
      
      (:answer (&rest msg)
       ;; for round-trip testing
       (ac:pr msg))
      
      (t (&rest msg)
         (error "Unknown message: ~A~%Node: ~A" msg (node-ip node)))
      )))
  
;; -------------------------------------------------------

(defun make-node-dispatcher (node)
  ;; use indirection to node-dispatcher for while we are debugging and
  ;; extending the dispatcher. Saves reconstructing the tree every
  ;; time the dispatching chanages.
  (ac:make-actor
   ;; one of these closures is stored in the SELF slot of every node
   (lambda (&rest msg)
     (apply 'node-dispatcher node msg))))

  
