;; top-level glue code for a node - enode == Emotiq Node 

;; this pseudo-code has not been compiled!!!

;; Mostly for discussion.  This pseudo-code can be adjusted.

;; I expect most of this code to be thrown away and replaced with "real" code.

;; I've invented several namespaces...

;; blocks:generate-genesis-block
;
;; internal-blocks: - every node keeps a cache of the blockchain, "internal-blocks" is how I refer to it
;; internal-blocks:set-epoch --- sets the internal counter to the most recent epoch (epoch == topmost block number)
;; internal-blocks:insert-block-into-my-internal-pool :epoch N :block block -- add block, index of N, into whatever
;; internal data structure are used to hold the entire blockchain

;; emotiq-error: is for controlling the actions of assertions and error messages

;; network:send -- sends messages to all(?) distributed nodes (issue: if gossip does not guarantee that every node
;;  will see a message, we need to consider whether there are some messages that MUST be seen by every node)

(in-package :emotiq/mvp)

;; top level 
(defun enode ()
  ;; main-node-p is a flag that is set T if this node is the "main" node (first to come up on the network)
  ;; the main node, in the MVP, creates the genesis block and serves it to other nodes (this is fragile, but sufficient for MVP)
  ;; the main node, in the MVP, creates a fake random number generator and broadcasts random numbers at various time
  ;;  intervals (say, every 30 seconds)
  ;; the "code
  (let ((main-node-p (create-genesis-block-only-once)))
    ;; the first node creates the genesis block and, therefore it's notion of epoch is "1"
    ;; all other nodes (in this variation) don't have any blocks at all, so they have an epoch of "0"
    (let ((epoch (if main-node-p 1 0)))
      ;; all nodes need to fetch the blockchain on restart
      (setf epoch (get-entire-blockchain epoch))
      ;; start up the steady-state node
      ;; all nodes run a small "tree" of Actors 
      ;; (1) all nodes: router
      ;; (2a) main node only: random number faker (rand-pup)
      ;; (2b) all nodes: CoSi
      ;; (2b)all nodes: wallet
      ;; (2b) all nodes: Random Receiver
      ;;   architecture issue: several choices: 
      ;;     (a) kill all current work and jump to an election as soon as a :random message is received
      ;;     (b) continue working when :random message received, messages are queued, start election when work done, random numbers is a list (queue)
      ;;     (c) same as (b), but every new :random overwrites the single variable (random numbers are not queued)
      ;; (2b) all nodes: new-transaction-receiver (aka mempool)

      (launch-router main-node-p)

)))

(defun launch-router (main-node-p)
  "spawn several services on this node, accept all messages and route them to appropriate service"
  (let ((enode   (ac:make-actor (cosi-simgen:make-node <???> <????> <????> )))
        (wallet (ac:make-actor #'wallet))
        (block-service (ac:make-actor #'block-server))
        (random-gen (rand-pup main-node-p))
        (random-receiver (ac:make-actor #'random-handler))
        (mpool (ac:make-actor #'mpool-handler)))
    (let ((router (ac:make-actor
                   (lambda (self msg)
                     (ecase (first msg)

                       ;; all of the reply-to messages
                       ((:cosi-sign-prepare 
                         :cosi-sign-commit
                         :validate
                         :public-key)
                        (apply 'send enode msg))

                       ;; no reply-to messages
                       ((:add/change-node
                         :remove-node
                         :election)
                        (apply 'send enode msg))
                       
                       ;; -------------------------------
                       ;; internal comms between Cosi nodes
                     
                       (:signing
                        (apply 'send enode msg))
                     
                       ;; -----------------------------------
                       ;; for sim and debug
                     
                       ((:answer
                         :reset)
                        (apply 'send enode msg))
                     
                       ;; newly invented messages
                       ;;
                       ((:get-height
                         :get-block)
                        (apply 'send block-service msg))
                     
                       (:random
                        (apply 'send random-receiver msg))
                     
                       (:new-transaction
                        (apply 'send mpool msg))
                     
                       )))))
      (when random-gen
        ;; if I am the main node, tell the random number generator where
        ;; my router is (so that the generator can send a message to the router)
        (ac:send random-gen `(:router ,router))))))

;; stubbed out functions

(defun wallet ((self ac:actor) msg)
  "create transactions, send them to be included in the blockchain ; receive inputs"
  )

(defun block-service ((self ac:actor) msg)
  "sends blocks to nodes that need to read-in the full blockchain (e.g. after a reboot)"
  )



;; utility functions

(defun create-genesis-block-only-once ()
  "if this is the first node on the network, create a genesis block and return t, else do nothing and return nil"
  ;; issue: where is the genesis block for the MVP?
  ;; Is it hard-coded into every node, or, does one node wake up with it and serve it out to other blocks?

  ;; The code below assumes that the first node to come up will be the main (via the non-existence of the ~/emotiq_config file)
  ;; and will generate the genesis block, serving it to all other nodes ; am I missing any race-conditions on the creation of the
  ;; file "~/emotiq_config" ????  As I understand the docs, the "with-open-file" will return nil in the the variable "once-only"
  ;; if the file already exists

  (with-open-file (once-only "~/emotiq_first_time" :direction :ouput :if-does-not-exist :create :if-exists nil)
    (if once-only
        (progn
          (format once-only "t~%")  ;; create once-only (lock) file and put junk into it
          (internal-blocks:insert-block-into-my-internal-pool
           :epoch 1
           :block (blocks:generate-genesis-block))
          (internal-blocks:set-epoch 1)
          
            ;; the once-only block also runs the fake random number generator (see rand-pup, below)
          t ;; t if this is the very first node to wake up

          )

      nil ;; nil, if this is not the first node

      )))


(defun get-entire-blockchain (epoch)
  "loop until all blocks have been stuffed into our internal data structures.
  A loop is needed because the read(s) take time and new blocks might have been 
  generated while we've been busy reading blocks
  -- return the high water mark for blocks (aka epoch)"

  (let (local-epoch)
    (loop
     (setf local-epoch (network:send :get-height))
     (when (> local-epoch epoch)
       (emotiq-error:cant-happen))
     (when (= local-epoch epoch)
       (loop-finish))
     (let ((block (network:send :get-block (+1 local-epoch))))
       (incf local-epoch)
       (internal-blocks:intsert-block-into-my-internal-pool epoch block)))
    epoch))
    
 (defun rand-pup (main-node-p)
  "if this is the main node, spawn an actor that generates a faked random number every NN seconds"
  (if main-node-p
      (ac:make-actor
       #'(lamdbda (self msg)
                  (let (router)
                    ;; we need to see :router message first, since we will use "router" further down
                    (if (eq :router (first msg))
                        (setf router (second msg))
                      (progn
                        (emotiq-error:assert (eq :tick (first msg)))
                        (emotiq-error:assert parent)) ;; parent must be set first
                    (actors:sleep 30-seconds)  ;; error intentional until we replace this with real code
                    (let ((random-num-msg `(:random ,(random 1000))))
                      (network:broadcast random-num-msg)

                      ;; send to the router for this node, so it will see the random num, also
                      ;; this could also be implemented by setf'ing *next-random*

                      (ac:send router random-num-msg)

                    )
    nil)
  )


(defparameter *next-random* 0)

(defun random-receiver ((self ac:actor) msg)
  "receive faked random numbers for use in upcoming elections"
  (let ((rand (first msg)))
    (setf *next-random* rand)))

(defun get-random ()
  *next-random*)


(defun mempool-handler ((self ac:actor) msg)
  "do a level-1 (of 2) verification of an incoming transaction, gossip it to other nodes, stuff it into our local mempool"
  ;; using David's cloaking code, transactions are verified in 2 phases - phase 1 verifies the transaction in situ
  ;; phase 2 verification looks for double-spends (within a newly completed block and the rest of the blockchain)
  (emotiq-error:assert (eq :transaction (first msg)))
  (let ((txn (second msg)))
    (when (crypto-transaction:verify-level-1 txn)
      (network:gossip-transaction txn)
      (mempool:add-verified-level-1-transaction txn))))


