;; top-level glue code for a node - emtqnode == Emotiq Node 

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

(in-package :emotiq)

;; top level 
(defun emtqnode ()

      ;; This is a lot like the node.js "solution".  Node.js uses underlying O/S routines (libev.so) to work
      ;; with asynch (non-blocking) I/O provided by the guts of the O/S.  Node.js relies on callbacks, which
      ;; drives you into callback hell.  Actors is a structured way of dealing with callbacks.  An Actor is
      ;; much like a "process" (thread).  Everything is self-contained and relies only on messages sent by
      ;; Actors to other Actors.  Actors are cheaper than processes, in that Actors run in Green Threads.
      ;; There is no "context switch" overhead (context switch == bad news, dump the lookahead caches, dump
      ;; all registers onto the stack, etc, etc.)  Node.js relies on anonymous lambdas as callbacks.  Guess
      ;; what?  Lisp invented that s*** long, long ago.  The trick is to apply "hierarchy" and "locality of
      ;; reference" to slay callback hell.  Flatness (i.e. Node.js) is BAD, hierarchy and locality of
      ;; reference (i.e.  Actors) is GOOD.

;;  all MVP nodes contain the genesis block in their block "array".  Transactions (after being cleared) are held in mempool, 
;;  Blocks are held in an "array" (indexable by block number, which is the same as "epoch")

;;  In EMTQ, Blocks are published by the CoSi leaders.  We are using PoS (proof of stake).  Bitcoin uses PoW (proof of work).

;;  In BitCoin, the first node to solve the PoW "riddle", "wins" the contest (election) and gets to publish
;;  its block to the blockchain.  The blockchain contains a number of block, in order - they contain backward
;;  pointers (the block hash) to previous blocks and a Merkle root (in previous block).  If any previous block
;;  has been tampered with, it's Merkle root (and its hash, the "backward pointer") will be completely
;;  different - allowing verifier nodes to quickly calculate and declare tampering.  In the EMTQ blockchain,
;;  we use PoS instead of PoW.  We save lots of cpu cycles, but burn those cycles up doing cloaking (a heavy
;;  crypto operation).

;;  Instead of doing PoW, EMTQ uses PoS and declares a "winner" (aka leader) at the beginning of an epoch, instead of at then end of 
;;  an epoch (which is what BitCoin does).  

;;  A block is just a bunch of transactions, plus some extra info (e.g. previous block hash, previous Merkle tree root)

;;  The "winner" (aka leader) gets to publish a block to the blockchain.  In BitCoin, the winner is the first
;;  node to finish the PoW "riddle" (== grab a bunch of transactions, calculate a hash of the cluster of
;;  transactions, plus some "random number" (called a Nonce) that makes the final hash less than some given
;;  value.  PoW means that a node must recalculate this puzzle until it "gets it right".  Guessing is the only
;;  possibility - there is no clear-cut way to arrive at a hash of of the transactions plus the Nonce.  This
;;  hash must result in a hash that is less than some number.  Crypto-theory says so.  So, every node simply
;;  spins its wheels trying to find the answer (and burns CPU cycles and electricity).  BitCoin parameterizes
;;  and tunes its chain so that _someone_ manages to solve the puzzle every 10 minutes or so.  The BitCoin algorithm
;;  is dynamic - it it takes longer than 10 minutes to solve the riddle, the BitCoin blockchain reduces the complexity
;;  of the riddle in the next epoch (and v.v.).
;;
;;  In BitCoin, it is possible to have a race condition between "winners" (aka "leaders"), esp. when
;;  propagation delay times on the network are involved.  Maybe two nodes - at different ends of the network -
;;  find solutions to their "riddles" at the "same" time.  Time is fractal.  This is just Physics, no way
;;  around it.  Let's say that a node at end A solves its riddle, publishes a block and then sends it to the
;;  chain.  At the other end B, a node solves the riddle, publishes its block and then sends it to the chain.
;;  Both A and B think that they are "winners" and that their block is the winner.  A publishes its block
;;  before it sees that B has published a block.  This is a "race".  Did A win, or did B win?  In BitCoin,
;;  this race is solved by the length of the chain.  A publishes, B publishes, both think that they've won,
;;  but A sprays its solution to more nodes more quickly than B does.  After a few cycles (about 10 minutes
;;  each BitCoin == epochs) A's block is buried by more new blocks than B's block.  The chain containing A's
;;  block ends up being longer than the chain containing B's block (a chain "fork" (different from hard/soft
;;  forks of the C++ code).  B's chain "loses" and dies off, because its chain is shorter than A's.  A's chain
;;  wins and propagates forever.  Or, v.v.  Either way, the "race" condition is conquered and only one set of
;;  transactions lives to see another day (in the above, this would be every transaction contained in the
;;  block published by A, some of which may have been contained in the block published by B, so somehow, most
;;  worthwhile transactions get published in a "winning" block, at some time).  

;;  BitCoin's mempool (bag of candidate transactions) actually dumps transactions about every 72 hours - they
;;  were too small or didn't come with enough reward.  They just get dumped on the floor and are never
;;  published into a block.
;;
;;  BitCoin originally promised that transactions would be hidden (hide identity of buyers/sellers).  It
;;  didn't work.  Hackers found ways to break this promise (e.g. A.I. to track transaction movement and to
;;  guess "who" was involved).

;;  Furthermore BitCoin has growth problems.  When more nodes join the network, they slow everything down
;;  (propagation delays, again).  BitCoin also defines a max size for a block.  People are just now finding
;;  out that the chosen max size, limits the size and speed of the chain.
;;
;;  With PoS, EMTQ gets around the guessing game and simply chooses a "winner" (aka "leader") at the
;;  beginning, using some crypto-theory notion of how to generate a very random number (Randherd) and who has
;;  "skin in the game".  The "winner" (aka "leader") gets to build and publish a block.  Period.  No further
;;  competition.  No wasted cpu cycles playing the "riddle" game.  There are a bunch of crypto-theory rules
;;  (invented by Engineers and Mathematicians) which deal with leaders that cheat or leaders that crash.  

;;  CoSi is an algorithm that chops up the work on the blockchain.  Chopping up the work allows it to be parallelized
;;  and allows the publishing of blocks to happen faster.  The trade-off is that CoSi can't be done "all at once".  It
;;  needs to work in a couple of passes.  It needs nodes to "sign off" on blocks built by the Leader.  It's a game of 
;;  trade-offs - how much can we trust a block, vs. how quickly each block can be published.
;; 
;;  EMTQ also uses a new crypto-theory that allows us to hide transactions better (better than BitCoin).  This
;;  extra / new crypto-work takes a lot of CPU cycles and burns at least as much electricity as the BitCoin
;;  PoW game(s).  We will have to experiment and tune the EMTQ network for the "best" balance between
;;  block-publishing speed and cloaking.
;;
;;  BitCoin uses the UTXO model and Ethereum does not.  UTXO's are not "full" transactions - they are
;;  optimized transactions (not all of the public keys are included and do not need to be sent across the
;;  network, TXNS need to have access to the keys, but we don't need to send them across the network, esp.
;;  full nodes, which have everything at their disposal).  EMTQ uses the optimized UTXO model.  When I was a
;;  kid and went to the bank, I would fill out a small slip that said stuff like "move xxx $'s from my account
;;  A to my other account B".  The Bank Teller would read the slip and create a full-blown transaction using a
;;  computer terminal (transactions would be cleared at the end of the day, kind of like Epochs and published
;;  blocks in our model).  I would have to wait in line to talk to the Teller.  The lineup is like mempool,
;;  except that the bank gets to choose who to talk to and doesn't have to treat each person in the lineup, in
;;  any kind of order.  Kind of like the old payola schemes - if the Teller sees that you've provided a bribe of
;;  $100, they'll choose you before dealing with anyone who only offered a $10 bribe).

    ;; every Node contains the genesis block at startup, so every Node's notion of "epoch" at the start is 1 (the blockchain 
    ;; contains exactly one block)

    (let ((epoch 1))

      ;; all nodes need to fetch the blockchain on restart
      
      (cosi-simgen::reconstruct-blockchain)

      ;; start up the steady-state node
      ;; all nodes run a small "tree" of Actors and LW processes.

      ;; (1) all nodes: router as an Actor.  The Router scans all incoming messages (e.g. by looking at /
      ;; ecasing on (first txn) then sends the whole message to the appropriate chain (CoSi, vs. new
      ;; transaction, vs. etc.)

      ;; (2a) one node only: random number faker (rand-pup) in an LW process (thread)
      ;; (2b) all nodes run CoSi as an Actor
      ;; (2b)all nodes run wallet i an LW process.  Assumed: wallet needs access to *standard-input* and *standard-output*,
      ;;   which might entail blockin I/O.  Blocking I/O is not handled by Actors, but, it is fully handled by
      ;;   LW process-run-function.
      ;; (2b) all nodes: Random Number Receiver as an Actor.
      ;;   architecture issue: several choices: 
      ;;     (a) kill all current work and jump to an election as soon as a :random message is received 
      ;; (b) continue working when :random message received, messages are queued, start election when work
      ;;     done, random numbers is a list (queue) 
      ;; (c) same as (b), but every new :random overwrites the single variable (random numbers are not queued)

      ;; (aside: Actors, in general, queue messages, but when a message contains a random number what do we
      ;; do?  Do we form a queue of random numbers, or do we have a single global which gets overwritten each
      ;; time a random number comes in?  Queing random numbers can create strange latency-related problems in
      ;; complicated / thick cases - what if a Node is way behind (due to latency) and starts trying to talk
      ;; to a Leader who is no longer the Leader (a couple of elections behind)?  Queued Election messages can
      ;; result in similar problems.  Queuing Elections can be different from queueing random number messages.
      ;; What is the simplest combination for the MVP???  I dunno.  I kind of favour not queuing anything - if
      ;; a random number comes in, just kill all of your current work and "return" to a "known state" (begin a
      ;; new election).  Excessive killing of ongoing work can be "tuned" by fooling with the timeout value.

      ;; (2b) all nodes: new-transaction-receiver (aka mempool) via an Actor.

      ;; essential rule: most node inputs can be done via Actors, anything that could cause blocking I/O
      ;; needs to be done in a LW process (since LW is integrated with LW I/O, but not integrated with
      ;; Actors.

      (launch-router main-node-p)

)))

(defun launch-router (main-node-p)
  "spawn several services on this node, accept all messages and route them to appropriate service"

  ;; All we need to do is make Actors for everything that is non-blocking and create LW Processes for anthing
  ;; that might block.  The Actors lib works along-side of LW Processes, and co-exists happily with LW Processes.

  (let ((emtqnode (make-actor #'node-code-top-level))
        (wallet (make-os-thread #'wallet))
        (block-service (ac:make-actor #'block-server))
        (random-gen (if ( I-am-the-very-special-node-p)
                        (make-os-thread #rand-pup)
                      nil))
        (random-receiver (ac:make-actor #'random-handler))
        (mpool (ac:make-actor #'mpool-handler)))
    (let ((router (ac:make-actor
                   ;; the Router Actor "parses" incoming messages by looking at their message types (e.g.
                   ;; (first msg), then sends the complete message to the appropriate message handler chain.
                   ;; This is essentially what David already has built, with the exception that messages get
                   ;; routed to various handler-Actors.  There is almost no difference, except when debugging
                   ;; - if a message comes up in the "wrong" Actor, you immediately know that synchronization
                   ;; is "off" in the network.

                   ;; add new message types here, as we discover what is needed

                   (lambda (self msg)
                     (ecase (first msg)

                       ;; all of the reply-to messages
                       ((:cosi-sign-prepare 
                         :cosi-sign-commit
                         :validate
                         :public-key)
                        
                        (apply 'send emtqnode msg))

                       ;; no reply-to messages
                       ((:add/change-node
                         :remove-node
                         :election)
                        (apply 'send emtqnode msg))
                       
                       ;; -------------------------------
                       ;; internal comms between Cosi nodes
                     
                       (:signing
                        (apply 'send emtqnode msg))
                     
                       ;; -----------------------------------
                       ;; for sim and debug
                     
                       ((:answer
                         :reset)
                        (apply 'send emtqnode msg))
                     
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

(defun make-os-thread (fn)
  "make a Process/Thread that co-exists with Actors - This version is LW only"
  (mp:process-run-function fn)

;; stubbed out functions

(defun I-am-the-very-special-node-p ()
  "return T if this EMTQ Node has special privileges - e.g. only one node should run the rand faker - return T is this is that Node (probably set up in a config file)"
  ;; this info is in a config file somewhere.  Fetch it and return T or NIL as appropriate.
)

(defun wallet ((self ac:actor) msg)
  "create transactions, send them to be included in the blockchain ; receive inputs"
  (loop
   (let ((str nil))
     (format *standard-output* "type something~%")
     (setf (read-line *standard-input*)))
     (format *standard-output* "you typed /~a/~%" str)
     ;; duh - needs all the usual error checking and termination stuff
     ;; (loop-finish)
     ;;
     ;; done this way, this Process can access other Lisp things in the same image, including, probably sending messages
     ;; to Actors and *earmuff* variables (LW makes this stuff properly overlap in a Lisp-like manner).  Ask David, he
     ;; knows this stuff cold.
     )))

(defun block-service ((self ac:actor) msg)
  "sends blocks to nodes that need to read-in the full blockchain (e.g. after a reboot)"
  ;; all nodes contain the full block-chain - if any (non-full?) node requests a certain block, then dig it up and send it back
  ;; to the requestor - maybe not needed in MVP???
  )

;; utility functions

 (defun rand-pup ()
   "run the fake random number generator (the caller puts this in an os-process)"
   (rh-trial-election::make-trial-election-beacon)))

(defparameter *next-random* 0)

(defun random-receiver ((self ac:actor) msg)
  "receive faked random numbers for use in upcoming elections"
  ;; This is an Actor, it over-writes the *next-random* variable with each new random number
  ;; This is an design issue!  (1) Maybe we want to queue the random numbers (in which case a PUSH is more appropriate)
  ;; Or, maybe we want to (2) send a message to our Node to kill all its work and go to Election mode, without passing GO.
  ;; (2) is easy using Actors - just send a :reset message, null out the block-being-built, etc.  If it's not obvious, ask me
  ;; or David.  It should be about one line of code...
  (let ((rand (first msg)))
    (setf *next-random* rand)))

(defun get-random ()
  "return the next random number for an election - this version uses overwrite (it could be a queue and a simple POP, but needs
   some careful thought about what is best"
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

(defun node-code-top-level ()
  "run a node from the top (this is called as an Actor)"
  )