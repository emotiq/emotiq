# Running the local node simulation

## Running
    
    (ql:quickload :emotiq/sim)
    (emotiq/sim:initialize)  ;; takes several keywords - see node-sim.lisp
    (emotiq/sim:run)
    
## Explanation    
    
The simulation spawns an actor which will asynchronously to perform
the following steps.  AMOUNT is by default 1000, which may be
configured in parameters to RUN:

  1.  Create a genesis transaction with AMOUNT coins.  Transact AMOUNT
      coins to `*user-1*`.  The resulting transaction can be referenced
      via `*tx-1*`.

  2.  Transfer the AMOUNT of coins from `*user-1`* to `*user-2*` as `*tx-2*`.

  3.  Transfer `(- amount (floor (/ amount 2)))` coins from `*user-2*` to
      `*user-3*` as `*tx-3*`.
  
  4.  Force these transactions to be commited as part of the block.
  
Various diagnostic messages from the actor threads will 
appear to `cl:*standard-output*`.

After the simulation completes, one may inspect the created blocks
via:

    (emotiq/sim:blocks)
    
The specials `*tx-1*` `*tx-2*` `*tx-3*` will hold references to the
transactions.

The specials `*user-1*` `*user-2*` `*user-3*` will hold references to the
user identities.

Subsequently, one may use the `spend` and `spend-list` functions to
create further transactions.  

After transactions have been created, one may force a new block to be
created via:

    (force-epoch-end)
    
    



