# Running the local node simulation

The reference behavior of the Emotiq chain with all nodes in the local
process.

## Running

### New Transactions
```lisp
(ql:quickload :emotiq/sim)
(emotiq/sim:initialize)  ;; takes several keywords - see node-sim.lisp
(emotiq/sim:run-new-tx)
```



## Explanation    

### EMOTIQ/SIM:RUN (deprecated)

The simulation performs the following steps:

  1.  Create a genesis transaction with MONETARY-SUPPLY coins.  
      Transact 1000 coins to *USER-1*.  The resulting transaction
      can be referenced via *TX-1*.

  2.  In the transaction references as *TX-2*, *USER-1* sends 500
      coins to *USER-2*, 490 coins to *USER-3*, with a fee of 10
      coins.

Various diagnostic messages from the actor threads will
appear to `cl:*standard-output*` and `cl:*standard-error*`.

### EMOTIQ/SIM:RUN-NEW-TX (use this, not RUN)

Simulation with "new transactions".

### Interactive Introspection
After the simulation completes, one may inspect the created blocks
via:
```lisp
(emotiq/sim:blocks)
```

The specials `*tx-1*` and `*tx-2*` will hold references to the
transactions.

The specials `*user-1*` `*user-2*` `*user-3*` will hold references to the
user identities.

    
