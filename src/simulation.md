# Running the local node simulation

## Running

### Cloaked
```lisp
(ql:quickload :emotiq/sim)
(emotiq/sim:initialize)  ;; takes several keywords - see node-sim.lisp
(emotiq/sim:run) 
```    

### Uncloaked 
```lisp
(ql:quickload :emotiq/sim)
(emotiq/sim:initialize)  ;; takes several keywords - see node-sim.lisp
(emotiq/sim:run :cloaked nil)
```

## Explanation    

The simulation performs the following steps:

  1.  Create a genesis transaction with MONETARY-SUPPLY coins.  
      Transact 1000 coins to *USER-1*.  The resulting transaction
      can be referenced via *TX-1*.

  2.  In the transaction references as *TX-2*, *USER-1* sends 500
      coins to *USER-2*, 490 coins to *USER-3*, with a fee of 10
      coins.

Various diagnostic messages from the actor threads will
appear to `cl:*standard-output*` and `cl:*standard-error*`.

After the simulation completes, one may inspect the created blocks
via:
```lisp
(emotiq/sim:blocks)
```

The specials `*tx-1*` and `*tx-2*` will hold references to the
transactions.

The specials `*user-1*` `*user-2*` `*user-3*` will hold references to the
user identities.

Subsequently, one may use the `spend` and `spend-list` functions to
create further transactions.  


## Notes
### helpers...
```lisp
(progn
  (system:run-shell-command "rm -rf ~/.cache/common-lisp/")
  (ql:quickload :emotiq/sim))

(progn
  (emotiq/sim:initialize)
  (emotiq/sim::run :cloaked nil))
```    

### for pt linux
```lisp
(system:run-shell-command "rm -rf ~/.cache/common-lisp/")
(ql:quickload :emotiq/sim)
(emotiq/sim:initialize :cosi-prepare-timeout 60 :cosi-commit-timeout 60 :executive-threads 8)
```
```lisp
(emotiq/sim:run :cloaked t)
```
or
```lisp
(emotiq/sim:run :cloaked nil)
```


    
