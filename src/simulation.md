# Running the local node simulation

The reference behavior of the Emotiq chain with all nodes in the local
process.


## Configuring for local run

Only needs to be done once:

```lisp
(ql:quickload :emotiq/config/generate)
(emotiq/config/generate:ensure-defaults :force t :for-purely-local t)
```    


## Running chain locally

After configuring

```lisp
(ql:quickload :emotiq/app)
(emotiq/app:test-app)
```

After a test run, `emotiq/app:*blocks*` should return the full blockchain (relative to the Node associated with the REPL process), which can then be inspected.

A test run takes several minutes.  One should see periodic (about once a minute) calls for new elections.  In lispworks, the emotiq:note output is directed to the Output Window (hcl:*background-output*) whereas all other lisps direct the emotiq:note output to *error-output*.

The message "Leader see transactions: NIL" means that the current leader has no new transactions and does not seal an otherwise "empty" block.

The message "Leader sees transactions: (#<Tx ...)" means that the leader had new transactions in its mempool and an attempt to seal a new block runs, using the CoSi protocol.  In such a case, one should see messages "Waiting for Cosi prepare" and (probably) " Block validated ...",  When a block is sealed, one should see the message " Block signatures = ..." and, soon afterwards a new election.  A Leader should not seal a block if it has not received enough (defined by the BFT limit) signatures - in such a case, one should see PREPARE messages, but no block is sealed.  At present, this condition should not occur in a :for-purely-local configuration.

Currently, app-test creates a blockchain with 6 blocks in it (in reverse-order of creation).  The first created block (block #5 in LW), is the genesis block and it contains one transaction - a COINBASE transaction.

The other 5 blocks contain 2 transactions, each.  (1) A COLLECT transaction (fees to Leader) and (2) a SPEND transaction (which is the real transaction).

The number of transactions in a block will change as new work on test-app is merged.
