# to run the simulation:

- (ql:quickload :emotiq/sim)
- (in-package :emotiq-user)
- (initialize-sim)

If you don't see messages like 
    "fowarding-by-default-to-me: (RESET)" 
the initialization has failed.      

- (run-sim)

to see the blocks:

- (in-package :emotiq-user)
- (blocks)

To see a transaction

- (in-package :emotiq-user)
- (txn)

both of these functions only work after the system has settled down.



