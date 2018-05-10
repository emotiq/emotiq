# To run the simulation
    
    (ql:quickload :emotiq/sim)
    (emotiq/sim:initialize)
    (emotiq/sim:run)
    
Various diagnostic messages from the actor threads will 
appear to `cl:*STANDARD-OUTPUT*`.

After the simulation completes, one may inspect the created blocks
via:

    (emotiq/sim:blocks)
    
The transactions (currently three) may be inspected via:

    (emotiq/sim:transactions)





