# To run the simulation

    
You must configure the simulation for the localhost via:

    (ql:quickload :cosi-bls)
    (cosi-simgen::cosi-init "127.0.0.1")
    (cosi-simgen::cosi-generate :nodes 8)
    
Then, reboot your Lisp (a bug here that we need to figure out).

    (ql:quickload :emotiq/sim)
    (emotiq/sim:initialize)

If you don't see messages like 
    
    "fowarding-by-default-to-me: (RESET)" 

the initialization has failed.

    (emotiq/sim:run)

to see the blocks:

    (emotiq/sim:blocks)

To see a transaction

    (emotiq/sim:txn1)

both of these functions only work after the system has settled down.



