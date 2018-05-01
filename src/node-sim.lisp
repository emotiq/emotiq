;; top-level glue code for a simulation node - enode == Emotiq Node
 
(in-package :cosi-simgen)

(defun node ()
  (cosi-simgen::cosi-init "192.168.2.13") ;; change this address to suit your machine
  (cosi-simgen::cosi-generate)
  (cosi-simgen::init-sim)
  (cosi-simgen::tst-blk)

  (emotiq/cli::main)
)

