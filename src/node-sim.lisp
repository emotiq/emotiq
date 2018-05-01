;; top-level glue code for a simulation node - enode == Emotiq Node
 
(in-package :cosi-simgen)

(defun node (&optional (ipstr "127.0.0.1"))
  (cosi-simgen::cosi-init ipstr) ;; change this address to suit your machine
  (cosi-simgen::cosi-generate)
  (cosi-simgen::init-sim)
  (cosi-simgen::tst-blk)

  (emotiq/cli::main)
)

