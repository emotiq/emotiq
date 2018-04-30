;; top-level glue code for a node - enode == Emotiq Node 
(in-package :emotiq)

(defun enode ()
  (cosi-simgen::cosi-init)
  (cosi-simgen::cosi-generate)
  (cosi-simgen::init-sim)
  (cosi-simgen::tst-blk)

  (emotiq/cli::main)
)
