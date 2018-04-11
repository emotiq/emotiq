;;; package.lisp
;;; for gossip protocols
;;; SVS

(defpackage :gossip
  (:use :cl)
  (:export
   #:*nodes*
   #:solicit
   #:solicit-wait
   #:solicit-progress
   #:stop-gossip-sim
   #:run-gossip-sim
   #:as-hash-table
   #:make-node
   #:listify-nodes
   #:set-protocol-style
   #:get-protocol-style))
