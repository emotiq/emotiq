;;; package.lisp
;;; for gossip protocols
;;; SVS

(defpackage :gossip
  (:use :cl)
  (:export
   #:*nodes*
   #:*log-filter*
   #:make-graph
   #:solicit
   #:solicit-wait
   #:solicit-progress
   #:stop-gossip-sim
   #:run-gossip-sim
   #:as-hash-table
   #:make-node
   #:listify-nodes
   #:set-protocol-style
   #:get-protocol-style
   #:random-node
   #:gossip-node
   #:proxy-gossip-node
   #:eripa
   #:archive-log
   #:measure-timing
   ))

(defpackage :gossip/transport
  (:use :cl)
  (:export
   #:start-transport
   #:stop-transport
   #:status
   #:transmit))

