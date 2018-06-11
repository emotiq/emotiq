;;; package.lisp
;;; for gossip protocols
;;; SVS

(defpackage :gossip
  (:use :cl)
  (:export
   #:gossip-startup
   #:ping-other-machines
   #:*nodes*
   #:*log-filter*
   #:*log*
   #:*log-dots*
   #:make-graph
   #:solicit
   #:solicit-wait
   #:solicit-progress
   #:solicit-direct
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
   #:unwrap
   #:save-log
   #:save-text-log
   #:deserialize-log
   ))


(defpackage gossip/config
  (:use #:cl)
  (:export
   #:initialize 
   #:get-values))

(defpackage :gossip/transport
  (:use :cl)
  (:export
   #:start-transport
   #:stop-transport
   #:status
   #:transmit))


