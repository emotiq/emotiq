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
   #:visualize-nodes

   ; API
   #:application-handler
   #:get-live-uids
   #:singlecast
   #:broadcast
   #:establish-broadcast-group
   #:dissolve-graph
   ))

(defpackage gossip/config
  (:use #:cl)
  (:export
   #:initialize 
   #:get-values))

