;;; package.lisp
;;; for gossip protocols
;;; SVS

(defpackage :gossip
  (:use :cl)
  (:export
   #:gossip-handler-case
   #:cosi-loaded-p
   #:initialize-node
   #:*nominal-gossip-port*
   #:gossip-startup
   #:ping-other-machines
   #:*nodes*
   #:edebug
   #:clear-local-nodes
   #:*log-filter*
   #:*log*
   #:make-graph
   #:solicit
   #:solicit-wait
   #:solicit-progress
   #:solicit-direct
   #:run-gossip
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
   #:uid
   #:gossip-init
   #:lookup-node
   #:ensure-pinger-daemon

   ; API
   #:locate-local-uid-for-graph
   #:application-handler
   #:*ll-application-handler*
   #:get-live-uids
   #:singlecast
   #:broadcast
   #:establish-broadcast-group
   #:dissolve-graph
   #:memoize-node

   #:shutdown-gossip-server
   ))

(defpackage gossip/config
  (:use #:cl)
  (:IMPORT-FROM :gossip :edebug :eripa :gossip-handler-case)
  (:export
   #:generate-network
   #:generate-node
   #:*aws-example*
   #:initialize 
   #:get-values))

(defpackage :gossip/transport
  (:use :cl)
  (:IMPORT-FROM :gossip :edebug :eripa :gossip-handler-case)
  (:export
   #:start-transport
   #:stop-transport
   #:status
   #:transmit))


