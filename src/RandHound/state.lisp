(in-package :randhound)

;; cosi-simgen-
(defun node-bitmap-table ()
  "The current nodes idea of the node network.

A table that should be in consistent sort order."
  (let ((node-bit-table
         (or
          cosi-simgen:*current-node*
          (gossip::local-real-uids))))
    (unless node-bit-table
      (error "Failed to find the current node"))
    node-bit-table))

(defun nodes ()
  ;;; TODO just create a simple in memory graph of Cosi nodes, bypassing Gossip
  ;;; gonna fail unless gossip is started
  (or
   (gossip:get-live-uids)
   (gossip:local-real-nodes)))





