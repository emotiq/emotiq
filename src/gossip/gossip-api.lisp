;;; gossip-api.lisp
;;; 07-Jun-2018 SVS

;;; High-level API for gossip clients. Messages herein are sent via :announce
;;;  gossip protocol, and are application messages, which will be packaged
;;;  inside gossip messages for transport.
;;;  We won't be using gossip replies herein.
;;; 'Message' herein is assumed to be a list whose car represents the ultimate disposition
;;;   actor-ish thing that the cdr of message will be sent to by receiving node.

(in-package :gossip)

(defun get-live-keys ()
  "Returns a list of live keys. Second value returned is a universal-time of last time
   the list was refreshed."
  )

(defun direct-send (message nodeID)
  "Sends message directly to given nodeID. GraphID is irrelevant here."
  (let ((solicitation (make-solicitation
                       :reply-to nil
                       :kind :announce
                       :forward-to nil
                       :args message)))
    (send-msg solicitation
              nodeID                   ; destination
              nil)))

(defun sub-broadcast (message &key graphID nodeID)
  "Sends message via neighborcast"
  (let ((solicitation (make-solicitation
                       :reply-to nil
                       :kind :announce
                       :forward-to t ; neighborcast
                       :graphID (or graphID *default-graphID*)
                       :args message)))
    (send-msg solicitation
              nodeID                   ; destination
              nil)))

(defun gossip-broadcast  (message &key graphID nodeID)
  "Sends message via traditional gossip"
  (let ((solicitation (make-solicitation
                       :reply-to nil
                       :kind :announce
                       :forward-to 2 ; traditional gossip
                       :graphID (or graphID *default-graphID*)
                       :args message)))
    (send-msg solicitation
              nodeID                   ; destination
              nil)))

(defun establish-star-broadcast-group (list-of-nodeIDs &key graphID center-nodeID)
  "Creates a star topology with center-nodeID in center.
  If no center-nodeID is given, a pseudo-node is created for that purpose.
  Returns given graphID or it makes a new one and returns that.
  If given graphID exists on these nodes, all connections on that graphID will be destroyed first."
 
  )

(defun establish-gossip-broadcast-group (list-of-nodeIDs &key graphID)
 "Creates a connected graph (but usually not fully-connected) from list-of-nodeIDs.
  Returns given graphID or it makes a new one and returns that.
  If given graphID exists on any of these nodes, all connections on that graphID will be destroyed first."
  )

(defun establish-broadcast-group (list-of-nodeIDs &key graphID)
   "High-level API call. Decides itself whether
   star or gossip is best, based on number of nodes, etc. (Large number of nodes suggests
   gossip is better.)"
  )


