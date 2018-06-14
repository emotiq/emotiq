;;; gossip-api.lisp
;;; 07-Jun-2018 SVS

;;; High-level API for gossip clients. Messages herein are sent via :announce
;;;  gossip protocol, and are application messages, which will be packaged
;;;  inside gossip messages for transport.
;;;  We won't be using gossip replies herein.
;;; 'Message' herein is ignored and is assumed to be application-level, not gossip-level.

(in-package :gossip)

(defun get-live-uids ()
  "Returns a list of live keys. Second value returned is a universal-time of last time
   the list was refreshed."
  )

(defun singlecast (message nodeID &key graphID (howmany 2))
  "High-level API for sending message to given single nodeID. If graphID
   is supplied, message will be forwarded along the graph, starting
   with some locally-known node that's part of that graph.
   If graphID is not supplied, a direct connection to nodeID will
   be attempted and used.
   If you want to use the default graph, you must explicitly pass *default-graphID* in graphID.
   Howmany is only used if graphID is supplied."
  (let ((solicitation nil))
    (if graphID
        ; Find a local node in that graphID to send to, with forwarding.
        (let ((startnodeID (locate-local-node-for-graph graphID)))
          (when startnodeID
            (setf solicitation (make-solicitation
                                :reply-to nil
                                :kind :k-singlecast
                                :forward-to howmany
                                :args message))
            (send-msg solicitation
                      startnodeID                   ; destination
                      nil)))
        
        ; otherwise ensure there's a node with the given nodeID (real or proxy) and send to it directly NOT DONE YET
        (progn
          (setf solicitation (make-solicitation
                              :reply-to nil
                              :kind :k-singlecast
                              :forward-to nil
                              :args message))
          (send-msg solicitation
                    nodeID                   ; destination
                    nil)))))

; Normally you wouldn't call this as a high-level application programmer. Call broadcast instead.
;;; DONE
(defun gossipcast (message &key (graphID *default-graphID*) startnodeID (howmany 2))
  "Sends message via traditional gossip.
  Howmany determines whether traditional gossip or neighborcast is used."
  (let ((solicitation (make-solicitation
                       :reply-to nil
                       :kind :k-multicast
                       :forward-to howmany ; traditional gossip
                       :graphID graphID
                       :args message)))
    (send-msg solicitation
              startnodeID                   ; destination
              nil)))

;;; DONE
(defun broadcast (message &key (style ':neighborcast) (graphID *default-graphID*) startnodeID)
  "High-level API for broadcasting a message along a graph.
   If startnodeID is provided, it will be used as the starting node.
   If startnodeID is not provided, we'll start at some locally-known node that's part of given graphID.
   Use style = :neighborcast for most reliable transport.
   Use style = :gossip or some small integer (like 2 or 3) for better scalability on large graphs, at expense of some reliability.
   (An integer--if supplied--represents the maximum number of a node's neighbors to forward the message to.
    :neighborcast means 'use all the neighbors'. :gossip means 'use up to 2 neighbors'.)"
  (unless startnodeID
      (setf startnodeID (locate-local-node-for-graph graphID)))
  (cond
    ((eql ':neighborcast style) (gossipcast message :graphID graphID :startnodeID startnodeID :howmany t))
    ((eql ':gossip style)       (gossipcast message :graphID graphID :startnodeID startnodeID :howmany 2))
    ((integerp style)           (gossipcast message :graphID graphID :startnodeID startnodeID :howmany style))
    (t (error "Invalid style ~S" style))))

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
   gossip is better.)
   If given graphID exists on any of these nodes, all connections on that graphID will be destroyed first."
  (dissolve-graph graphID)
  )

(defun dissolve-graph (graphID &key startnodeID)
  "Dissolves all connections associated with graphID.
  Starts at startnodeID if given; otherswise it finds one to start with."
  (unless startnodeID
    (setf startnodeID (locate-local-node-for-graph graphID)))
  (when startnodeID
    (let ((solicitation (make-solicitation
                         :reply-to nil
                         :kind :k-dissolve
                         :forward-to t ; neighborcast, although dissolve handler will enforce this regardless
                         :graphID graphID 
                         :args nil)))
      ; must also destroy entry in locate-local-node-for-graph table (if any) for this graphID
      (send-msg solicitation
                startnodeID                   ; destination
                nil)
      t)))



;;; Utility routines

(defun locate-local-node-for-graph (graphID)
  )