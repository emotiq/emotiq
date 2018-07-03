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
  (uber-set))

(defun singlecast (message nodeID &key graphID (howmany 2) startnodeID)
  "High-level API for sending message to given single nodeID. If graphID
   is supplied, message will be forwarded along the graph, starting
   with some locally-known node that's part of that graph.
   If graphID is not supplied, a direct connection to nodeID will
   be attempted and used.
   If you want to use the default graph, you must explicitly pass *default-graphID* in graphID.
   Howmany is only used if graphID is supplied."
  (let ((solicitation nil))
    (if graphID
        (progn
          (unless startnodeID
            (setf startnodeID (locate-local-node-for-graph graphID)))
          (when startnodeID
            (setf solicitation (make-solicitation
                                :reply-to nil
                                :kind :k-singlecast
                                :forward-to howmany
                                :graphID graphID
                                :args (cons nodeID message)))
            (send-msg solicitation
                      startnodeID                   ; destination
                      nil)))
        
        ; otherwise ensure there's a node with the given nodeID (real or proxy) and send to it directly
        (progn
          (setf solicitation (make-solicitation
                              :reply-to nil
                              :kind :k-singlecast
                              :forward-to nil
                              :args (cons nodeID message)))
          (send-msg solicitation
                    nodeID                   ; destination
                    nil)))))

; Normally you wouldn't call this as a high-level application programmer. Call broadcast instead.
(defun gossipcast (message &key (graphID *default-graphID*) startnodeID (howmany 2) (kind ':k-multicast))
  "Sends message via traditional gossip.
  Howmany determines whether traditional gossip or neighborcast is used."
  (let ((solicitation (make-solicitation
                       :reply-to nil
                       :kind kind
                       :forward-to howmany ; traditional gossip
                       :graphID graphID
                       :args message)))
    (send-msg solicitation
              startnodeID                   ; destination
              nil)))

(defun broadcast (message &key (style ':neighborcast) (graphID *default-graphID*) startnodeID (kind ':k-multicast))
  "High-level API for broadcasting a message along a graph.
   If startnodeID is provided, it will be used as the starting node.
   If startnodeID is not provided, we'll start at some locally-known node that's part of given graphID.
   Use style = :neighborcast for most reliable transport.
   Use style = :gossip or some small integer (like 2 or 3) for better scalability on large graphs, at expense of some reliability.
   (An integer--if supplied--represents the maximum number of a node's neighbors to forward the message to.
    :neighborcast means 'use all the neighbors'. :gossip means 'use up to 2 neighbors'.)
   Normally an API programmer should let :kind default."
  (unless startnodeID
      (setf startnodeID (locate-local-node-for-graph graphID)))
  (cond
    ((eql ':neighborcast style) (gossipcast message :graphID graphID :startnodeID startnodeID :howmany t :kind kind))
    ((eql ':gossip style)       (gossipcast message :graphID graphID :startnodeID startnodeID :howmany 2 :kind kind))
    ((integerp style)           (gossipcast message :graphID graphID :startnodeID startnodeID :howmany style :kind kind))
    (t (error "Invalid style ~S" style))))

(defun hello (pkey ipaddr ipport &key (style ':neighborcast) (graphID *default-graphID*) startnodeID)
  "Announce the presence of pkey (node UID) at ipaddr and ipport to a graph. Nodes traversed will
   add the pkey/ipaddr pair to their knowledge of the :uber graph.
   graphID here is merely the graph traversed by this message; this does not connect nodes to any given
   graph except for the :uber graph."
  (broadcast (list pkey ipaddr ipport) :style style :graphID graphID :startnodeID startnodeID :kind ':k-hello))

;;; NOT DONE YET
(defun establish-star-broadcast-group (list-of-nodeIDs &key graphID center-nodeID)
  "Creates a star topology with center-nodeID in center.
  If no center-nodeID is given, a pseudo-node is created for that purpose.
  Returns given graphID or it makes a new one and returns that.
  If given graphID exists on these nodes, all connections on that graphID will be destroyed first."
  graphID
  )

;;; NOT DONE YET
(defun establish-gossip-broadcast-group (list-of-nodeIDs &key graphID)
 "Creates a connected graph (but usually not fully-connected) from list-of-nodeIDs.
  Returns given graphID or it makes a new one and returns that.
  If given graphID exists on any of these nodes, all connections on that graphID will be destroyed first."
  graphID
  )

;;; NOT DONE YET
(defun establish-broadcast-group (list-of-nodeIDs &key graphID)
   "High-level API call. Decides itself whether
   star or gossip is best, based on number of nodes, etc. (Large number of nodes suggests
   gossip is better.)
   If given graphID exists on any of these nodes, all connections on that graphID will be destroyed first."
  (dissolve-graph graphID)
  graphID
  )

(defun dissolve-graph (graphID &key startnodeID)
  "Dissolves all connections associated with graphID.
  Starts at startnodeID if given; otherswise it finds one to start with."
  ; Note that it makes no sense to dissolve the :uber graph, because the nodes
  ;  themselves don't even know about the uber graph. Dissolving the :uber graph is a no-op.
  ;  (Or should it mean "dissolve all graphs"?)
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

; (setf localnode (car (gossip::local-real-uids)))
; (hello localnode (eripa) 65002 :graphid ':uber :startnodeid (car (remote-real-uids)))