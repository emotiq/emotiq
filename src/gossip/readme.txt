;; quick-and-dirty test of gossip protocol

(ql:quickload :gossip)
(in-package :gossip)
(make-graph 10) ; make a connected graph with 10 nodes
(visualize-nodes *nodes*) ; only if you've installed graphviz
(run-gossip-sim)
(solicit (first (listify-nodes)) :announce :foo)
(inspect *log*)

(run-gossip-sim) ; just an easy way to clear the *log*
(solicit (first (listify-nodes)) :count-alive)
;; wait a few seconds, then
(inspect *log*)

;; You should see a final entry like
(:FINALREPLY "node1" "sol12" (2 9 8 3 7 ...)) ........ 
Where the list beginning "(2 9 8 3 7 ...)" should contain
all (or most) of the nodes in the graph.