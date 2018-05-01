# Gossip Documentation

Gossip protocol allows messages to be ...

Inline image test:

<img src="10nodes2.svg" alt="10-node network">

How I made the above graph:
```
(in-package :gossip)
(restore-graph-from-file "tests/10nodes2.lisp")
(visualize-nodes *nodes*)
```

How to make a random graph:
```
(in-package :gossip)
(setf *default-uid-style* :tiny)
(make-graph 10)
(visualize-nodes *nodes*)
```