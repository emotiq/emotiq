;;; 10nodes2.lisp
;;; Saved graph file.
;;; Call gossip::restore-graph-from-file on this file to restore graph from it.

;;; Interesting graph. Start at node9.

(in-package :gossip)

(make-node
  :UID 9
  :ADDRESS 'NIL
  :NEIGHBORS '(2 4)
  :LOGFN 'INTERACTIVE-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 4
  :ADDRESS 'NIL
  :NEIGHBORS '(8 3 5 9)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 5
  :ADDRESS 'NIL
  :NEIGHBORS '(6 8 4)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 8
  :ADDRESS 'NIL
  :NEIGHBORS '(4 3 5)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 3
  :ADDRESS 'NIL
  :NEIGHBORS '(4 1 8)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 1
  :ADDRESS 'NIL
  :NEIGHBORS '(2 6 3)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 6
  :ADDRESS 'NIL
  :NEIGHBORS '(5 2 1)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 2
  :ADDRESS 'NIL
  :NEIGHBORS '(9 1 7 6)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 7
  :ADDRESS 'NIL
  :NEIGHBORS '(10 2)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 10
  :ADDRESS 'NIL
  :NEIGHBORS '(7)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
