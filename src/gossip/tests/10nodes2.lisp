;;; 10nodes2.lisp
;;; Saved graph file.
;;; Call gossip::restore-graph-from-file on this file to restore graph from it.

;;; Interesting graph. Start at node9.

(in-package :gossip)

(make-node
  :UID 9
  :ADDRESS 'NIL
  :neighborhood '(2 4)
  :LOGFN 'INTERACTIVE-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 4
  :ADDRESS 'NIL
  :neighborhood '(8 3 5 9)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 5
  :ADDRESS 'NIL
  :neighborhood '(6 8 4)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 8
  :ADDRESS 'NIL
  :neighborhood '(4 3 5)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 3
  :ADDRESS 'NIL
  :neighborhood '(4 1 8)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 1
  :ADDRESS 'NIL
  :neighborhood '(2 6 3)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 6
  :ADDRESS 'NIL
  :neighborhood '(5 2 1)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 2
  :ADDRESS 'NIL
  :neighborhood '(9 1 7 6)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 7
  :ADDRESS 'NIL
  :neighborhood '(10 2)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
(make-node
  :UID 10
  :ADDRESS 'NIL
  :neighborhood '(7)
  :LOGFN 'DEFAULT-LOGGING-FUNCTION
  :local-kvs (as-hash-table
  'EQUAL
  'NIL)
 )
