;;; gossip-tests.lisp

(in-package :gossip-tests)

(REMOVE-TESTS :ALL)

(defun %aliveness (uid)
  "Runs test sending initial message to node with given uid"
  (assert-eql 10 (solicit-wait uid :count-alive))
  (assert-true
   (unordered-equal '(1 2 3 4 5 6 7 8 9 10)
                    (solicit-wait uid :list-alive))))

(defun %key-value (uid)
  (solicit uid :gossip-relate-unique :foo :bar)
  (sleep .5) ; give it time to work [solicit won't wait]
  (assert-equal
   '((:BAR . 10))
   (solicit-wait uid :gossip-lookup-key :foo))
  
  (solicit uid :gossip-remove-key :foo)
  (sleep .5)
  (assert-equal
   '((NIL . 10))
   (solicit-wait uid :gossip-lookup-key :foo))
  (solicit uid :gossip-tally :foo 1)
  (sleep .5)
  (assert-equal
   '((1 . 10))
   (solicit-wait uid :gossip-lookup-key :foo))
  (solicit uid :gossip-tally :foo 1)
  (sleep .5)
  (assert-equal
   '((2 . 10))
   (solicit-wait uid :gossip-lookup-key :foo)))

; (let ((*print-failures* t)) (run-tests '(aliveness)))
(define-test aliveness
             "Test count-alive, list-alive, and key-value protocols"
  (let ((oldnodes *nodes*))
    (unwind-protect
        (progn
          (setf *nodes* (gossip::make-uid-mapper))
          (clrhash *nodes*)
          (make-test-network)
          (ac::kill-executives)
          (assert-eql 10 (run-gossip-sim))
          (let* ((nodes (listify-nodes))
                 (uids (mapcar #'gossip::uid nodes)))
            ; do same test starting at every possible node
            (mapc '%aliveness uids)
            ))
      (setf *nodes* oldnodes)
      )))

; (let ((*print-failures* t)) (run-tests '(key-value)))
(define-test key-value
             "Test count-alive, list-alive, and key-value protocols"
  (let ((oldnodes *nodes*))
    (unwind-protect
        (progn
          (setf *nodes* (gossip::make-uid-mapper))
          (clrhash *nodes*)
          (make-test-network)
          (ac::kill-executives)
          (assert-eql 10 (run-gossip-sim))
          (let* ((nodes (listify-nodes))
                 (uids (mapcar #'gossip::uid nodes)))
            ; do same test starting at every possible node
            (mapc '%key-value uids)
            ))
      (setf *nodes* oldnodes)
      )))

; (let ((*print-failures* t))  (run-tests :all))

(defun make-test-network ()
  "Interesting graph of 10 nodes."
  (make-node
   :UID 9
   :ADDRESS 'NIL
   :NEIGHBORS '(2 4)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 4
   :ADDRESS 'NIL
   :NEIGHBORS '(8 3 5 9)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 5
   :ADDRESS 'NIL
   :NEIGHBORS '(6 8 4)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 8
   :ADDRESS 'NIL
   :NEIGHBORS '(4 3 5)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 3
   :ADDRESS 'NIL
   :NEIGHBORS '(4 1 8)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 1
   :ADDRESS 'NIL
   :NEIGHBORS '(2 6 3)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 6
   :ADDRESS 'NIL
   :NEIGHBORS '(5 2 1)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 2
   :ADDRESS 'NIL
   :NEIGHBORS '(9 1 7 6)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 7
   :ADDRESS 'NIL
   :NEIGHBORS '(10 2)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 10
   :ADDRESS 'NIL
   :NEIGHBORS '(7)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   ))