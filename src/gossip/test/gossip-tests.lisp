(in-package :gossip-test)

(defparameter *debug* nil
  "True if you want intermediate timings to print out and show inspector on failures")

(eval-when (:load-toplevel :execute)
  (lisp-unit:remove-tests :all))

(defmacro with-networking (&body body)
  `(progn
     (build-fixture)
     ,@body
     (teardown-fixture)))
                                      
(defun build-fixture ()
  (gossip::gossip-init ':maybe))

(defun teardown-fixture ()
  (gossip::gossip-init ':uninit)
  )

(defun %aliveness (uid)
  "Runs test sending initial message to node with given uid"
  (assert-eql 10 (unwrap (solicit-wait uid :count-alive)))
  (assert-true
   (unordered-equal '(1 2 3 4 5 6 7 8 9 10)
                    (unwrap (solicit-wait uid :list-alive)))))

(defun %key-value (uid)
  (gossip:archive-log)
  (solicit uid :gossip-relate-unique :foo :bar)
  (sleep .1) ; give it time to work [solicit won't wait]
  (when *debug* (format t "~%UID: ~D g-r-u Elapsed-time: ~S" uid (gossip:measure-timing :ACCEPTED)))
  (assert-equal
   '((:BAR . 10))
   (unwrap (solicit-wait uid :gossip-lookup-key :foo)))
  (when *debug* (inspect gossip::*log*))
  (gossip:archive-log)
  (solicit uid :gossip-remove-key :foo)
  (sleep .1)
  (when *debug* (format t "~%UID: ~D g-r-k Elapsed-time: ~S" uid (gossip:measure-timing :ACCEPTED)))
  (gossip:archive-log)
  (assert-equal
   '((NIL . 10))
   (unwrap (solicit-wait uid :gossip-lookup-key :foo)))
  (gossip:archive-log)
  (solicit uid :gossip-tally :foo 1)
  (sleep .1)
  (when *debug* (format t "~%UID: ~D g-t1 Elapsed-time: ~S" uid (gossip:measure-timing :ACCEPTED)))
  (gossip:archive-log)
  (assert-equal
   '((1 . 10))
   (unwrap (solicit-wait uid :gossip-lookup-key :foo)))
  (when *debug* (inspect gossip::*log*))
  (gossip:archive-log)
  (solicit uid :gossip-tally :foo 1)
  (sleep .1)
  (when *debug* (format t "~%UID: ~D g-t2 Elapsed-time: ~S" uid (gossip:measure-timing :ACCEPTED)))
  (gossip:archive-log)
  (assert-equal
   '((2 . 10))
   (unwrap (solicit-wait uid :gossip-lookup-key :foo)))
  (when *debug* (inspect gossip::*log*)))

; (let ((*print-failures* t)) (run-tests '(aliveness)))
(define-test aliveness
             "Test count-alive and list-alive"
  ;;; careful with globals. Remember that they get a different value
  ;;; in each thread if you're not careful.  So we set them here
  ;;; rather than bind them.
  (with-networking 
      (let ((oldnodes *nodes*))
        (set-protocol-style :neighborcast)
        (unwind-protect
             (progn 
               (setf *nodes* (gossip::make-uid-mapper))
               (clear-local-nodes)
               (make-test-network)
               (ac::kill-executives)
               (assert-eql 10 (run-gossip))
               (let* ((nodes (listify-nodes))
                      (uids (mapcar #'gossip::uid nodes)))
                                        ; do same test starting at every possible node
                 (mapc '%aliveness uids)))
          (setf *nodes* oldnodes)))))

; (let ((*print-failures* t)) (run-tests '(key-value)))
(define-test key-value
             "Test key-value protocols"
  ;;; careful with globals. Remember that they get a different value
  ;;; in each thread if you're not careful.  So we set them here
  ;;; rather than bind them.
  (with-networking              
    (let ((oldnodes *nodes*))
      (set-protocol-style :neighborcast)
      (unwind-protect
           (progn
             (setf *nodes* (gossip::make-uid-mapper))
             (clear-local-nodes)
             (make-test-network)
             (ac::kill-executives)
             (assert-eql 10 (run-gossip))
             (let* ((nodes (listify-nodes))
                    (uids (mapcar #'gossip::uid nodes)))
            ;; do same test starting at every possible node
               (mapc '%key-value uids)))
        (setf *nodes* oldnodes)))))

(define-test partial-gossip "Test partial-gossip with parameter 2"
  ;;; careful with globals. Remember that they get a different value
  ;;; in each thread if you're not careful.  So we set them here
  ;;; rather than bind them.
  (with-networking
    (let ((oldnodes *nodes*))
      (set-protocol-style :gossip 2)
      (unwind-protect
           (let ((results nil)
                 (*log-filter* nil))
             (setf *nodes* (gossip::make-uid-mapper))
             (clear-local-nodes)
             (make-graph 100)
             (ac::kill-executives)
             (assert-eql 100 (run-gossip))
             (dotimes (i 100)
               (push (unwrap (solicit-wait (random-node) :count-alive)) results))
             (setf results (mapcar (lambda (x) (if (numberp x) x nil)) results)) ; ensure :TIMEOUT doesn't appear in data
             (assert (lambda ()
                       (every (lambda (x)
                                (if (numberp x)
                                    (> x 90)
                                    t))
                              results))))
        (setf *nodes* oldnodes)))))

; (let ((*print-failures* t)) (run-tests '(partial-gossip)))
; (let ((*print-failures* t)) (run-tests :all))

(defun random-choice (list)
  "Returns a random element from list"
  (nth (random (length list)) list))

; It's normal to see invalid-send-target errors in the log after this test, because we're destroying
;   *nodes* while extant messages are still being sent. This doesn't hurt anything.
(define-test singlecast-local "Test singlecast"
  ;;; careful with globals. Remember that they get a different value
  ;;; in each thread if you're not careful.  So we set them here
  ;;; rather than bind them.
  (with-networking
      (let ((oldnodes *nodes*)
            (numnodes 100)
            (old-application-handler *ll-application-handler*))
        (unwind-protect
            (let ((results nil)
                  (*log-filter* nil)
                  (msg '(foo message)))
              (setf *nodes* (gossip::make-uid-mapper))
              (setf *ll-application-handler* (lambda (node full-msg) (setf results (list (gossip::hopcount full-msg) node full-msg))))
              (clear-local-nodes)
              (gossip::make-nodes numnodes)
              (ac::kill-executives)
              (assert-eql numnodes (length (get-live-uids)))
              (assert-eql numnodes (run-gossip))
              (let* ((somelocal (gossip::locate-local-node-for-graph :uber))
                     (sometarget (random-choice (remove somelocal (gossip::local-real-uids)))))
                (assert-true (member somelocal (get-live-uids)))
                (singlecast msg sometarget :startnodeID somelocal :howmany t
                            ; Note: :howmany is t because we must use neighborcast here for the :uber network.
                            ;   That's the only way to be sure the network is connected (and in the case of the :uber network,
                            ;   it's fully-connected.)
                            :graphID ':uber)
                (mpcompat:process-wait-with-timeout "answer-wait" 5 (lambda () results))
                (assert-true results)
                (when results (assert-equal msg (cdr (gossip::args (third results)))))
                (when *debug* (format t "~%SINGLECAST-LOCAL Hopcount=~D" (first results)))
                ; (print results) contains hopcount, node, and full message
                ))
          (setf *nodes* oldnodes
                *ll-application-handler* old-application-handler)))))

; (let ((lisp-unit::*use-debugger* t)(*print-failures* t)) (run-tests '(singlecast-local)))

; Also need tests on singlecast-remote and singlecast-direct [no graphID]

; It's normal to see invalid-send-target errors in the log after this test, because we're destroying
;   *nodes* while extant messages are still being sent. This doesn't hurt anything.
(define-test broadcast-local "Test broadcast"
  ;;; careful with globals. Remember that they get a different value
  ;;; in each thread if you're not careful.  So we set them here
  ;;; rather than bind them.
  (with-networking
      (let ((oldnodes *nodes*)
            (numnodes 100)
            (old-application-handler *ll-application-handler*))
        (unwind-protect
            (let ((results nil)
                  (resultlock (mpcompat:make-lock))
                  (*log-filter* nil)
                  (msg '(foo message)))
              (setf *nodes* (gossip::make-uid-mapper))
              ; every node that receives the message will push its id onto results
              (setf *ll-application-handler* (lambda (node full-msg)
                                               (declare (ignore full-msg))
                                               (mpcompat:with-lock (resultlock)
                                                 (push node results))))
              (clear-local-nodes)
              (gossip::make-nodes numnodes)
              (ac::kill-executives)
              (assert-eql numnodes (length (get-live-uids)))
              (assert-eql numnodes (run-gossip))
              (let* ((somelocal (gossip::locate-local-node-for-graph :uber)))
                (assert-true (member somelocal (get-live-uids)))
                (broadcast msg :style ':neighborcast :graphID ':uber :startnodeID somelocal)
                ; wait until over half of nodes have seen the message, or 5 seconds, whichever comes first
                (mpcompat:process-wait-with-timeout "answer-wait" 5 (lambda () (> (length results) (/ numnodes 2))))
                (assert-true results)
                (when *debug* (format t "~%BROADCAST-LOCAL ~D" (length results)))
                (sleep 1) ; just to allow the remainder of nodes to see the message
                (assert-true (= (length results) numnodes))))
          (setf *nodes* oldnodes
                *ll-application-handler* old-application-handler)))))

; (let ((lisp-unit::*use-debugger* t)(*print-failures* t)) (run-tests '(broadcast-local)))

; Also need tests on broadcast-remote and broadcast-gossip [not neighborcast]

(defun make-test-network ()
  "Interesting graph of 10 nodes."
  (make-node ':gossip
   :temporary-p t
   :UID 9
   :neighborhood '(2 4)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node ':gossip
   :temporary-p t
   :UID 4
   :neighborhood '(8 3 5 9)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node ':gossip
   :temporary-p t
   :UID 5
   :neighborhood '(6 8 4)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node ':gossip
   :temporary-p t
   :UID 8
   :neighborhood '(4 3 5)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node ':gossip
   :temporary-p t
   :UID 3
   :neighborhood '(4 1 8)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node ':gossip
   :temporary-p t
   :UID 1
   :neighborhood '(2 6 3)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node ':gossip
   :temporary-p t
   :UID 6
   :neighborhood '(5 2 1)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node ':gossip
   :temporary-p t
   :UID 2
   :neighborhood '(9 1 7 6)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node ':gossip
   :temporary-p t
   :UID 7
   :neighborhood '(10 2)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node ':gossip
   :temporary-p t
   :UID 10
   :neighborhood '(7)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   ))
