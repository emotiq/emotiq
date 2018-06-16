;;; gossip-tests.lisp

(in-package :gossip-tests)

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
  (gossip::gossip-init ':uninit))

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
      (let ((oldnodes *nodes*)
            (oldlogdots *log-dots*))
        (set-protocol-style :neighborcast)
        (unwind-protect
             (progn 
               (setf *log-dots* t)
               (setf *nodes* (gossip::make-uid-mapper))
               (clear-local-nodes)
               (make-test-network)
               (ac::kill-executives)
               (assert-eql 10 (run-gossip-sim))
               (let* ((nodes (listify-nodes))
                      (uids (mapcar #'gossip::uid nodes)))
                                        ; do same test starting at every possible node
                 (mapc '%aliveness uids)))
          (setf *nodes* oldnodes
                *log-dots* oldlogdots)))))

; (let ((*print-failures* t)) (run-tests '(key-value)))
(define-test key-value
             "Test key-value protocols"
  ;;; careful with globals. Remember that they get a different value
  ;;; in each thread if you're not careful.  So we set them here
  ;;; rather than bind them.
  (with-networking              
    (let ((oldnodes *nodes*)
          (oldlogdots *log-dots*))
      (set-protocol-style :neighborcast)
      (unwind-protect
           (progn
             (setf *log-dots* t)
             (setf *nodes* (gossip::make-uid-mapper))
             (clear-local-nodes)
             (make-test-network)
             (ac::kill-executives)
             (assert-eql 10 (run-gossip-sim))
             (let* ((nodes (listify-nodes))
                    (uids (mapcar #'gossip::uid nodes)))
            ;; do same test starting at every possible node
               (mapc '%key-value uids)))
        (setf *nodes* oldnodes
              *log-dots* oldlogdots)))))

(define-test partial-gossip "Test partial-gossip with parameter 2"
  ;;; careful with globals. Remember that they get a different value
  ;;; in each thread if you're not careful.  So we set them here
  ;;; rather than bind them.
  (with-networking
    (let ((oldnodes *nodes*)
          (oldlogdots *log-dots*))
      (set-protocol-style :gossip 2)
      (unwind-protect
           (let ((results nil)
                 (*log-filter* nil))
             (setf *log-dots* t)
             (setf *nodes* (gossip::make-uid-mapper))
             (clear-local-nodes)
             (make-graph 100)
             (ac::kill-executives)
             (assert-eql 100 (run-gossip-sim))
             (dotimes (i 100)
               (push (unwrap (solicit-wait (random-node) :count-alive)) results))
             (setf results (mapcar (lambda (x) (if (numberp x) x nil)) results)) ; ensure :TIMEOUT doesn't appear in data
             (assert (lambda ()
                       (every (lambda (x)
                                (if (numberp x)
                                    (> x 90)
                                    t))
                              results))))
        (setf *nodes* oldnodes
              *log-dots* oldlogdots)))))

; (let ((*print-failures* t)) (run-tests '(partial-gossip)))
; (let ((*print-failures* t)) (run-tests :all))

(defun make-test-network ()
  "Interesting graph of 10 nodes."
  (make-node
   :UID 9
   :ADDRESS 'NIL
   :neighborhood '(2 4)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 4
   :ADDRESS 'NIL
   :neighborhood '(8 3 5 9)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 5
   :ADDRESS 'NIL
   :neighborhood '(6 8 4)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 8
   :ADDRESS 'NIL
   :neighborhood '(4 3 5)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 3
   :ADDRESS 'NIL
   :neighborhood '(4 1 8)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 1
   :ADDRESS 'NIL
   :neighborhood '(2 6 3)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 6
   :ADDRESS 'NIL
   :neighborhood '(5 2 1)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 2
   :ADDRESS 'NIL
   :neighborhood '(9 1 7 6)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 7
   :ADDRESS 'NIL
   :neighborhood '(10 2)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   )
  (make-node
   :UID 10
   :ADDRESS 'NIL
   :neighborhood '(7)
   :local-kvs (as-hash-table
               'EQUAL
               'NIL)
   ))
