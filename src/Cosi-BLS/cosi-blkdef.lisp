;; cosi-blkdef.lisp -- Provisional structure of blocks
;; Just so we have something to test against...
;;
;; DM/Emotiq 02/18
;; ----------------------------------------------------------------
#|
The MIT License

Copyright (c) 2018 Emotiq AG

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :cosi-blkdef)

(defstruct cosi-block
  blk-hash
  prev-ptrs ;; list of hashes 1,2,4 back
  (trans     (ads-treap:make-authenticated-treap))   ;; Treap of transations + proofs
  (keys      (ads-treap:make-authenticated-treap)))  ;; Treap of new keys + proofs

(defvar *all-blocks*    (make-hash-table   ;; the dummy blockchain...
                         :test 'equal))
(defvar *current-block* (make-cosi-block)) ;; block we're working on

;; ----------------------------------

(defclass circ-queue ()
  ((nel
    ;; nbr items queue can hold
    :reader  circ-queue-nel
    :initarg :nel)
   (pos
    ;; current head position in queue
    :accessor circ-queue-pos
    :initform 0)
   (items
    ;; contents vector
    :accessor circ-queue-items)))

(defmethod initialize-instance :after ((q circ-queue) &key nel &allow-other-keys)
  (setf (circ-queue-items q) (make-array nel
                                         :initial-element nil)))

(defmethod mod-offs ((q circ-queue) ix)
  (mod (+ ix (circ-queue-pos q))
       (circ-queue-nel q)))

(defmethod qref ((q circ-queue) pos)
  (aref (circ-queue-items q)
        (mod-offs q pos)))

(defmethod set-qref ((q circ-queue) pos item)
  (setf (aref (circ-queue-items q)
              (mod-offs q pos))
        item))

(defsetf qref set-qref)

(defmethod insert-item ((q circ-queue) item)
  (setf (qref q 0)         item
        (circ-queue-pos q) (mod-offs q 1))
  item)

;; -------------------------------------

(defvar *block-queue*
  ;; holds the last 4 block hashes for fast backchain computation
  (make-instance 'circ-queue
                 :nel 4))

;; -------------------------------------

(defstruct pkey+prover
  pkey proof)

(defmethod ads-treap:treap-prio-for-item ((item pkey+prover))
  (pkey+prover-pkey item))

(defmethod ads-treap:treap-key-for-item ((item pkey+prover))
  (pkey+prover-pkey item))

(defun add-key-to-block (pkey proof)
  (ads-treap:insert (make-pkey+prover
                     :pkey pkey
                     :proof proof)
                    (cosi-block-keys *current-block*)))
                    
(defun get-block-keys ()
  (cosi-block-keys *current-block*))

;; -------------------------------------

(defstruct transaction
  pkey
  trans)

(defmethod ads-treap:treap-prio-for-item ((item transaction))
  (transaction-pkey item))

(defmethod ads-treap:treap-key-for-item ((item transaction))
  (transaction-pkey item))

(defun add-transaction-to-block (pkey trans)
  (ads-treap:insert (make-transaction
                     :pkey   pkey
                     :trans  trans)
                    (cosi-block-trans *current-block*)))

(defun get-block-transactions ()
  (cosi-block-trans *current-block*))

;; ----------------------------------------

(defun vis-hash (&rest args)
  (cosi-keying:published-form
   (apply 'sha3/256-buffers args)))

(defun convert-vis-to-vec (hash)
  (ed-convert-int-to-lev (cosi-keying:need-integer-form hash) 32))

(defun hash-element (item)
  (vis-hash (loenc:encode item)))

(defun hash-pair-hashes (h1 h2)
  (vis-hash
   (convert-vis-to-vec h1)
   (convert-vis-to-vec h2)))

(defun merkle-tree (&rest args)
  ;; all args are hashes
  (um:nlet-tail iter ((lst args)
                      (ans nil))
    (cond ((endp lst)
           (if (null (cdr ans))
               (or (car ans)
                   (hash-element nil))
             (apply 'merkle-tree (nreverse ans))))
          
          (t
           (iter (cddr lst)
                 (cons (hash-pair-hashes (car lst)
                                         (or (cadr lst)
                                             (hash-element nil)))
                       ans)))
          )))

(defun treap-digest (treap)
  (if treap
      (cosi-keying:published-form (ads:prover-digest treap))
    (hash-element nil)))

(defun publish-block ()
  ;; stuff current-block into the blockchain (hash table) and create a
  ;; new empty block
  (let* ((hash  (merkle-tree
                 (apply 'merkle-tree (cosi-block-prev-ptrs *current-block*))
                 (treap-digest (cosi-block-keys *current-block*))
                 (treap-digest (cosi-block-trans *current-block*)))))
    (when (gethash hash *all-blocks*)
      (error "Blockchain collision!!"))
    (setf (cosi-block-blk-hash *current-block*) hash
          (gethash hash *all-blocks*) *current-block*)
    (insert-item *block-queue* hash)
    (setf *current-block* (make-cosi-block
                           :prev-ptrs (delete nil (list hash
                                                        (qref *block-queue* -2)
                                                        (qref *block-queue* -4)))))
    ))

(defun goback (n &optional (start *current-block*))
  ;; move backward by n blocks from start block
  (um:nlet-tail iter ((n   n)
                      (blk start))
    (if (zerop n)
        blk
      (when blk
        (let ((prevs (cosi-block-prev-ptrs blk)))
          (when prevs
            (case n
              (1  (let ((prev (first prevs)))
                    (when prev
                      (gethash prev *all-blocks*))))
              (2  (let ((prev (second prevs)))
                    (when prev
                      (gethash prev *all-blocks*))))
              (3   (iter 1 (goback 2 blk)))
              (4   (let ((prev (third prevs)))
                     (when prev
                       (gethash prev *all-blocks*))))
              (t   (iter (- n 4) (goback 4 blk)))
              )))))))
        
        
