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
  prev-ptrs
  trans
  keys)

(defvar *all-blocks*    (make-hash-table))
(defvar *current-block* (make-cosi-block))
  
(defun add-key-to-block (pkey proof)
  (push (cons pkey proof) (cosi-block-keys *current-block*)))

(defun add-transaction-to-block (trans)
  (push trans (cosi-block-trans *current-block*)))

(defun get-block-keys ()
  (cosi-block-keys *current-block*))

(defun get-block-transactions ()
  (cosi-block-trans *current-block*))

(defun vis-hash (&rest args)
  (cosi-keying:published-form
   (apply 'ecc-crypto-b571:sha3/256-buffers args)))

(defun convert-vis-to-vec (hash)
  (edec:ed-convert-int-to-lev (cosi-keying:need-integer-form hash) 32))

(defun hash-element (item)
  (vis-hash (loenc:encode item)))

(defun hash-pair-hashes (h1 h2)
  (vis-hash
   (convert-vis-to-vec h1)
   (convert-vis-to-vec h2)))

(defun merkle-tree (&rest args)
  (um:nlet-tail iter ((lst args)
                      (ans nil))
    (cond ((endp lst)
           (if (null (cdr ans))
               (or (car ans)
                   (hash-element nil))
             (merkle-tree (nreverse ans))))

          ((null (cdr lst))
           (hash-pair-hashes (car lst) (hash-element nil)))
          
          (t
           (iter (cddr lst)
                 (cons (hash-pair-hashes (car lst) (cadr lst))
                       ans)))
          )))
          
(defun publish-block ()
  (let* ((hash  (merkle-tree
                 (apply 'merkle-tree (cosi-block-prev-ptrs *current-block*))
                 (apply 'merkle-tree (mapcar 'hash-element
                                             (cosi-block-trans *current-block*)))
                 (apply 'merkle-tree (mapcar 'hash-element
                                             (cosi-block-keys *current-block*))))))
    (setf (cosi-block-blk-hash *current-block*) hash
          (gethash hash *all-blocks*) *current-block*)
    (labels ((prev-hash (n)
               (let ((blk (goback n)))
                 (when blk
                   (cosi-block-blk-hash blk)))))
      (let ((hash2  (prev-hash 1))
            (hash4  (prev-hash 3)))
        (setf *current-block* (make-cosi-block
                               :prev-ptrs (list hash hash2 hash4)))))
    ))

(defun goback (n &optional (start *current-block*))
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
        
        
