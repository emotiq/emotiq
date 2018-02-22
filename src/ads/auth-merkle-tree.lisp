;; auth-merkle-tree.lisp - an example of an Authenticated Data Structure
;;
;; DM/Emotiq 01/18
;; --------------------------------------------------------------------
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


(in-package :ads)

(defclass merkle-tree ()
  ;; abstract base closs for Merkle trees made up of Tips and Nodes
  ;; Tips contain data, Nodes point to two other Trees:
  ;;
  ;;  Type 'a tree = Tip ('a) | Node ('a tree * 'a tree)
  ())

(defun tip-expected ()
  (error "Merkle Tip expected"))

(defun node-expected ()
  (error "Merkle Node expected"))

;; ----------------------------------------------------------------------

(defclass merkle-tip (merkle-tree)
  ;; Tips hold actual data values, and are the leaves of the tree.
  ;; Tips should never hold authenticated data types for their value.
  ((val  :reader  merkle-tip-val
         :initarg :val)))

;; ----------------------------------------------------------------------

(defclass merkle-node (merkle-tree)
  ;; Nodes are interior nodes of the tree, and point to other Nodes or
  ;; Tips. Nodes can hold either authenticated or unauthenticated sub-trees.
  ((lnode :reader  merkle-node-l
          :initarg :L)
   (rnode :reader  merkle-node-r
          :initarg :R)))

(defmethod shallow ((node merkle-node))
  ;; construct an unauthenticated version of a node
  (make-instance 'merkle-node
                 :L (shallow (merkle-node-l node))
                 :R (shallow (merkle-node-r node))))

;; -----------------------------------------------

(defmethod fetch ((node merkle-node) (path cons))
  (fetch (ecase (car path)
           (:L  (merkle-node-l node))
           (:R  (merkle-node-r node)))
         (cdr path)))

(defmethod fetch ((node merkle-node) (path null))
  (tip-expected))

(defmethod fetch ((tip merkle-tip) (path null))
  (merkle-tip-val tip))

(defmethod fetch ((tip merkle-tip) (path cons))
  (node-expected))

;; -----------------------------------------------

(defun make-authenticated-merkle-tree (&rest items)
  ;; construct a fully authenticated Merkle tree from a list of value
  ;; items.
  (labels
      ((pair-off (nodes &optional acc)
         (cond ((null nodes)
                (nreverse acc))
               
               ((singleton? nodes)
                (nreverse (append nodes acc)))

               (t
                (pair-off (cddr nodes)
                          (cons (make-instance 'merkle-node
                                               :L (auth (car nodes))
                                               :R (auth (cadr nodes)))
                                acc)))
               ))
       (form-tree (nodes)
         (if (singleton? nodes) ;; shrunk to one node yet?
             ;; return a Prover to the top node of the tree
             (auth (car nodes))
           ;; else -- iterate by successive pairs
           (form-tree (pair-off nodes)))))
    
    (form-tree (mapcar (lambda (val)
                         ;; yes - I know this seems like gratuitous
                         ;; boxing, but bear with me...
                         (make-instance 'merkle-tip
                                        :val val))
                       items))))

;; --------------------------------------------------------

#|
(let* ((tree  (make-authenticated-merkle-tree "One" "Two" "Three"))
       (path  '(:L :R)))
  ;; should have: '(:L :L) = "One"
  ;;              '(:L :R) = "Two"
  ;;              '(:R)    = "Three"
  (multiple-value-bind (ans wlist)
      (prove (fetch tree path))
    (inspect (mapcar 'witness-val wlist)) ;; each witness is a shallow except final one
    (verify wlist (fetch (shallow tree) path))))

;;
;; you can obtain the overall hash of an ADS by constructing in VERIFY
;; mode with a null witness list... The VERIFY construction consumes
;; only constant space.
;;
(let* ((tree (make-authenticated-merkle-tree :one :two :three))
       (dig  (verify nil (make-authenticated-merkle-tree :one :two :three))))
  (hash= (digest-hashval dig) (prover-digest tree)))
|#

;; -------------------------------------------------------------------------

(defmethod update ((tip merkle-tip) (path null) new-val)
  (auth (make-instance 'merkle-tip
                       :val  new-val)))

(defmethod update ((tip merkle-tip) (path cons) new-val)
  (node-expected))


(defmethod update ((node merkle-node) (path cons) new-val)
  (let ((l  (merkle-node-l node))
        (r  (merkle-node-r node)))
    (auth (ecase (car path)
            (:L  (make-instance 'merkle-node
                                :L (update l (cdr path) new-val)
                                :R r))
            (:R  (make-instance 'merkle-node
                                :L l
                                :R (update r (cdr path) new-val)))
            ))))

(defmethod update ((node merkle-node) (path null) new-val)
  (tip-expected))

;; -------------------------------------------------------------------------
;; Discard some branch or tip of the Merkle tree, producing a modified
;; tree as a result. This is purely functional and will not mutate the
;; original tree.
;;
;; Call (DISCARD tree NIL), i.e., with an empty path, discards the
;; whole tree and returns a NIL value. Not an error, per se, but also
;; not a MERKLE-TREE. If you really want an authenticated value, then
;; call (AUTH NIL).

(defmethod discard ((tip merkle-tip) (path null))
  ;; If we reach here, then Merkle tree has vaporized...  We shouldn't
  ;; ever reach a tip, unless that was the only thing in the tree.
  nil)

(defmethod discard ((tip merkle-tip) (path cons))
  ;; oops! the path must have been too long for this tree
  (node-expected))

(defmethod discard ((node merkle-node) (path cons))
  ;; Discard permits the excision of whole branches if we are supplied
  ;; a short path
  (let ((l  (merkle-node-l node))
        (r  (merkle-node-r node)))
    (auth
     (if (singleton? path) ;; at last element of path
         ;; we're in the parent node of two sub-trees
         ;; one of them has to go...
         (ecase (car path)
           (:L  r)
           (:R  l))
       ;; else - we're at an interior node along the path
       (ecase (car path)
         (:L  (make-instance 'merkle-node
                             :L (discard l (cdr path))
                             :R r))
         (:R  (make-instance 'merkle-node
                             :L  l
                             :R  (discard r (cdr path))))
         )))
    ))

(defmethod discard ((node merkle-node) (path null))
  ;; Wow! You really want to discard the whole thing?  The only way to
  ;; get here is by calling DISCARD on the top node of the tree with
  ;; an empty path.
  nil)


