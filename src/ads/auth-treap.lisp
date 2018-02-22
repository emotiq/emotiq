;; auth-treap.lisp -- Authenticated Treaps
;;
;; DM/Emotiq  01/18
;; ------------------------------------------------------------------
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

(defclass treap ()
  ((prio
    :reader   treap-prio
    :initarg  :prio)
   (key
    :reader   treap-key
    :initarg  :key)
   (val
    :reader   treap-val
    :initarg  :val)
   (left
    :accessor treap-left
    :initarg  :left
    :initform nil)
   (right
    :accessor treap-right
    :initarg  :right
    :initform nil)))

(defmethod shallow ((node treap))
  (make-instance 'treap
                 :prio  (treap-prio node)
                 :key   (treap-key node)
                 :val   (treap-val node)
                 :left  (shallow (treap-left node))
                 :right (shallow (treap-right node)) ))

(defmethod fetch ((node treap) (path cons))
  (fetch (ecase (car path)
           (:L (treap-left node))
           (:R (treap-right node)))
         (cdr path)))

(defmethod fetch ((node treap) (path null))
  (treap-val node))

;; ------------------------------------------------------------

(defmethod treap-prio-for-item (item)
  ;; default method - subclass should override
  (hash item))

(defmethod treap-key-for-item (item)
  ;; default method - subclass should override
  item)

;; ------------------------------------------------------------

(defun clone-node (node &key
                        (left  (treap-left node))
                        (right (treap-right node)))
  (auth
   (make-instance 'treap
                  :prio  (treap-prio node)
                  :key   (treap-key node)
                  :val   (treap-val node)
                  :left  left
                  :right right)))

;; ------------------------------------------------------------
;; PROBE -- get past the Auth-Type envelope to the contained item

(defmethod probe (x)
  x)

(defmethod probe ((x prover))
  (probe (prover-val x)))

(defmethod probe ((x digest))
  (error "Can't probe a Digest"))

;; ------------------------------------------------------------
;; INSERT - add/update an item in the Treap. Purely functional, no
;; mutation of original Treap. Can't be usefully applied by Verifiers.

(defmethod insert (item tree)
  (insert
   (make-instance 'treap
                  :prio (treap-prio-for-item item)
                  :key  (treap-key-for-item item)
                  :val  item)
   tree))

(defmethod insert ((node treap) (tree null))
  (auth node))

(defmethod insert (node (tree prover))
  (insert node (probe tree)))

(defmethod insert ((node treap) (tree treap))
  (let* ((node-key (treap-key node))
         (top-key  (treap-key tree))
         (kcmp     (compare node-key top-key)))
    (cond ((zerop kcmp)
           ;; replace existing by node
           (setf (treap-left node) (treap-left tree)
                 (treap-right node) (treap-right tree))
           (auth node))

          ((minusp kcmp)
           (let* ((left-child (insert node (treap-left tree)))
                  (tleft      (probe left-child))
                  (pcmp       (compare (treap-prio tleft) (treap-prio tree))))
             (cond ((plusp pcmp)
                    (clone-node tleft
                                :right (clone-node tree
                                                   :left (treap-right tleft))))
                   (t
                    (clone-node tree
                                :left left-child))
                   )))

          ((plusp kcmp)
           (let* ((right-child (insert node (treap-right tree)))
                  (tright      (probe right-child))
                  (pcmp        (compare (treap-prio tright) (treap-prio tree))))
             (cond ((plusp pcmp)
                    (clone-node tright
                                :left (clone-node tree
                                                  :right (treap-left tright))))
                   (t
                    (clone-node tree
                                :right right-child))
                   )))
          )))

(defun make-authenticated-treap (&rest items)
  (reduce (lambda (tree item)
            (insert item tree))
          items
          :initial-value nil))

;; --------------------------------------------------------------------
;; for visual debugging...

#+:LISPWORKS
(progn
  (defmethod treap-children (node layout)
    nil)
  
  (defmethod treap-children ((node prover) layout)
    (treap-children (probe node) layout))

  (defmethod treap-children ((node treap) layout)
    (multiple-value-bind (l r)
        (values (treap-left node)
                (treap-right node))
      (let ((lx (or l
                    (vector)))
            (rx (or r
                    (vector))))
        (cond ((and (null l)
                    (null r))
               nil)
              (t
               (case layout
                 ((:left-right :right-left) (list rx lx))
                 (t   (list lx rx))))
              ))))

  (defmethod print-node (x keyfn)
    nil)

  (defmethod print-node ((node prover) keyfn)
    (print-node (probe node) keyfn))

  (defmethod print-node ((node treap) keyfn)
    (format nil "~A" (funcall keyfn (treap-key node))))
  
  (defmethod view-treap ((tree prover) &key (key #'identity) (layout :left-right))
    (view-treap (probe tree) :key key :layout layout))

  (defmethod view-treap ((tree treap) &key (key #'identity) (layout :left-right))
    (capi:contain
     (make-instance 'capi:graph-pane
                    :layout-function layout
                    :roots (list tree)
                    :children-function (lambda (node)
                                         (treap-children node layout))
                    :print-function (lambda (node)
                                      (print-node node key))
                    ))))

;; --------------------------------------------------------------------

#|
 ;; test it out... result should be independent of insertion order
(let* ((items '(:one :two :three :four :five :six :seven))
       (tree  (apply 'make-authenticated-treap items)))
  (inspect tree)
  (view-treap tree))

(setf x
  (let* ((items '(:one :two :three :four :five :six :seven))
	 (tree  (apply 'make-authenticated-treap items)))
    tree))
|#
