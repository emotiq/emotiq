;; election.lisp -- Leader Elections
;;
;; DM/Emotiq  03/18
;; ---------------------------------------------------------------
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

(in-package :sim-trial-electionb)
;; ---------------------------------------------------------------

(defvar *beacon-timer*     nil)
(defvar *beacon-interval*  30)
(defvar *self-destruct*    nil)

(defun kill-beacon ()
  (setf *self-destruct* t))

;; mp is the multi-processing library for LispWorks
(defun make-trial-election-beacon ()
  (unless *beacon-timer*
    (setf *beacon-timer* (mp:make-timer (lambda ()
                                          (cond (*self-destruct*
                                                 (mp:unschedule-timer *beacon-timer*)
                                                 (setf *self-destruct* nil
                                                       *beacon-timer*  nil))
                                                (t
                                                 (send-all (/ (random 1000000)
                                                              1000000)))
                                                ))
                                        ))
    (mp:schedule-timer-relative *beacon-timer*
                                *beacon-interval*
                                *beacon-interval*)))


;; ordered list/vector of all known stakeholders
;; in real life, this should be an ordered list/vector of *real-nodes*
;; in the simulator, 1 machine, this is a list of all nodes (fake and real)
;; and, in the simulator, we only hold mock elections, and print the
;; results, since only real nodes can be leaders
(defvar *all-nodes*  nil) 

(defun set-nodes (sorted-node-list)
  (setf *all-nodes* sorted-node-list))


(defun send-all (n)
  (mapc (lambda (node)
          (send (node-ip-addr node) :hold-an-election n))
        *all-nodes*))

;; --------------------------------------------------

(defclass tree-node ()
  ((l   :reader tree-node-l
        :initarg :l
        :initform nil)
   (r   :reader tree-node-r
        :initarg :r
        :initform nil)
   (sum :reader tree-node-sum
        :initarg :sum)
   (cosi-node :reader tree-node-node
              :initarg :cosi-node)
  (:documentation "Election tree node"))

(defmethod tree-node-p (x)
  nil)

(defmethod tree-node-p ((x tree-node))
  t)

(defmethod node-stake ((node tree-node))
  (tree-node-sum node))

(defmethod node-stake ((node cosi-simgen::node))
  (cosi-simgen::node-stake node))
  
;; a tree of stakes, cosi-simgen::nodes at the leaves (very bottom of tree)

(defun make-tree-node (pair)
  (destructuring-bind (l &optional r) pair
    (if r
        (make-instance 'tree-node
                       :l  l
                       :r  r
                       :sum (+ (node-stake l)
                               (node-stake r)))
      ;; else
      l)))

(defun hold-trial-election (nfrac)
  "Given a fraction (0 < nfrac < 1) arrange all stakeholders into a
binary decision tree, and determine the node which wins the election,
based on their relative stake"
  (let* ((tree    (um:nlet-tail iter ((nodes *all-nodes*))  ;; tail recursive, scheme-like, named let
                    (if (= 1 (length nodes))
                        nodes
                      (iter (mapcar 'make-tree-node (um:group nodes 2)))))))
    (um:nlet-tail iter ((tree  tree))                       ;; tail recursive, scheme-like, named let
      (if (tree-node-p tree)
          (let* ((l      (tree-node-l tree))
                 (r      (tree-node-r tree))
                 (lstake  (node-stake l))
                 (tstake  (node-stake tree)))
            (if (< (/ lstake tstake) nfrac)
                (iter r)
              (iter l)))
        ;; else - we have arrived at a winner
        tree))))

        
    
                                      
