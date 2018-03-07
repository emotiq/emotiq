;; file-btrees.lisp -- Memory-mapped file-based B-Trees
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

(fli:define-c-struct tree-node_t
  (:byte-packing 1)
  (sys           memory-block-header_t)
  (height        uint16)
  (fill-pointer  uint16)
  (filler        uint32)
  (node-list     (:c-array off_t 0)))

;; -----------------------------------

(defclass file-node (btree:node)
  ((name              :reader        file-node-name
                      :initarg       :name)
   
   (height            :accessor      btree:node-height
                      :allocation    :persistent
                      :initform      :no-init
                      :c-access-spec 'height
                      :c-type        uint16
                      :constraint    'mmf:i>0)

   (fill-pointer      :accessor      btree:node-fill-pointer
                      :allocation    :persistent
                      :initform      :no-init
                      :c-access-spec 'fill-pointer
                      :c-type        uint16)

   (node-list-pointer :reader        node-list-pointer
                      :allocation    :derived
                      :initform      :no-init
                      :function      'node-list-pointer-for-node))
  
  (:metaclass mmf:persistent-metalevel-class)
  (:c-type tree-node_t))


;; -------------------------------------------

(defun make-file-node (name mapper &optional position)
  (make-instance 'file-node
                 :name      name
                 :mapper    mapper
                 :address   position))

;; -------------------------------------------

(defun node-list-pointer-for-node (node)
  (mmf:cast node 'off_t
            :rel-address (align-8 (mmf:foreign-slot-offset
                                   'tree-node_t 'node-list))))

;; -------------------------------------------

(defmethod btree:node-list-cell ((tn file-node) index)
  (make-file-node (file-node-name tn) tn
                  (mmf:fetch (node-list-pointer tn)
                             :index index)))

(defmethod (setf btree:node-list-cell) (val (tn file-node) index)
  (mmf:store (mmf:pointer-address val) (node-list-pointer tn) :index index))

(defmethod (setf btree:node-list-cell) ((val integer) (tn file-node) index)
  (mmf:store val (node-list-pointer tn) :index index))

;; -----------------------------

(defmethod btree:copy-node-list-cells ((to file-node) to-index
                                       (from file-node) from-index
                                       ncells)
  (mmf:copy-region (node-list-pointer to)   to-index
                   (node-list-pointer from) from-index
                   ncells))

;; ------------------------------------------------------------

(defconstant +sys-overhead+   (mmf:foreign-slot-offset 'tree-node_t 'height))

(defconstant +max-node-slots+
  (let ((nslots (- (truncate (- +page-size+
                                (align-8 (mmf:foreign-slot-offset 'tree-node_t 'node-list)))
                             (mmf:size-of 'off_t))
                   2))) ;; room for overflow
    (if (evenp nslots)
        (1- nslots)
      nslots)))

;; -------------------------------------------

(defmethod btree:node-capacity ((node file-node))
  +max-node-slots+)

;; ------------------------------------------------------------
;; ------------------------------------------------------------

(fli:define-c-struct btree_t
  (:byte-packing 1)
  (root   off_t)
  (count  uint64))

(defclass file-btree (btree:btree)
  ((name       :reader        file-btree-name
               :initarg       :name)
   
   (root       :accessor      file-btree-root
               :allocation    :persistent
               :initform      :no-init
               :c-access-spec 'root
               :c-type        off_t)
   
   (count      :accessor      btree:items-count
               :allocation    :persistent
               :initform      :no-init
               :c-access-spec 'count
               :c-type        uint64)

   (cache      :reader        file-btree-cache
               :initform      nil))

  (:metaclass mmf:persistent-metalevel-class)
  (:c-type btree_t)
  (:cacheable t))

;; -------------------------------------------

(defmethod btree:root-node ((btree file-btree))
  (let ((root-pos (file-btree-root btree)))
    (unless (zerop root-pos)
      (make-file-node (file-btree-name btree) btree root-pos))
    ))

(defmethod (setf btree:root-node) ((node file-node) (btree file-btree))
  (setf (file-btree-root btree) (mmf:pointer-address node)))

(defmethod (setf btree:root-node) ((node (eql nil)) (btree file-btree))
  (btree:discard-node btree (btree:root-node btree))
  (setf (file-btree-root btree) 0))

;; -------------------------------------------

(defmethod btree:make-node ((btree file-btree) height)
  (let* ((pos (- (alloc-page-aligned
                  (- +page-size+ +sys-overhead+)
                  +tree-node+)
                 +sys-overhead+))
         (pnode (make-file-node (file-btree-name btree) btree pos)))
    (setf (btree:node-height       pnode) height
          (btree:node-fill-pointer pnode) 0)
    pnode))

;; -------------------------------------------

(defmethod btree:discard-node ((btree file-btree) (node file-node))
  (discard (+ (mmf:pointer-address node)
              (mmf:size-of 'memory-block-header_t))))

;; -------------------------------------------------

(defmethod btree:coerce-to-object ((btree file-btree) obj)
  ;; convert to the primary mapper
  (db-ptr obj))

;; -------------------------------------------------

(defmethod btree:get-cache ((btree file-btree) constructor-fn)
  (let ((cache-obj (file-btree-cache btree)))
    (or-setf (table-cache-cache cache-obj)
             (when constructor-fn
               (funcall constructor-fn))) ))

;; -------------------------------------------------

(defmethod btree:btree-lock ((btree file-btree))
  (table-cache-lock (file-btree-cache btree)))
  
;; --------------------------------------
;; --------------------------------------

(defun make-file-btree (name pos &key (compare '-) (key 'identity))
  (make-instance 'file-btree
                 :name       name
                 :mapper     (db-ptr pos :type 'btree_t)
                 :compare    compare
                 :key        key))

(defmethod initialize-instance :after ((fb file-btree) &key &allow-other-keys)
  (let ((cache (allocate-table-cache fb)))
    (setf (table-cache-cache cache) nil)
    (setf (slot-value fb 'cache) cache)))

;; -------------------------------------------

(defmethod initialize-file-btree ((btree file-btree))
  (setf (file-btree-root btree)   0
        (btree:items-count btree) 0))

;; -------------------------------------------------

(defun delete-btree (ptr)
  (declare (ignore ptr))
  (error "Not yet implemented!"))

;; -------------------------------------------------
;; fast bulk-loading
;; --------------------------------------------------

(defun sort-positions (keys vpos &key (test '<) (key 'identity))
  (let* ((nel (length vpos))
         (ixs (loop for ix from 0 below nel collect ix))
         (sorted-ixs (sort ixs test :key (funcall key (curry 'aref keys)))))
    (map 'vector (curry 'aref vpos) sorted-ixs)))

(defun create-bulk-tree (btree keys vpos &key (test '<) (key 'identity))
  (btree:with-locked-btree (btree)
    (let ((ptmp (make-file-node (file-btree-name btree) btree)))
      (labels
          ((iter-outer (ht vpos)
             (let ((nel (length vpos)))
           
               ;; this routine fails if we have too many keys... FIX IT!
               (assert (< nel (* 1/2 +max-node-slots+ +max-node-slots+)))
           
               (labels
                   ((ensure-file-node (val)
                      (if (typep val 'file-node)
                          val
                        (make-file-node (file-btree-name btree) btree val)))

                    (temp-ensure-file-node (val)
                      (if (typep val 'file-node)
                          val
                        (progn
                          (setf (mmf:pointer-address ptmp) val)
                          ptmp)))
                
                    (iter (ix accum)
                      (if (>= ix nel)
                          (coerce (nreverse accum) 'vector)
                        (let* ((node  (btree:make-node btree ht))
                               (limit (min (- nel ix) (btree:node-capacity node))))
                          (if (and (>  (- nel ix) limit)
                                   (>= (+ ix limit 1) nel))
                              (decf limit)) ;; so we have at least 1 entry in the final right node
                          (dotimes (jx limit)
                            (setf (btree:node-list-cell node jx)
                                  (temp-ensure-file-node (aref vpos (+ ix jx)))))
                          (push node accum)
                          (if (< (+ ix limit) nel)
                              (push (ensure-file-node (aref vpos (+ ix limit))) accum))
                          (setf (btree:node-fill-pointer node) limit)
                          (iter (+ ix limit 1) accum))
                        )))
             
                 (let ((vnodes (iter 0 nil)))
                   (if (> (length vnodes) 1)
                       (iter-outer (1+ ht) vnodes)
                     (aref vnodes 0)))
                 ))))
    
        (setf (btree:root-node btree)
              (iter-outer 1 (sort-positions keys vpos :test test :key key))
              (btree:items-count btree) (length keys))
        ))))
