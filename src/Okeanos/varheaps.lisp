;; varheaps.lisp -- Heap memory manangement for objects of variable size
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

;; V8Heaps are for smallish objects (typically strings) that are smaller than 256 bytes.
;; Hence a one-byte leading count would be sufficient for them. No particular memory alignment
;; is offered to such objects. We don't store the prefix count. We leave that to the caller.

(fli:define-c-struct v8heap_t
  (:byte-packing 1)
  (current-page    off_t  )  ;; 8
  (fill-pointer    ssize_t)) ;; 2, total = 10

(defclass v8heap ()
  ((current-page :accessor      v8heap-current-page
                 :allocation    :persistent
                 :c-access-spec 'current-page
                 :c-type        off_t)

   (fill-pointer :accessor      v8heap-fill-pointer
                 :allocation    :persistent
                 :c-access-spec 'fill-pointer
                 :c-type        ssize_t)

   (name         :reader        v8heap-name
                 :initarg       :name))

  (:metaclass mmf:persistent-metalevel-class)
  (:c-type v8heap_t)
  (:cacheable t))

;; -------------------------------------------

(defun make-v8heap (name position)
  (make-instance 'v8heap
                 :name    name
                 :mapper  (db-ptr position :type 'v8heap_t)))
  
;; -------------------------------------------
;; leave room for a prev-pointer at offset 0
(defconstant +first-v8heap-data-position+ (mmf:size-of 'off_t))
;; -------------------------------------------

(defun effective-varheap-ptr (name ptr)
  (or ptr
      (make-v8heap name (alloc (mmf:size-of 'v8heap_t)
                               +small-var-heap-header+))))
  
(defun create-v8heap (name &optional ptr)
  (bind*
      ((p (effective-varheap-ptr name ptr))
       (:struct-accessors v8heap
        (current-page
         fill-pointer) p))

    (setf current-page    0
          fill-pointer    0)
    (values (mmf:pointer-address p) p)))

;; ----------------------------

(defun new-null-object ()
  ;; don't bother storing objects of zero size  
  0)

(defun new-short-object (heap-ptr size)
  (bind*
      ((:struct-accessors v8heap
        (current-page
         fill-pointer) heap-ptr))
    
    (when (or (= 0 current-page)
              (> (+ size fill-pointer) +page-size+))
      ;; if need more room, just ignore leftover room, and get another page
      (let* ((new-page (alloc-page-aligned
                        (- +page-size+
                           (mmf:size-of 'memory-block-header_t))
                        +small-var-heap-block+)))
        (store-position-at heap-ptr new-page current-page)
        (setf current-page new-page
              fill-pointer +first-v8heap-data-position+)))
    (prog1
        (+ current-page fill-pointer)
      (incf fill-pointer size))
    ))

(defun new-large-object ()
  (error "V8Heaps only maintiain small objects (<= 256 bytes)"))
  
(defmethod new-v8object ((heap-ptr v8heap) size)
  ;; size should be the total size, including the prefix count
  ;; hence a count-preceding string of length 255 should ask for size 256
  (cond ((<= size 0)   (new-null-object))
        ((<= size 256) (new-short-object heap-ptr size))
        (t             (new-large-object))
        ))

;; ----------------------------
          
(defmethod free-object ((heap-ptr v8heap) p)
  ;; just ignore deletions
  (declare (ignore p))
  nil)

;; --------------------------------------------------

