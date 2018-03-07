;; heaps.lisp -- Heap memory manangement for objects of fixed size
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

(fli:define-c-struct heap_t
  (:byte-packing 1)
  (current-page    off_t)   ;; 8
  (free-list       off_t)   ;; 8
  (count           uint64)  ;; 8
  (allocation-size size_t)  ;; 4
  (object-size     size_t)  ;; 4
  (fill-pointer    size_t)) ;; 4, total = 36

(defclass heap ()
  ((current-page    :accessor      heap-current-page
                    :allocation    :persistent
                    :c-access-spec 'current-page
                    :c-type        off_t)

   (free-list       :accessor      heap-free-list
                    :allocation    :persistent
                    :c-access-spec 'free-list
                    :c-type        off_t)

   (count           :accessor      heap-count
                    :allocation    :persistent
                    :c-access-spec 'count
                    :c-type        uint64)
   
   (allocation-size :accessor      heap-allocation-size
                    :allocation    :persistent
                    :c-access-spec 'allocation-size
                    :c-type        size_t
                    :constraint    'mmf:i>0)

   (object-size     :accessor      heap-object-size
                    :allocation    :persistent
                    :c-access-spec 'object-size
                    :c-type        size_t
                    :constraint    'mmf:i>0)

   (fill-pointer    :accessor      heap-fill-pointer
                    :allocation    :persistent
                    :c-access-spec 'fill-pointer
                    :c-type        size_t)

   (name            :reader        heap-name
                    :initarg       :name)
   )

  (:metaclass mmf:persistent-metalevel-class)
  (:c-type heap_t)
  (:cacheable t))

;; -------------------------------------------------------

(defun make-heap (name position)
  (make-instance 'heap
                 :name   name
                 :mapper (db-ptr position :type 'heap_t)))

;; -------------------------------------------------------
;; leave room for a prev-pointer at offset 0
(defconstant +first-heap-data-position+ (mmf:size-of 'off_t))
;; -------------------------------------------------------

(defun effective-heap-ptr (name ptr)
  (or ptr
      (make-heap name (alloc (mmf:size-of 'heap_t)
                             +fixed-heap-header+))
      ))

(defun create-heap (name object-size alloc-size &optional heap-ptr)
  (bind*
      ((p (effective-heap-ptr name heap-ptr))
       (objsize (align-8 object-size))
       (:struct-accessors heap
        (current-page
         free-list
         fill-pointer
         object-size
         allocation-size
         count) p))

    (setf current-page    0
          free-list       0
          fill-pointer    0
          count           0
          object-size     objsize
          allocation-size (- (align-page (+ +first-heap-data-position+
                                            (* objsize alloc-size)))
                             (mmf:size-of 'memory-block-header_t)))
    (values (mmf:pointer-address p) p)))

;; -------------------------------------------------------

(defun delete-heap (ptr)
  (let ((ptr (db-ptr ptr :type 'heap_t)))
    (declare (ignore ptr))
    (error "Not yet implemented")))

;; -------------------------------------------------------

(defmethod new-object ((heap-ptr heap))
  (bind*
      ((:struct-accessors heap
        (current-page
         free-list
         allocation-size
         object-size
         fill-pointer
         count) heap-ptr))

    (cond ((zerop free-list)
           (when (or (= 0 current-page)
                     (>= (+ fill-pointer object-size) allocation-size))
             (let* ((new-page (alloc-page-aligned
                               allocation-size
                               +fixed-heap-block+)))
               (store-position-at heap-ptr new-page current-page)
               (setf current-page new-page
                     fill-pointer +first-heap-data-position+)))
           (prog1
               (+ current-page fill-pointer)
             (incf fill-pointer object-size)
             (incf count)))

          ;; ----------------------------
          
          (t
           (prog1
               free-list
             (setf free-list (fetch-position-at heap-ptr free-list))
             (incf count)))
          )))

;; -------------------------------------------------------

(defmethod free-object ((heap-ptr heap) (p mmf:mmptr))
  (free-object heap-ptr (mmf:pointer-address p)))

(defmethod free-object ((heap-ptr heap) (p mmf:persistent-root-class))
  (free-object heap-ptr (mmf:pointer-address p)))

(defmethod free-object ((heap-ptr heap) position)
  (bind*
      ((:struct-accessors heap
        (free-list
         count) heap-ptr))
    (validate-position heap-ptr position)
    (store-position-at heap-ptr position free-list)
    (setf free-list position)
    (decf count)))

;; -------------------------------------------------------

(defun fetch-position-at (mapper position)
  (mmf:fetch (mmf:cast mapper 'off_t :address position)))

;; -------------------------------------------------------

(defun store-position-at (mapper position new-position)
  (mmf:store new-position (mmf:cast mapper 'off_t :address position)))

;; -------------------------------------------------------

(defun validate-position (heap-ptr position)
  (bind*
      ((:struct-accessors heap
        (current-page
         allocation-size
         object-size) heap-ptr))
    (do ((base current-page (fetch-position-at heap-ptr base)))
        ((= 0 base))
      (when (and (<= (+ base +first-heap-data-position+) position)
                 (< position (+ base allocation-size))
                 (= 0 (rem (- position base +first-heap-data-position+) object-size)))
        (ensure-not-on-freelist heap-ptr position)
        (return-from validate-position)))
    (error "Invalid object position for heap")))

(defun ensure-not-on-freelist (heap-ptr position)
  (bind*
      ((:struct-accessors heap
        (free-list) heap-ptr))
    (do ((pos free-list (fetch-position-at heap-ptr pos)))
        ((= 0 pos))
      (if (= pos position)
          (error "Object already on free-list")))
    ))

;; --------------------------------------------------

(defmethod heap-bulk-load ((heap-ptr heap) generator-fn)
  (bind*
      ((:struct-accessors heap
        (current-page
         free-list
         allocation-size
         object-size
         fill-pointer
         count) heap-ptr)
       (imm-count         count)
       (imm-current-page  current-page)
       (imm-free-list     free-list)
       (imm-fill-pointer  fill-pointer)
       (k-allocation-size allocation-size)
       (k-object-size     object-size))

    (labels ((fast-load ()
               (cond ((= 0 imm-free-list)
                      (when (or (= 0 imm-current-page)
                                (>= (+ imm-fill-pointer k-object-size) k-allocation-size))
                        (let ((new-page (alloc-page-aligned
                                         k-allocation-size
                                         +fixed-heap-block+)))
                          (store-position-at heap-ptr new-page imm-current-page)
                          (setf imm-current-page new-page
                                imm-fill-pointer +first-heap-data-position+)))
                      (prog1
                          (+ imm-current-page imm-fill-pointer)
                        (incf imm-fill-pointer k-object-size)
                        (incf imm-count)))
                     
                     (t
                      (prog1
                          imm-free-list
                        (setf imm-free-list (fetch-position-at heap-ptr imm-free-list))
                        (incf imm-count)))
                     )))
      
      (funcall generator-fn #'fast-load)
      (setf free-list    imm-free-list
            current-page imm-current-page
            fill-pointer imm-fill-pointer
            count        imm-count))
    ))
