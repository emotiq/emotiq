;; alloc.lisp
;; --------------------------------------------------------------------------------------
;;
;; DM/RAL  08/08
;; --------------------------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2008 Refined Audiometrics Laboratory, LLC

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

;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

;; -------------------------------------------

(fli:define-c-struct memory-block-header_t
  (:byte-packing 1)
  (block-size   off_t)
  (freel        off_t) ;; backpointer along free-list
  (type-uuid    uuid_t))

(defconstant +memory-block-header-size+ (mmf:size-of 'memory-block-header_t))

;; check that memory-block-header is a power-of-2 in size
(assert (= +memory-block-header-size+
           (ceiling-pwr2 +memory-block-header-size+)))


(defclass memory-block-header ()
  ((block-size  :accessor      memory-block-header-block-size
                :allocation    :persistent
                :c-access-spec 'block-size
                :c-type        off_t)
   
   (freel       :accessor      memory-block-header-freel
                :allocation    :persistent
                :c-access-spec 'freel
                :c-type        off_t)

   (uuid-ptr    :reader        memory-block-header-uuid-pointer
                :allocation    :derived
                :function      #'(lambda (mbhdr)
                                   (mmf:foreign-slot-pointer mbhdr 'type-uuid)))
   )
  (:metaclass mmf:persistent-metalevel-class)
  (:c-type memory-block-header_t))

;; -------------------------------------------

(defun make-memory-block-header (&optional address)
  (make-instance 'memory-block-header
                 :mapper (database-mapper *current-okeanos-db*)
                 :address address))

;; -------------------------------------------

(defun mark-allocation (pos size type-uuid)
  ;; size includes the memory-block-header
  (let* ((p (make-memory-block-header pos)))
    (setf (memory-block-header-block-size p) size
          (memory-block-header-freel p)      0)
    (mmf:store type-uuid (memory-block-header-uuid-pointer p))
    (+ pos +memory-block-header-size+)))

;; -------------------------------------------
;; Difference between reserve and alloc:
;;  - reserve grabs the next consecutive region at next-avail or higher
;;  - alloc tries to fit request into an available free block or else calls reserve
;; -------------------------------------------

(defun reserve-null-region ()
  0)

;; -------------------------------------------

(defun aligned-allocation-position (pos)
  (align-pwr2 pos +memory-block-header-size+))

(defun aligned-allocation-size (size)
  (aligned-allocation-position (+ size +memory-block-header-size+)))

;; -------------------------------------------

(defun reserve-region (size type-uuid)
  (bind*
      ((:struct-accessors data-file-header
        (next-avail) (database-mapper *current-okeanos-db*))
       (pos  next-avail)
       (sz   (aligned-allocation-size size)))
    
    (assert (= pos (aligned-allocation-position pos)))
    (setf next-avail (+ sz pos))
    (mark-allocation pos sz type-uuid)))

;; -------------------------------------------
;; export reserve-next-available

;; unused
(defun reserve-next-available (size type-uuid)
  (cond ((zerop size) (reserve-null-region))
        (t            (reserve-region size type-uuid))
        ))

;; -------------------------------------------

(defun ensure-next-avail-page-aligned ()
  (bind*
      ((:struct-accessors data-file-header
        (next-avail free-list) (database-mapper *current-okeanos-db*))
       (old-pos next-avail)
       (pos     (align-page next-avail)))
    
    (unless (= pos old-pos)
      ;; create a dummy free block to soak up extra space between
      ;; current next-avail and needed aligned position
      (assert (>= pos (+ old-pos +memory-block-header-size+)))
      (let* ((aligned-pos (reserve-region
                           ;; we might actually be zero size beyond header
                           ;; - that's okay here
                           (- pos old-pos +memory-block-header-size+)
                           +free-block+))
             (free-pos    (- aligned-pos +memory-block-header-size+))
             (p           (make-memory-block-header free-pos)))
        (shiftf (memory-block-header-freel p) free-list free-pos)))
    ))
  
;; -------------------------------------------

(defun reserve-page-aligned-region (size type-uuid)
  (ensure-next-avail-page-aligned)
  (reserve-region size type-uuid))

;; -------------------------------------------
;; export reserve-next-available-page-aligned

;; unused
(defun reserve-next-available-page-aligned (size type-uuid)
  (cond ((zerop size) (reserve-null-region))
        (t            (reserve-page-aligned-region size type-uuid))
        ))

;; -------------------------------------------
;; alloc-parms -- just a way to avoid over-lengthy parameter lists

(defstruct alloc-parms
  ptr aligned-pos blksize tsize diff pos follow type)

;; -------------------------------------------

(defun split-bottom-block (parms)
  (bind*
      ((:struct-accessors alloc-parms
        (ptr aligned-pos
         (aligned-blksize blksize)
         pos diff) parms))
    
    ;; trim off the bottom, creating a lower non-aligned free block
    ;; and an upper aligned free-block, then recurse
    (assert (= diff (aligned-allocation-position diff)))
    (assert (>= diff +memory-block-header-size+))
    (setf (memory-block-header-block-size ptr) diff)
    (mark-allocation aligned-pos aligned-blksize +free-block+)
    (let ((aligned-ptr (make-memory-block-header aligned-pos)))
      (shiftf (memory-block-header-freel aligned-ptr)
              (memory-block-header-freel ptr)
              aligned-pos)
      (values nil aligned-pos pos))))
   

(defun split-top-block (parms)
  (bind*
      ((:struct-accessors alloc-parms
        (ptr aligned-pos
         (aligned-blksize blksize)
         tsize pos follow) parms))
    
    ;; trim off the top, creating a lower aligned free block
    ;; and an upper free block, then recurse
    (let* ((hidiff (- aligned-blksize tsize))
           (hipos  (+ aligned-pos tsize))
           (hiptr  (make-memory-block-header hipos)))
      (assert (>= hidiff +memory-block-header-size+))
      (assert (= hipos (aligned-allocation-position hipos)))
      (mark-allocation hipos hidiff +free-block+)
      (shiftf (memory-block-header-freel hiptr)
              (memory-block-header-freel ptr)
              hipos)
      (values nil pos follow))))


(defun split-free-block-for-page-alignment (parms)
  ;; current free block at ptr is larger than needed
  ;; split into two free blocks, one of which is our desired size and aligned to a page,
  ;; then call the iter function
  ;; diff is the space between the start of the free block and the page aligned position.
  (bind*
      ((:struct-accessors alloc-parms
        (diff) parms))
    
    (cond
     ((plusp diff) (split-bottom-block parms))
     (t            (split-top-block parms))
     )))

;; -------------------------------------------

(defun unlink-free-block (parms)
  (bind*
      ((:struct-accessors alloc-parms
        (pos follow ptr tsize
          (type-uuid type)) parms)
       (:struct-accessors data-file-header
        (free-list) (database-mapper *current-okeanos-db*)))

    (if follow
        (let ((follow-ptr (make-memory-block-header follow)))
          (setf (memory-block-header-freel follow-ptr)
                (memory-block-header-freel ptr)))
      ;; else
      (setf free-list (memory-block-header-freel ptr)))
    (mark-allocation pos tsize type-uuid)))

;; -------------------------------------------

(defun first-fit-page-aligned (parms)
  (bind*
      ((:struct-accessors alloc-parms
        (tsize pos) parms)
       (ptr             (make-memory-block-header pos))
       (blksize         (memory-block-header-block-size ptr))
       (aligned-pos     (align-page pos))
       (diff            (- aligned-pos pos))
       (aligned-blksize (- blksize diff)))
    (cond
     ((or (> aligned-blksize tsize)
          (and (plusp diff)
               (= aligned-blksize tsize)))
      (setf (alloc-parms-ptr         parms) ptr
            (alloc-parms-aligned-pos parms) aligned-pos
            (alloc-parms-blksize     parms) aligned-blksize
            (alloc-parms-diff        parms) diff)
      (split-free-block-for-page-alignment parms))
     
     ((= aligned-blksize tsize)
      (setf (alloc-parms-ptr parms) ptr)
      (unlink-free-block parms))
     
     (t ;; aligned-blksize too small
        (values nil (memory-block-header-freel ptr) pos))
     )))

;; -------------------------------------------

(defun alloc-page-aligned-region (size type-uuid)
  (bind*
      ((tsize (aligned-allocation-size size))
       (:struct-accessors data-file-header
        (free-list) (database-mapper *current-okeanos-db*))
       (parms (make-alloc-parms
               :tsize     tsize
               :type      type-uuid)))
    
    (nlet iter ((pos    free-list)
                (follow nil))
      (cond ((zerop pos) (reserve-page-aligned-region size type-uuid))
            (t           (progn
                           (setf (alloc-parms-pos    parms) pos
                                 (alloc-parms-follow parms) follow)
                           (multiple-value-bind (ans new-pos new-follow)
                               (first-fit-page-aligned parms)
                             (or ans
                                 (iter new-pos new-follow))
                             )))
            ))))

;; -------------------------------------------

(defmacro with-locked-file (&body body)
  `(do-with-locked-file (deferred ,@body)))

(defun do-with-locked-file (fn)
  (mmf:with-locked-mmf ((mmf:mapped-file-of-mmptr (database-mapper *current-okeanos-db*)))
    (funcall fn)))

#+:LISPWORKS
(editor:setup-indent "with-locked-file" 1)

;; -------------------------------------------
;; export alloc-page-aligned

;; user API
(defun alloc-page-aligned (size type-uuid)
  (with-locked-file
    (cond ((zerop size) (reserve-null-region))
          (t            (alloc-page-aligned-region size type-uuid))
          )))
  
;; -------------------------------------------

(defun allocate-top-region (parms)
  (bind*
      ((:struct-accessors alloc-parms
        (pos ptr blksize tsize
         (type-uuid type)) parms))
    
    ;; allocate us in the top-end
    (let* ((diff  (- blksize tsize))
           (hipos (+ pos diff)))
      (assert (>= diff +memory-block-header-size+))
      (assert (= hipos (aligned-allocation-position hipos)))
      (setf (memory-block-header-block-size ptr) diff)
      (mark-allocation hipos tsize type-uuid))))

;; -------------------------------------------

(defun first-fit (parms)
  (bind*
      ((:struct-accessors alloc-parms
        (tsize pos) parms)
       (ptr     (make-memory-block-header pos))
       (blksize (memory-block-header-block-size ptr)))
    
    (cond
     ((> blksize tsize)
      (setf (alloc-parms-ptr     parms) ptr
            (alloc-parms-blksize parms) blksize)
      (allocate-top-region parms))
     
     ((= blksize tsize)
      (setf (alloc-parms-ptr parms) ptr)
      (unlink-free-block parms))
     
     (t ;; blksize too small
        (values nil (memory-block-header-freel ptr) pos))
     )))
  
;; -------------------------------------------

(defun alloc-region (size type-uuid)
  (bind*
      ((tsize (aligned-allocation-size size))
       (:struct-accessors data-file-header
        (free-list) (database-mapper *current-okeanos-db*))
       (parms (make-alloc-parms
               :tsize     tsize
               :type      type-uuid)))
    
    (nlet iter ((pos    free-list)
                (follow nil))
      (cond ((zerop pos) (reserve-region size type-uuid))
            (t           (progn
                           (setf (alloc-parms-pos    parms) pos
                                 (alloc-parms-follow parms) follow)
                           (multiple-value-bind (ans new-pos new-follow)
                               (first-fit parms)
                             (or ans
                                 (iter new-pos new-follow))
                             )))
            ))))

;; -------------------------------------------
;; export alloc

;; user API
(defun alloc (size type-uuid)
  (with-locked-file
    (cond ((zerop size) (reserve-null-region))
          (t            (alloc-region size type-uuid))
          )))

;; ---------------------------------------------------

(defun discard-region (pos)
  (with-locked-file
    (bind*
        ((:struct-accessors data-file-header
          (free-list) (database-mapper *current-okeanos-db*))
         (blk-base (- pos +memory-block-header-size+))
         (p        (make-memory-block-header blk-base)))
      
      (mark-allocation blk-base (memory-block-header-block-size p) +free-block+)
      (shiftf (memory-block-header-freel p) free-list blk-base))))

;; -------------------------------------------
;; export discard

;; user API
(defmethod discard ((pos integer))
  (unless (zerop pos)
    (discard-region pos)))

(defmethod discard ((ptr mmf:mmptr))
  (discard (mmf:pointer-address ptr)))


;; ---------------------------------------------------------------

;; user API
(defun allocation-type (ptr)
  (bind*
      ((hdr (make-memory-block-header (- (mmf:pointer-address ptr)
                                         +memory-block-header-size+)))
       (uuid (mmf:fetch (memory-block-header-uuid-pointer hdr))))
    (or
     (rest
      (assoc uuid `((,+free-block+            . :free-block)
                    (,+tree-header+           . :tree-header)
                    (,+tree-node+             . :tree-node)
                    (,+fixed-heap-header+     . :fixed-heap-header)
                    (,+fixed-heap-block+      . :fixed-heap-block)
                    (,+small-var-heap-header+ . :small-var-heap-header)
                    (,+small-var-heap-block+  . :small-var-heap-block)
                    (,+string-pool-header+    . :string-pool-header)
                    ;; (,+file-uuid+             . :file-uuid)
                    (,+long-string+           . :long-string)
                    ;; (,+schema-entry+          . :schema-entry)
                    (,+file-table+            . :file-table)
                    (,+data-item-uuid+        . :data-item))
             :test 'uuid:uuid=))
     :unknown)))

;; ---------------------------------------------------------------

;; user API
(defun allocation-size (ptr)
  ;; allocation-size includes the size of the block header itself
  (bind*
      ((hdr (make-memory-block-header (- (mmf:pointer-address ptr)
                                         +memory-block-header-size+))))
    (memory-block-header-block-size hdr)))

       
