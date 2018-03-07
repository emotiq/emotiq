;; string-pool.lisp -- storage for strings of variable length
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

(fli:define-c-struct string-pool_t
  (:byte-packing 1)
  (btree         btree_t )  ;; 16
  (heap          v8heap_t)) ;; 10, total = 26

(defclass string-pool ()
  ((count     :accessor      string-pool-count
              :allocation    :persistent
              :c-access-spec '(btree count)
              :c-type        uint64)

   (btree     :reader        string-pool-btree
              :allocation    :derived
              :function      'make-string-pool-btree)
   
   (heap      :reader        heap-pointer
              :allocation    :derived
              :function      'make-string-pool-varheap)

   (name      :reader        string-pool-name
              :initarg       :name))

  (:metaclass mmf:persistent-metalevel-class)
  (:c-type string-pool_t)
  (:cacheable t))

;; ---------------------------------------------------

(defun make-string-pool (name position)
  (make-instance 'string-pool
                 :name   name
                 :mapper (db-ptr position :type 'string-pool_t)))

;; ---------------------------------------------------

(defun make-string-pool-btree (pool-ptr)
  (make-file-btree (string-pool-name pool-ptr)
                   (mmf:foreign-slot-pointer pool-ptr 'btree)
                   :compare 'string-compare
                   :key     'fetch-string))

;; ---------------------------------------------------

(defun make-string-pool-varheap (pool-ptr)
  (make-v8heap (string-pool-name pool-ptr)
               (mmf:foreign-slot-pointer pool-ptr 'heap)))

;; ---------------------------------------------------

(defun string-compare (str1 str2)
  (cond ((string< str1 str2) -1)
        ((string< str2 str1)  1)
        (t                    0)
        ))

;; ---------------------------------------------------

(defun fetch-null-string ()
  "")

(defun fetch-short-string (pstr len)
  (mmf:fetch-ascii-string (mmf:cast pstr `(:c-array uint8 ,len)
                                    :rel-address 1)
                          :length len))

(defun fetch-long-string (pstr)
  (loenc:decode pstr
                :reader #'(lambda (ptr index)
                            (mmf:fetch ptr :index index))
                :start 1))

(defun fetch-non-null-string (pstr len)
  (cond ((= 0 len) (fetch-long-string pstr))
        (t         (fetch-short-string pstr len))
        ))
    
;; ---------------------------------------------------

(defun fetch-string (ptr &optional pos)
  (let ((pstr (mmf:cast ptr 'uint8 :address pos)))
    (cond ((zerop (mmf:pointer-address pstr))
           (fetch-null-string))
          
          (t
           (fetch-non-null-string pstr (mmf:fetch pstr)))
          )))

;; ---------------------------------------------------

(defun fetch-string-indirect (ptr &optional pos)
  ;; convention is that all main-btree objects have a string pointer
  ;; in their first slot. This is an off_t that points somewhere into the
  ;; string-pool.
  (fetch-string ptr (mmf:fetch (mmf:cast ptr 'off_t :address pos))))

;; ---------------------------------------------------

(defun create-new-string-pool (name &optional pool-ptr)
  (bind*
      ((p (or pool-ptr
              (make-string-pool name (alloc (mmf:size-of 'string-pool_t)
                                            +string-pool-header+)))))
    (initialize-file-btree (string-pool-btree p))
    (create-v8heap name (heap-pointer p))
    (values (mmf:pointer-address p) p)))

;; ---------------------------------------------------

(defmethod find-string ((pool-ptr string-pool) (str string))
  (btree:find-item (string-pool-btree pool-ptr) str))

;; ---------------------------------------------------

(defun add-null-string (pool-ptr)
  (mmf:cast pool-ptr 'uint8 :address 0))

#|
(defun add-short-string (pool-ptr str len)
  (let* ((pos  (new-v8object (heap-pointer pool-ptr) (1+ len)))
         (pstr (mmf:cast pool-ptr 'uint8 :address pos))
         (arr  (map 'vector 'char-code str)))
    (mmf:store len pstr)
    (mmf:store-array arr (mmf:cast pstr `(:c-array uint8 ,len)
                                   :rel-address 1))
    pstr))
|#

(defun add-short-string (pool-ptr str len)
  (let* ((pos  (new-v8object (heap-pointer pool-ptr) (1+ len)))
         (pstr (mmf:cast pool-ptr 'uint8 :address pos)))
    (mmf:store len pstr)
    (mmf:store-ascii-string str (mmf:cast pstr `(:c-array uint8 ,len)
                                          :rel-address 1)
                            :length len)
    pstr))

(defun add-long-string (pool-ptr str)
  (let* ((arr  (loenc:encode str))
         (len  (length arr))
         (pos  (alloc (1+ len) +long-string+))
         (ptr  (mmf:cast pool-ptr 'uint8 :addresss pos)))
    (mmf:store 0 ptr) ;; zero indicates long string to follow
    (mmf:store-array arr (mmf:cast ptr `(:c-array uint8 ,len)
                                   :rel-address 1))
    ptr))
  
(defmethod add-string-to-pool ((pool-ptr string-pool) (str string))
  (let ((len (length str)))
    (cond ((= len 0)   (add-null-string pool-ptr))
          ((< len 256) (add-short-string pool-ptr str len))
          (t           (add-long-string pool-ptr str))
          )))

;; ---------------------------------------------------

(defmethod insert-string ((pool-ptr string-pool) (str string))
  (or (find-string pool-ptr str)
      (let ((str-ptr (add-string-to-pool pool-ptr str)))
        (btree:insert-item (string-pool-btree pool-ptr) str str-ptr)
        str-ptr)))

;; --------------------------------------------------

(defun collect-strings (pool-ptr)
  (let* ((btree  (string-pool-btree pool-ptr))
         (key-fn (btree:key-fn btree)))
    (um:accum acc
      (btree:map-tree btree
                      (um:compose #'acc key-fn)))
    ))
    
