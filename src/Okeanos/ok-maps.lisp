;; ok-maps.lisp -- Persistent Maps
;; --------------------------------------------------------------------------------------
;; OkeanosMM -- memory mapped database system
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  03/09
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

;; -------------------------------------------

(fli:define-c-struct ok-map-entry
  (:byte-packing 1)
  (key   off_t)
  (oid   oid_t))

(defvar *ok-map-schema*
  (um:lazy (make-schema
            :ok-map-schema
            '((:key           :encoded-value
               :c-type        off_t
               :c-access-spec key
               :indexed       :unique)
              
              (:val           :oid
               :c-type        oid_t
               :c-access-spec oid))
            
            :c-type 'ok-map-entry)))

(defun ensure-ok-map-schema ()
  (um:force *ok-map-schema*))

;; -------------------------------------------

(defclass ok-map (ok-map-in-memory persistent-kernel-object)
  ;; the persistent portion
  ((name      :reader     ok-map-name
              :allocation :persistent
              :indexed    :unique
              :initarg    :name
              :initform   (mkstr (make-uuid)))

   (table-pos :reader     ok-map-table-pos
              :allocation :persistent)

   (heap-pos  :reader     ok-map-heap-pos
              :allocation :persistent)

   (btrees    :reader     ok-map-btrees
              :allocation :persistent))
  
  (:metaclass persistent-kernel-class)
  (:oid       #/oid/{00000000-0000-0000-0000-000000000002}))


(defclass ok-map-in-memory ()
  ((table     :reader     ok-map-table
              :initform   nil)
   
   (additions :accessor   ok-map-additions
              :initform   (make-hash-table :test 'equal))
   
   (deletions :accessor   ok-map-deletions
              :initform   (make-hash-table :test 'equal))
   
   (map-lock  :reader     ok-map-lock
              :initform   (mpcompat:make-lock :name "OK-Map"))
   ))

(defmethod initialize-instance :after ((obj ok-map) &rest args
                                       &key &allow-other-keys)
  (when-remote
    ;; need to give the server a chance to tell us that the object's OID
    ;; needs to be changed to match an older existing object, when we
    ;; have the same primary index key value.
    (apply 'ask-server-to-instantiate-copy obj args)))
  
;; -----------------------------------------------------

(defmacro with-map-lock ((map) &body body)
  `(mpcompat:with-lock ((ok-map-lock ,map))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-map-lock" 1)

;; -----------------------------------------------------

(defmethod after-retrieve :after ((obj ok-map))
  (unless-remote
    (setf (slot-value obj 'table) (make-file-table
                                   :name      (ok-map-name obj)
                                   :schema    (ensure-ok-map-schema)
                                   :table-pos (ok-map-table-pos obj)
                                   :heap-pos  (ok-map-heap-pos obj)
                                   :btrees    (ok-map-btrees obj)))))

;; -------------------------------------------
;; -------------------------------------------
;; In these commit routines, we have a write-lock from above
;; so only one thread is running any of them

(defun allocate-ok-map-table (obj)
  (let ((ft (create-new-unnamed-file-table (ok-map-name obj)
                                           (ensure-ok-map-schema)
                                           :use-heap 1000)))
    (setf-slots obj
                'table      ft
                'heap-pos   (file-table-heap-pos ft)
                'btrees     (map 'list #'(lambda (btree)
                                         (mmf:pointer-address btree))
                                 (file-table-btrees ft))
                'table-pos  (mmf:pointer-address ft)) ))

#|
(defmethod commit-object-deletion :after ((obj ok-map) item)
  (declare (ignore item))
  (when-let (tbl (ok-map-table obj))
    (delete-file-table tbl)))
|#

(defconstant +log-map-add+    (char-code #\M))
(defconstant +log-map-delete+ (char-code #\N))

(defmethod commit-object-changes ((obj ok-map) item)
  (unless (ok-map-table obj)
    (allocate-ok-map-table obj))
  
  (let ((tbl (ok-map-table obj))
        (oid (persistent-item-oid item)))
    
    (if (find :add/remove (persistent-item-dirty item) :key 'first)
        (update-wts oid)
      (call-next-method))
    
    (map-keys-values #'(lambda (k v)
                       (delete-row `(:key ,(get-encoded-string-value v)) tbl)
                       (wr-ok-log +log-map-delete+ oid k))
                     (ok-map-deletions obj))
    (clear-keys-values (ok-map-deletions obj))
    
    (map-keys-values #'(lambda (k v)
                       (insert-row `(:key ,(get-encoded-string-value (car v)) :val ,(cdr v)) tbl)
                       (wr-ok-log +log-map-add+ oid (cons k (cdr v))))
                     (ok-map-additions obj))
    (clear-keys-values (ok-map-additions obj))
    ))

  ;; ----------------------------------------------------------------

(defmethod get-map ((obj ok-map) key)
  (with-read-lock ()
    (with-map-lock (obj)
      (let* ((k      (make-ok-set-key key))
             (keystr (encoded-string-s k)))
        (unless (get-key-value keystr (ok-map-deletions obj))
          (let* ((not-there #())
                 (val (get-key-value keystr (ok-map-additions obj) not-there)))
            (if (eq not-there val)
                (lookup-in-map-tree obj key)
              (values (cdr val) t)))
          )))))

(defun lookup-in-map-tree (obj key)
  (if-remote
   (rpc `(lookup-in-map-tree-oid ,(oid-for-object obj) ,key))
   ;; else
   (when-let (tbl (ok-map-table obj))
     (when-let (row (fetch-row `(:key ,key) tbl))
       (values (getf row :val) t))
     )))

(defmethod lookup-in-map-tree-oid ((oid oid) key)
  (lookup-in-map-tree (get-persistent-object oid) key))

;; -------------------------------------------

(defmethod (setf get-map) (val (obj ok-map) key)
  (with-read-lock ()
    (with-map-lock (obj)
      (let* ((k      (make-ok-set-key key))
             (keystr (encoded-string-s k)))
        (remove-key-value keystr (ok-map-deletions obj))
      (setf (get-key-value keystr (ok-map-additions obj)) (cons k (ref (persist val))))
      (mark-me-add/remove obj)
      val))))

;; -------------------------------------------

(defmethod unmap ((obj ok-map) key)
  (with-read-lock ()
    (with-map-lock (obj)
      (let* ((k      (make-ok-set-key key))
             (keystr (encoded-string-s k)))
        (remove-key-value keystr (ok-map-additions obj))
        (setf (get-key-value keystr (ok-map-deletions obj)) k)
        (mark-me-add/remove obj)
        key))))
  
;; -------------------------------------------

(defmethod map-map ((obj ok-map) fn
                   &key from to (direction :forward) max-records)
  (with-read-lock ()
    (with-map-lock (obj)
      (let ((deletions (ok-map-deletions obj)))
        (if-remote
            (dolist (pair (rpc `(collect-map-map-oid ,(oid-for-object obj)
                                                     ,from ,to ,direction ,max-records)))
              (destructuring-bind (k . v) pair
                (unless (get-key-value (encoded-string-s k) deletions)
                  (let ((key (get-encoded-string-value k)))
                    (funcall fn key v)))))
          ;; else
          (when-let (tbl (ok-map-table obj))
            ;; map existing table sans deletions
            (map-rows tbl
                      #'(lambda (row)
                        (let ((k (getf row :key)))
                          (unless (get-key-value (encoded-string-s k) deletions)
                            (funcall fn (get-encoded-string-value k) (getf row :val))
                            )))
                      :from from
                      :to to
                      :direction direction
                      :max-records max-records))))
      ;; next do pending additions
      (map-keys-values #'(lambda (k v)
                         (declare (ignore k))
                         (funcall fn (get-encoded-string-value (car v)) (cdr v)))
                       (ok-map-additions obj)) )))

(defmethod collect-map-map-oid ((oid oid) from to direction max-records)
  (let ((obj (get-persistent-object oid)))
    (um:accum acc
      (when-let (tbl (ok-map-table obj))
        (map-rows tbl
                  #'(lambda (row)
                      (acc (cons (getf row :key)
                                 (getf row :val))))
                  :from from
                  :to   to
                  :direction direction
                  :max-records max-records))
      )))

;; -------------------------------------------

(defmethod map-count ((obj ok-map))
  ;; should sync (rollback) to get accurate count
  (with-read-lock ()
    (with-map-lock (obj)
      (+ (if-remote
          (rpc `(map-count-oid ,(oid-for-object obj)))
          (if-let (tbl (ok-map-table obj))
              (file-table-count tbl)
            0))
         (- (key-value-count (ok-map-additions obj))
            (key-value-count (ok-map-deletions obj))) )) ))

(defmethod map-count-oid ((oid oid))
  (map-count (get-persistent-object oid)))

;; --------------------------------------------

(defmethod first-key ((obj ok-map))
  (if-remote
      (rpc `(first-map-key-oid ,(oid-for-object obj)))
    ;; else
    (with-read-lock ()
      (with-map-lock (obj)
        (when-let (row (fetch-first-row (ok-map-table obj)))
          (values (get-encoded-string-value (getf row :key)) t) )))))

(defmethod first-map-key-oid ((oid oid))
  (first-key (get-persistent-object oid)))

(defmethod last-key ((obj ok-map))
  (if-remote
   (rpc `(last-map-key-oid ,(oid-for-object obj)))
   ;; else
   (with-read-lock ()
     (with-map-lock (obj)
       (when-let (row (fetch-last-row (ok-map-table obj)))
         (values (get-encoded-string-value (getf row :key)) t) )))))

(defmethod last-map-key-oid ((oid oid))
  (last-key (get-persistent-object oid)))

;; --------------------------------------------

(defun find-ok-map (name)
  ;; because we are indexed...
  (find-indexed-object name 'ok-map 'ok-map-name))
  
(defmethod query ((map ok-map) selection &optional pred)
  (perform 'query
           (if-remote
            map
            (ok-map-table map))
           selection
           pred))

;; --------------------------------------------

(defmethod print-object ((obj ok-map) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "Name: ~A" (ok-map-name obj)) ))

;; --------------------------------------------

(defmethod update-from-remote ((obj ok-map) adds deletes)
  (let ((add-tbl (ok-map-additions obj))
        (del-tbl (ok-map-deletions obj)))
    (clear-keys-values add-tbl)
    (clear-keys-values del-tbl)
    (dolist (add adds)
      (let ((key (car add)))
        (setf (get-key-value (encoded-string-s key) add-tbl) add))) ;; val is an oid at this point
  (dolist (key deletes)
    (setf (get-key-value (encoded-string-s key) del-tbl) key))
  (when (or adds deletes)
    (mark-me-add/remove obj)) ))

(defmethod get-additions ((obj ok-map))
  (let ((pairs nil))
    (map-keys-values #'(lambda (k v)
                       (declare (ignore k))
                       (push v pairs))
                     (ok-map-additions obj))
    pairs))

(defmethod get-deletions ((obj ok-map))
  (let ((keys nil))
    (map-keys-values #'(lambda (k v)
                       (declare (ignore k))
                       (push v keys))
                     (ok-map-deletions obj))
    keys))

  
