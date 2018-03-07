;; ok-tables.lisp -- Persistent Tables
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

(defclass ok-table (persistent-kernel-object)
  ((name        :reader     ok-table-name
                :allocation :persistent
                :indexed    :unique
                :initarg    :name
                :initform   (mkstr (make-uuid)))

   (schema      :reader     table-schema
                :initarg    :schema
                :allocation :persistent)

   (heap-pos    :reader     ok-table-heap-pos
                :allocation :persistent
                :initform   nil)

   (btrees      :reader     ok-table-btrees
                :allocation :persistent)

   (table-pos   :reader     ok-table-table-pos
                :allocation :persistent
                :initform   nil)

   ;; --- non-persistent slots ---
   (table       :reader     ok-table-table
                :allocation :instance
                :initform   nil)

   (sequence    :accessor   table-sequence
                :allocation :instance
                :initform   0)

   (use-heap    :reader     ok-table-use-heap
                :allocation :instance
                :initarg    :use-heap
                :initform   1000)
  
   (pending     :reader     ok-table-pending
                :allocation :instance
                :initform   (priq:make-unsafe-fifo))
   
   (table-lock  :reader     ok-table-lock
                :allocation :instance
                :initform   (mpcompat:make-lock :name "OK-Table")))
  
  (:metaclass persistent-kernel-class)
  (:oid       #/oid/{00000000-0000-0000-0000-000000000004}))

;; -----------------------------------------------------

(defmacro with-oktable-lock ((map) &body body)
  `(mpcompat:with-lock ((ok-table-lock ,map))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-oktable-lock" 1)

;; -----------------------------------------------------

(defclass ok-schema (persistent-kernel-object)
  ((name            :reader     ok-schema-name
                    :initarg    :name
                    :allocation :persistent
                    :indexed    :unique
                    :initform   (mkstr (make-uuid)))
   
   (schema          :reader     ok-schema-schema
                    :initarg    :schema
                    :allocation :persistent))

  (:metaclass persistent-kernel-class)
  (:oid       #/oid/{00000000-0000-0000-0000-000000000003}))
   
(defun find-ok-schema (name)
  (find-indexed-object name 'ok-schema 'ok-schema-name))

(defmethod initialize-instance :after ((obj ok-schema)
                                       &key name ischema cols c-type &allow-other-keys)
  (let ((isch (or ischema
                  (ref (persist (make-schema name cols
                                             :c-type c-type))))))
    (setf (slot-value obj 'schema) isch)
    (when-remote
      ;; need to give the server a chance to tell us that the object's OID
      ;; needs to be changed to match an older existing object, when we
      ;; have the same primary index key value.
      (ask-server-to-instantiate-copy obj :name name :schema isch))))

#|
(defmethod ask-server-to-instantiate-copy :around ((obj ok-schema) &rest args
                                                   &key &allow-other-keys)
  (let ((sch-item (persistent-item-for-oid (basic-slot-value obj 'schema))))
    (rpc `(copy-item-from-client ,sch-item))
    (call-next-method)))

(defun copy-item-from-client (item)
  (intern-persistent-item item)
  :OK)
|#

;; -------------------------------------------

(defmethod initialize-instance :after ((obj ok-table) &key name schema ischema &allow-other-keys)
  (let ((isch (or ischema
                  (basic-slot-value (deref schema) 'schema))))
    (setf (slot-value obj 'schema) isch)
    (when-remote
      ;; need to give the server a chance to tell us that the object's OID
      ;; needs to be changed to match an older existing object, when we
      ;; have the same primary index key value.
      (ask-server-to-instantiate-copy obj :name name :ischema isch))))

(defmethod after-retrieve :after ((obj ok-table))
  (unless-remote
    (let ((tbl (make-file-table
                :name      (ok-table-name obj)
                :schema    (table-schema obj)
                :table-pos (ok-table-table-pos obj)
                :heap-pos  (ok-table-heap-pos obj)
                :btrees    (ok-table-btrees obj))))
      (setf-slots obj
                  'table    tbl
                  'sequence (table-sequence tbl)) )))

;; -------------------------------------------
;; In these commit routines, we have a write-lock from above
;; so only one thread is running any of them

#|
(defmethod commit-object-deletion :after ((obj ok-table) item)
  (declare (ignore item))
  (when-let (tbl (ok-table-table obj))
    (delete-file-table tbl)))
|#

(defconstant +log-table-hdrs+   (char-code #\U))
(defconstant +log-table-insert+ (char-code #\V))
(defconstant +log-table-delete+ (char-code #\W))
(defconstant +log-table-bulk+   (char-code #\Z))

(defun collect-col-values (cols row)
  (loop for col in cols collect
        (getf row col)))

(defmethod allocate-ok-table-table ((tbl ok-table))
  (let ((ft (create-new-unnamed-file-table
             (ok-table-name tbl)
             (table-schema tbl)
             :use-heap (ok-table-use-heap tbl))))
    (setf-slots tbl
                'table      ft
                'heap-pos   (file-table-heap-pos ft)
                'btrees     (map 'list #'(lambda (btree)
                                         (mmf:pointer-address btree))
                                 (file-table-btrees ft))
                'table-pos  (mmf:pointer-address ft))
    (setf (table-sequence ft) (table-sequence tbl)) ))


(defmethod commit-object-changes ((obj ok-table) item)
  (unless (ok-table-table obj)
    (allocate-ok-table-table obj))
  
  (let* ((tbl     (ok-table-table obj))
         (oid     (persistent-item-oid item))
         (pending (ok-table-pending obj))
         (schema  (table-schema obj))
         (cols    (get-column-names schema))
         (ucol    (get-key-column-name schema)))
    
    (if (find :add/remove (persistent-item-dirty item) :key 'first)
        (update-wts oid)
      (call-next-method))
    
    (when-let (lst (priq:contents pending)) ;; this also clears the queue
      (wr-ok-log +log-table-hdrs+ oid (list ucol cols))
      (dolist (entry lst)
        (optima:ematch entry
          ((list* :delete row)
           (delete-precertified-row row tbl)
           (wr-ok-log +log-table-delete+ oid (getf row ucol)))
          
          ((list* :insert row)
           (insert-precertified-row row tbl)
           (wr-ok-log +log-table-insert+ oid (collect-col-values cols row)))
          
          ((list* :bulk-load data)
           (perform-bulk-loading tbl data)
           (wr-ok-log +log-table-bulk+ oid data))
          
          ))
      (setf (table-sequence tbl) (table-sequence obj)) )
    ))

;; ----------------------------------------------------------------
;; User API

(defmethod insert-row (names-and-values (table ok-table))
  (with-read-lock ()
    (with-oktable-lock (table)
      (let ((row (pre-certify-row names-and-values table :insert)))
        (priq:addq (ok-table-pending table) `(:insert . ,row))
        (mark-me-add/remove table)
        row))))

(defmethod delete-row (names-and-values (table ok-table))
  (with-read-lock ()
    (with-oktable-lock (table)
      (let ((row (pre-certify-row names-and-values table)))
        (priq:addq (ok-table-pending table) `(:delete . ,row))
        (mark-me-add/remove table)
        row))))

(defmethod fetch-row (names-and-values (table ok-table))
  (with-read-lock ()
    (with-oktable-lock (table)
      (let* ((row     (pre-certify-row names-and-values table))
             (pending (ok-table-pending table))
             (keyname (get-key-column-name table))
             (test    (get-key-column-test table))
             (key     (getf row keyname)))
        (if-let (ans (priq:findq key pending
                                 :key #'(lambda (prow)
                                          (getf (cdr prow) keyname))
                                 :test test
                                 :from-end t))
            (optima:ematch ans
              ( (list* :delete _)   nil)
              ( (list* :insert row) row))
          
          ;; else
          (if-remote
           (rpc `(fetch-precertified-row-oid ,row ,(oid-for-object table)))
           ;; else
           (fetch-precertified-row row (ok-table-table table)))
          )))))

(defmethod fetch-precertified-row-oid (row (oid oid))
  (fetch-precertified-row row (ok-table-table (get-persistent-object oid))))


(defmethod fetch-first-row ((table ok-table))
  ;; WARNING: does not look into pending queue
  (with-read-lock ()
    (with-oktable-lock (table)
      (if-remote
       (rpc `(fetch-first-table-row-oid ,(oid-for-object table)))
       ;; else
       (when-let (tbl (ok-table-table table))
         (fetch-first-row tbl))))))

(defmethod fetch-first-table-row-oid ((oid oid))
  (fetch-first-row (get-persistent-object oid)))


(defmethod fetch-last-row ((table ok-table))
  ;; WARNING: does not look into pending queue
  (with-read-lock ()
    (with-oktable-lock (table)
      (if-remote
       (rpc `(fetch-last-table-row-oid ,(oid-for-object table)))
       ;; else
       (when-let (tbl (ok-table-table table))
         (fetch-last-row tbl))))))

(defmethod fetch-last-table-row-oid ((oid oid))
  (fetch-last-row (deref oid)))


(defmethod map-rows ((table ok-table) fn &rest args &key &allow-other-keys)
  ;; args are :from :to and :direction (:forward :backward)
  ;; WARNING: does not look into pending queue
  (with-read-lock ()
    (with-oktable-lock (table)
      (if-remote
          (dolist (row (rpc `(collect-map-table-rows-oid
                              ,(oid-for-object table) ,@args)))
            (funcall fn row))
        ;; else
        (when-let (tbl (ok-table-table table))
          (apply 'map-rows tbl fn args))))))

(defmethod collect-map-table-rows-oid ((oid oid) &rest args &key &allow-other-keys)
  (let (rows)
    (apply 'map-rows (deref oid)
           #'(lambda (row)
             (push row rows))
           args)
    (nreverese rows)))

(defmethod create-cursor ((table ok-table) &rest args)
  ;; args are :key :from-start :from-end
  ;; WARNING: does not look into pending queue
  (with-read-lock ()
    (with-oktable-lock (table)
      (if-remote
          (rpc `(create-cursor-oid ,(oid-for-object table) ,@args))
        ;; else
        (when-let (tbl (ok-table-table table))
          (apply 'create-cursor tbl args))))))

(defmethod create-cursor-oid ((oid oid) &rest args)
  (apply 'create-cursor (get-persistent-object oid) args))

(defmethod table-count ((table ok-table))
  (if-remote
      (rpc `(table-count-oid ,(oid-for-object table)))
    ;; else
    (if-let (tbl (ok-table-table table))
        (file-table-count tbl)
      0)))

(defmethod table-count-oid ((oid oid))
  (table-count (get-persistent-object oid)))

(defmethod get-column-names ((table ok-table))
  (get-column-names (table-schema table)))

(defmethod get-key-column-name ((table ok-table))
  (get-key-column-name (table-schema table)))

(defmethod get-key-column-test ((table ok-table))
  (get-key-column-test (table-schema table)))

;; --------------------------------------------

(defmethod fetch-all-rows ((table ok-table) &rest args &key &allow-other-keys)
  (apply 'fetch-all-rows (ok-table-table table) args))

(defmethod fetch-rows-for-column ((table ok-table) column-name value &rest args
                                  &key &allow-other-keys)
  (apply 'fetch-rows-for-column (ok-table-table table) column-name value args))

(defmethod find-rows-for-column ((table ok-table) column-name &rest args
                                  &key &allow-other-keys)
  (apply 'find-rows-for-column (ok-table-table table) column-name args))

(defmethod query ((table ok-table) selection &optional pred)
  (perform 'query (if-remote
                   table
                   (ok-table-table table))
           selection pred))

;; --------------------------------------------

#+:LISPWORKS
(defmethod view-btree ((table ok-table))
  (when-let (tbl (ok-table-table table))
    (view-btree tbl)))

(defun find-ok-table (name)
  ;; because we are indexed...
  (find-indexed-object name 'ok-table 'ok-table-name))

;; --------------------------------------------

(defmethod print-object ((obj ok-schema) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "Name: ~A" (ok-schema-name obj)) ))
  
(defmethod print-object ((obj ok-table) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "Name: ~A" (ok-table-name obj)) ))
  
;; --------------------------------------------

(defmethod bulk-load-table ((table ok-table) nrows stuffer-fn)
  ;; keys must be a vector of key values
  (unless (zerop nrows)
    (with-oktable-lock (table)
      (unless (zerop (table-sequence table))
        (error "Table must be empty for bulk-loading"))
      (let* ((schema    (table-schema table))
             (cols      (schema-column-specs schema))
             (ncols     (length cols))
             (col-names (mapcar 'column-name cols))
             (pvals     (make-array ncols))
             (data      (make-array `(,nrows ,ncols))))
        
        (unless (fixed-length-records-p schema)
          (error "Bulk loading is only supported for fixed-length records"))
        
        (loop for ix from 0 below nrows do
              (loop for col in cols
                    for jx from 0
                    for val across (progn
                                     (funcall stuffer-fn ix col-names pvals)
                                     pvals)
                    do
                    (setf (aref data ix jx) (prescrub-column-value val col))))
        
        (priq:addq (ok-table-pending table) (cons :bulk-load data))
        (mark-me-add/remove table)))))

;; ----------------------------------------------------------

(defmethod update-ok-table-from-remote ((obj ok-table) adds seqno)
  (when adds
    (dolist (add adds)
      (priq:addq (ok-table-pending obj) add))
    (mark-me-add/remove obj)
    (setf (table-sequence obj) seqno) ))

(defmethod get-pendings ((obj ok-table))
  (let ((lst (priq:contents (ok-table-pending obj))))
    (dolist (add lst)
      (priq:addq (ok-table-pending obj) add))
    lst))

