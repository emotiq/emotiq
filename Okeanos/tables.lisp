;; tables.lisp -- Database Tables representations
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

(fli:define-c-struct file-table_t
  (:byte-packing 1)
  (count    uint64)    ;; nr records in table
  (sequence uint64))   ;; unique id for new records (incrementing)


(defclass file-table ()
  ((name       :reader        file-table-name
               :initarg       :name)
   
   (schema     :reader        table-schema
               :initarg       :schema)

   (count      :accessor      file-table-count
               :allocation    :persistent
               :c-access-spec 'count
               :c-type        uint64)

   (sequence   :accessor      table-sequence
               :allocation    :persistent
               :c-access-spec 'sequence
               :c-type         uint64)
   
   (heap-pos   :reader        file-table-heap-pos
               :initarg       :heap-pos)

   (btrees     :reader        file-table-btrees
               :initarg       :btrees)

   (cache      :reader        file-table-cache))

  (:metaclass mmf:persistent-metalevel-class)
  (:c-type    file-table_t)
  (:cacheable t))

;; -----------------------------------------------------------------

(defvar *file-table-row-cacheing* t)

(defmacro with-locked-table ((tbl) &body body)
  `(with-locked-cache ((file-table-cache ,tbl)) ,@body))

#+:LISPWORKS
(editor:setup-indent "with-locked-table" 1)

;; -----------------------------------------------------------------

(defmethod initialize-instance :after ((tbl file-table) &key schema &allow-other-keys)
  (setf (slot-value tbl 'cache) (allocate-table-cache tbl))
  (when schema
    (setup-btree-functions tbl schema)))

;; -----------------------------------------------------------------

(defclass file-table-rep ()
  ((name        :reader  file-table-rep-name
                :initarg :name)
   
   (schema-uuid :reader  file-table-rep-schema-uuid
                :initarg :schema-uuid)

   (heap-pos    :reader  file-table-rep-heap-pos
                :initarg :heap-pos)

   (btrees      :reader  file-table-rep-btrees
                :initarg :btrees)

   (tblpos      :reader  file-table-rep-table-pos
                :initarg :table-pos)))
                   
;; ------------------------------------------------------------------

(defun nyi ()
  (error "Not yet implemented"))

;; ------------------------------------------------------------------

(defun delete-file-table (tbl)
  (declare (ignore tbl))
  (nyi))

#|
(defun delete-file-table (tbl)
  (with-read-lock ()
    (with-locked-table (tbl)
      (let ((heap (file-table-heap-pos tbl)))
        (unless (zerop heap)
          (delete-heap heap)))
      (let* ((trees (file-table-btrees tbl)))
        (unless (zerop (length trees))
          (loop for tree across trees do
                (delete-btree tree))))
      (discard tbl)
      (delete-file (db-path (file-table-name tbl)))
      )))
|#

;; ------------------------------------------------------------------

(defun find-file-table (table-name)
  (with-read-lock ()
    (when (probe-file (db-path table-name))
      (let* ((rep (retrieve-folder-item table-name)))
        (table-entry->table rep table-name))
      )))


(defun setup-btree-functions (table schema)
  (dolist (index-col (schema-indices schema))
    (let* ((ix (column-btree-index index-col))
           (btree (aref (file-table-btrees table) ix)))
      (setf-slots btree
                  'btree:compare-fn (curry 'compare index-col)
                  'btree:key-fn     (if (eq :unique (column-indexed index-col))
                                        (curry 'fetch-column-value table index-col)
                                      'get-column-list-key))
      )))
    
(defun table-entry->table (rep table-name)
  (let ((schema   (retrieve-folder-item
                   (resources-name (file-table-rep-schema-uuid rep)))))
    (make-instance 'file-table
                   :name         table-name
                   :mapper       (db-ptr (file-table-rep-table-pos rep)
                                         :type 'file-table_t)
                   :schema       schema
                   :heap-pos     (file-table-rep-heap-pos rep)
                   :btrees       (map 'vector #'(lambda (pos)
                                                  (make-file-btree
                                                   table-name
                                                   (db-ptr pos :type 'btree_t)))
                                      (file-table-rep-btrees rep)) )))

(defun make-file-table (&key name schema table-pos heap-pos btrees)
  ;; called by OK-Table to instantiate an anonymous file-table object
  (make-instance 'file-table
                 :name     name
                 :mapper   (db-ptr table-pos
                                   :type 'file-table_t)
                 :schema   schema
                 :heap-pos heap-pos
                 :btrees   (map 'vector #'(lambda (pos)
                                            (make-file-btree name
                                             (db-ptr pos :type 'btree_t)))
                                btrees) ))
                 
;; ------------------------------------------------------------------

(defun allocate-btree (name)
  (let* ((pbtree (db-ptr (alloc (mmf:size-of 'btree_t) +tree-header+)
                         :type 'btree_t))
         (btree  (make-file-btree name pbtree)))
    (initialize-file-btree btree)
    btree))

(defun create-new-unnamed-file-table (name schema &key use-heap)
  ;; use-heap should be nil, or an allocation count (integer)
  (let* ((off        (alloc (mmf:size-of 'file-table_t) +file-table+))
         (btrees     (map 'vector #'(lambda (col)
                                    (declare (ignore col))
                                    (allocate-btree name))
                          (schema-indices schema)))
         (heap-pos   (if (and (fixed-length-records-p schema)
                              use-heap)
                         (create-heap name (schema-row-size schema)
                                      use-heap)
                       0))
         (table      (make-instance 'file-table
                                    :name        name
                                    :mapper      (db-ptr off :type 'file-table_t)
                                    :schema      schema
                                    :heap-pos    heap-pos
                                    :btrees      btrees)))
    
    (setf (file-table-count table) 0
          (table-sequence   table) 0)
    table))

(defun create-new-file-table-rep (table)
  (make-instance 'file-table-rep
                 :name        (file-table-name table)
                 :schema-uuid (schema-uuid (table-schema table))
                 :heap-pos    (file-table-heap-pos table)
                 :btrees      (map 'vector 'mmf:pointer-address
                                   (file-table-btrees table))
                 :table-pos   (mmf:pointer-address table)) )

(defun create-new-file-table (table-name schema-name &key use-heap)
  (with-write-lock ()
    (let* ((schema (require-file-schema schema-name))
           (table  (create-new-unnamed-file-table table-name schema :use-heap use-heap))
           (rep    (create-new-file-table-rep table)))
      (create-folder-item table-name rep)
      table)))

;; -------------------------------------------------
#|
(defun select-column-btree-pointer (table column)
  (aref (file-table-btrees table) (column-btree-index column)))

(defun column-btree (table column)
  (let ((btree (select-column-btree-pointer table column)))
    (setf (slot-value btree 'btree:compare-fn)
          (curry 'compare column)

          (slot-value btree 'btree:key-fn)
          (curry 'fetch-column-value table column))
    btree))
|#

(defun column-btree (table column)
  (aref (file-table-btrees table) (column-btree-index column)))

#|
(defun column-list-btree (table column)
  (let ((btree (select-column-btree-pointer table column)))
    (setf (slot-value btree 'btree:compare-fn)
          (curry 'compare column)

          (slot-value btree 'btree:key-fn)
          'get-column-list-key)
    btree))
|#

(defun column-list-btree (table column)
  (aref (file-table-btrees table) (column-btree-index column)))


(defun delete-column-list-record (table column prow)
  (let* ((btree (column-list-btree table column))
         (key   (fetch-column-value table column prow)))
    (with-read-lock ()
      (with-locked-table (table)
        (when-let (clist (btree:find-item btree key))
          (delete-from-column-list clist prow)
          (when (= 0 (column-list-count clist))
            (btree:delete-item btree key)
            (discard clist))
          )))))

;; ----------------------------------------------

(defun store-column-list-item (prow clist)
  (declare (ignore prow clist))
  (nyi))

(defun delete-from-column-list (clist prow)
  (declare (ignore clist prow))
  (nyi))

(defun create-column-list (key)
  (declare (ignore key))
  (nyi))

(defun column-list-count (clist)
  (declare (ignore clist))
  (nyi))

(defun get-column-list-key (ptr)
  (declare (ignore ptr))
  (nyi))

(defun make-column-list (ptr)
  (declare (ignore ptr))
  (nyi))

;; ----------------------------------------------

(defun insert-column-list-record (table column prow)
  (let* ((btree (column-list-btree table column))
         (key   (fetch-column-value table column prow)))
    (labels
        ((update-column-list (plst)
           (let ((clist (make-column-list plst)))
             (store-column-list-item prow clist)))

         (add-column-list (key)
           (let ((clist (create-column-list key)))
             (store-column-list-item prow clist)
             clist)))
      
      (btree:add/update-item btree key
                             :add-fn    #'add-column-list
                             :update-fn #'update-column-list)
      )))

;; ----------------------------------------------

(defun pre-certify-row (names-and-values table &optional insertp)
  (let* ((schema (table-schema table))
         (row    (normalize-row
                  (if (and insertp
                           (schema-needs-index-counter schema)
                           (not (getf names-and-values :record-number)))
                      (prog1
                          (list* :record-number (table-sequence table) names-and-values)
                        (incf (table-sequence table)))
                    ;; else
                    names-and-values)
                  schema))
         (key-name (get-key-column-name schema)))
    
    (when (eq +absent+ (getf names-and-values key-name +absent+))
      (error "No key ~S specified in row: ~A"
             key-name names-and-values))
         
    row))

;; ----------------------------------------------

;; user API
(defmethod insert-row (names-and-values (table file-table))
  (insert-precertified-row (pre-certify-row names-and-values table :insert) table))

(defun get-index-info (row table)
  (let* ((schema    (table-schema table))
         (ucol      (get-key-column schema))
         (btree     (column-btree table ucol))
         (key-val   (getf row (column-name ucol))))
    (values schema ucol btree key-val)))

(defun get-key-column-info (table)
  (let* ((schema   (table-schema table))
         (ucol     (get-key-column schema))
         (btree    (column-btree table ucol)))
    (values schema ucol btree)))

(defun insert-precertified-row (row table)
  (multiple-value-bind (schema ucol btree key-val)
      (get-index-info row table)

    (labels ((fill-record (prow)
               (dolist (col (schema-column-specs schema))
                 (store-column-value (getf row (column-name col))
                                     table col prow))
               (with-locked-table (table)
                 (update-row-cache table prow row)))

             (add-to-column-lists (prow cols)
               (dolist (col cols)
                 (unless (eq col ucol)
                   (insert-column-list-record table col prow))))

             (update-record (prow)
               (let ((changed-cols
                      (nlet iter ((cols (schema-indices schema))
                                     (chgd nil))
                        (if (endp cols)
                            chgd
                          ;; else
                          (let ((col (first cols)))
                            (if (or (eq col ucol)
                                    (and (fixed-length-records-p schema)
                                         (= 0 (compare col
                                                       (getf row (column-name col))
                                                       (fetch-column-value table col prow)))))
                                (iter (rest cols) chgd)
                              ;; else
                              (progn
                                (delete-column-list-record table col prow)
                                (iter (rest cols) (cons col chgd)))
                              ))))))
                 (if (fixed-length-records-p schema)
                     (fill-record prow)
                   (progn
                     (discard-record table prow)
                     (setf (mmf:pointer-address prow)
                           (allocate-record table schema row))))
                 
                 (add-to-column-lists prow changed-cols)
                 prow))
             
             (add-record (key)
               (declare (ignore key))
               (let* ((pos  (allocate-record table schema row))
                      (prow (mmf:copy-pointer table :address pos)))
                 (incf (file-table-count table))
                 ;; (incf (table-sequence table)) ;; already updated in pre-certify
                 (if (fixed-length-records-p schema)
                     (fill-record prow))
                 (add-to-column-lists prow (schema-indices schema))
                 prow)))

      (btree:add/update-item btree key-val
                             :update-fn #'update-record
                             :add-fn    #'add-record)
      row)))

(defun allocate-record (table schema row)
  ;; If the records are fixed-length, just allocate space for it -
  ;; we'll fill in the individual record fields later.
  ;; If not fixed-length, then allocate and serialize the record here.
  (cond ((fixed-length-records-p schema)
         (let* ((heap-offs (file-table-heap-pos table)))
           (if (= 0 heap-offs)
               (alloc (schema-row-size schema)
                              (schema-uuid schema))
             ;; else
             (new-object (make-heap (file-table-name table) heap-offs)))))
        
        (t
         (mmf:pointer-address
          (serialize-data-object (mapcar #'(lambda (col)
                                             (getf row (column-name col)))
                                         (schema-column-specs schema))
                                 :type-uuid (schema-uuid schema)
                                 :start     0)))
        ))
                
;; -------------------------------------------------

(defun discard-record (table prow)
  (let* ((heap-offs (file-table-heap-pos table)))
    (if (= 0 heap-offs)
        (discard prow)
      ;; else - using heap storage
      (free-object (make-heap (file-table-name table) heap-offs)
                   (mmf:pointer-address prow))
      )))

(defmethod delete-row (names-and-values (table file-table))
  (delete-precertified-row (pre-certify-row names-and-values table) table))

;; user API
(defun delete-precertified-row (row table)
  (multiple-value-bind (schema ucol btree key-val)
      (get-index-info row table)

    (with-read-lock ()
      (with-locked-table (table)
        (let ((prow nil))
          (btree:delete-item btree key-val
                             :delete-fn #'(lambda (pobj)
                                          (setf prow pobj)))
          (when prow
            (decf (file-table-count table))
            (let ((ans (loop for col in (schema-column-specs schema)
                             collect (column-name col)
                             collect (fetch-column-value table col prow))))
              
              ;; remove row from all other indices
              (dolist (col (schema-indices schema))
                (unless (eq col ucol)
                  (delete-column-list-record table col prow)))
              
              ;; free up row storage-condition
              (discard-record table prow)
              (clear-row-cache table prow)
              
              ans))) ))))

;; -------------------------------------------------
    
;; -------------------------------------------------

(defmethod fetch-table-value ((table file-table) prow &rest args &key type offset)
  (declare (ignore table type offset))
  (apply 'mmf:fetch prow args))

(defmethod store-table-value (val (table file-table) prow &rest args &key type offset)
  (declare (ignore table type offset))
  (apply 'mmf:store val prow args))

;; -------------------------------------------------

(defmethod fetch-table-string ((table file-table) prow &key type offset)
  (declare (ignore table type))
  (fetch-string-indirect prow (+ (mmf:pointer-address prow) offset)))

(defmethod store-table-string (str (table file-table) prow &key type offset)
  (declare (ignore table type))
  (let ((ptr (insert-string
              (string-pool-pointer (database-mapper *current-okeanos-db*))
              str)))
    (mmf:store (mmf:pointer-address ptr) prow :type 'off_t :offset offset)))

;; -------------------------------------------------

(defun check-row-cache (table prow)
  (when *file-table-row-cacheing*
    (let ((addr (mmf:pointer-address prow)))
      (um:check-cache (table-cache-cache (file-table-cache table)) addr))))

(defun update-row-cache (table prow new-row)
  (when *file-table-row-cacheing*
    (let ((addr  (mmf:pointer-address prow))
          (cache (table-cache-cache (file-table-cache table))))
      (um:update-cache cache addr new-row)) )
  new-row)

(defun clear-row-cache (table prow)
  (when *file-table-row-cacheing*
    (let ((cache (table-cache-cache (file-table-cache table))))
      (when (um:check-cache cache (mmf:pointer-address prow))
        (um:clear-cache cache)) )))

;; -------------------------------------------------

(defmethod fetch-column-value ((table file-table) column prow)
  (with-read-lock ()
    (with-locked-table (table)
      (if-let (row (check-row-cache table prow))
          (getf row (column-name column))
        (cond ((fixed-length-records-p (table-schema table))
               (let ((fetchfn (column-fetch-function column)))
                 (funcall fetchfn table prow
                          :type   (column-c-type column)
                          :offset (column-offset column))))
              
              (t
               (let* ((val  (deserialize-data-object prow :start 0))
                      (cols (schema-column-specs (table-schema table)))
                      (row  (loop for col in cols
                                  for item in val
                                  collect (column-name col)
                                  collect item)))
                 (update-row-cache table prow row)
                 (getf row (column-name column))))
              )))))

;; ------------------------------------------------------------------
;; user API

(defmethod fetch-row (names-and-values (table file-table))
  (fetch-precertified-row (pre-certify-row names-and-values table) table))

(defun fetch-precertified-row (row table)
  (with-read-lock ()
    (with-locked-table (table)
      (when-let (prow (locate-precertified-row row table))
        (or (check-row-cache table prow)
            (construct-row table (table-schema table) prow))))))

(defun locate-row (names-and-values table)
  (locate-precertified-row (pre-certify-row names-and-values table) table))

(defun locate-precertified-row (row table)
  (multiple-value-bind (schema ucol btree key-val)
      (get-index-info row table)
    (declare (ignore ucol schema))
    (with-read-lock ()
      (btree:find-item btree key-val))))

(defun construct-row (table schema prow)
  (with-locked-table (table)
    (let ((new-row (loop
                    for column in (schema-column-specs schema)
                    for name = (column-name column)
                    for value = (fetch-column-value table (get-column schema name) prow)
                    collect name
                    collect value)))
      (update-row-cache table prow new-row))))

;; ------------------------------------------------------------------

(defun store-column-value (val table column prow)
  (with-read-lock ()
    (with-locked-table (table)
      (let* ((storefn    (column-store-function column))
             (type       (column-type column))
             (constraint (column-constraint column))
             (cval       (coerce val type)))
        (if constraint
            (assert (funcall constraint cval)))
        (assert (fixed-length-records-p (table-schema table)))
        (funcall storefn cval table prow
                 :type   (column-c-type column)
                 :offset (column-offset column))
        (when-let (crow (check-row-cache table prow))
          (setf (getf crow (column-name column)) cval))
        cval))))

(defun store-prescrubbed-column-value (val table column prow)
  (with-read-lock ()
    (with-locked-table (table)
      (let ((storefn    (column-store-function column)))
        (funcall storefn val table prow
                 :type   (column-c-type column)
                 :offset (column-offset column))
        (when-let (crow (check-row-cache table prow))
          (setf (getf crow (column-name column)) val))
        val))))

;; -------------------------------------------------

;; -------------------------------------------------

(defmethod map-rows ((table file-table) fn
                     &key
                     from to
                     (direction :forward)
                     max-records)
  (multiple-value-bind (schema ucol ptree)
      (get-key-column-info table)
    (with-read-lock ()
      (with-locked-table (table)
        (btree:map-tree ptree
                        #'(lambda (prow)
                          (funcall fn (construct-row table schema prow)))
                        :from      (and from
                                        (normalize-for-column from ucol))
                        :to        (and to
                                        (normalize-for-column to ucol))
                        :direction   direction
                        :max-records max-records)))))
  
;; ---------------------------------------------------

(defmethod fetch-all-rows ((table file-table) &rest args &key &allow-other-keys)
  (multiple-value-bind (schema ucol ptree)
      (get-key-column-info table)
    (declare (ignore schema ptree))
    (let ((kcol (column-name ucol)))
      (um:accum acc
        (apply 'map-rows table
               (compose #'acc
                        (rcurry 'getf kcol))
               args))
      )))

;; ---------------------------------------------------

(defmethod fetch-rows-for-column ((table file-table) column-name value 
                                  &key max-records)
  (multiple-value-bind (schema ucol ptree)
      (get-key-column-info table)
    (declare (ignore ptree))
    (if (eq column-name (column-name ucol))
        (list (getf (fetch-row `(,column-name ,value) table) column-name))
      (let* (rows
             (column (get-column schema column-name))
             (ukey   (column-name ucol))
             (pred   (compose 'zerop
                              (curry 'compare
                                     column
                                     (normalize-for-column value column))
                              (rcurry 'getf column-name)))
             (count  0))
        (block mapping
          (map-rows table #'(lambda (row)
                            (when (funcall pred row)
                              (push (getf row ukey) rows)
                              (incf count)
                              (when (and max-records
                                         (>= count max-records))
                                (return-from mapping)))) ))
        (nreverse rows)) )))

;; ---------------------------------------------------

(defmethod find-rows-for-column ((table file-table) column-name &rest args
                                 &key from to max-records)
  (multiple-value-bind (schema ucol ptree)
      (get-key-column-info table)
    (declare (ignore ptree))
    (let (rows)
      (if (eq column-name (column-name ucol))
          (apply 'map-rows table (rcurry 'push rows) args)
        (let* ((column    (get-column schema column-name))
               (ukey      (column-name ucol))
               (norm-from (normalize-for-column from column))
               (norm-to   (and to (normalize-for-column to column))))
          (multiple-value-bind (nfrom nto)
              (if norm-to
                  (if (minusp (compare column norm-to norm-from))
                      (values norm-to norm-from)
                    (values norm-from norm-to))
                (values norm-from nil))
            (let ((pred  #'(lambda (row)
                           (let* ((rval  (getf row column-name))
                                  (cfrom (compare column rval nfrom))
                                  (cto   (and nto (compare column rval nto))))
                             (and (not (minusp cfrom))
                                  (if nto
                                      (minusp cto)
                                    t)) )))
                  (count 0))
              (block mapping
                (map-rows table #'(lambda (row)
                                  (when (funcall pred row)
                                    (push (getf row ukey) rows)
                                    (incf count)
                                    (if (and max-records
                                             (>= count max-records))
                                        (return-from mapping)))) ))))))
      (nreverse rows)) ))

;; ---------------------------------------------------

(defstruct cursor
  constr btcursor btree)

(defmethod create-cursor ((table file-table) &key key (from-start t) from-end)
  (multiple-value-bind (schema ucol ptree)
      (get-key-column-info table)
    (with-read-lock ()
      (when-let (curs (btree:create-cursor ptree
                                              :key (and key
                                                        (normalize-for-column key ucol))
                                              :from-start from-start
                                              :from-end   from-end))
        (make-cursor
         :constr   (curry 'construct-row table schema)
         :btcursor curs
         :btree    ptree) ))))

(defun cursor-move (cursor btfn)
  (when cursor
    (with-read-lock ()
      (multiple-value-bind (prow found-it)
          (funcall btfn (cursor-btcursor cursor))
        (when found-it
          (funcall (cursor-constr cursor) prow)) ))
    ))

(defun cursor-next (cursor)
  (cursor-move cursor 'btree:cursor-next))

(defun cursor-previous (cursor)
  (cursor-move cursor 'btree:cursor-previous))

;; --------------------------------------------

(defmethod fetch-first-row ((table file-table))
  (multiple-value-bind (schema ucol ptree)
      (get-key-column-info table)
    (declare (ignore ucol))
    (with-read-lock ()
      (with-locked-table (table)
        (when-let (ptr (btree:first-item ptree))
          (construct-row table schema ptr))) )))

(defmethod fetch-last-row ((table file-table))
  (multiple-value-bind (schema ucol ptree)
      (get-key-column-info table)
    (declare (ignore ucol))
    (with-read-lock ()
      (with-locked-table (table)
        (when-let (ptr (btree:last-item ptree))
          (construct-row table schema ptr))) )))

;; --------------------------------------------

#+:LISPWORKS
(defmethod view-btree ((table file-table))
  (multiple-value-bind (schema ucol btree)
      (get-key-column-info table)
    (declare (ignore schema ucol))
    (btree::view-btree btree)))

;; --------------------------------------------

(defmethod perform-bulk-loading ((table file-table) data)
  (multiple-value-bind (schema ucol btree)
      (get-key-column-info table)
    
    (let* ((mapper    (database-mapper *current-okeanos-db*))
           (cols      (schema-column-specs schema))
           (nrows     (array-dimension data 0))
           (vpos      (make-array nrows))
           (keypos    (position ucol cols))
           (keys      (make-array nrows
                                  :initial-contents (loop for ix from 0 below nrows
                                                          collect (aref data ix keypos)) )))
      (declare (simple-vector vpos))

      (heap-bulk-load (make-heap (file-table-name table) (file-table-heap-pos table))
                      #'(lambda (fast-alloc-fn)
                        (let ((prow (mmf:copy-pointer mapper)))
                          (loop for ix from 0 below nrows do
                                (let ((pos (funcall fast-alloc-fn)))
                                  (setf (svref vpos ix) pos
                                        (mmf:pointer-address prow) pos)
                                  
                                  (loop for col in cols
                                        for jx from 0
                                        do
                                        (store-prescrubbed-column-value
                                         (aref data ix jx) table col prow))
                                  )))
                        (setf (file-table-count table) nrows
                              (table-sequence table)   nrows)
                        ))
      (create-bulk-tree btree keys vpos
                        :test (column-compare-function ucol))
      )))
