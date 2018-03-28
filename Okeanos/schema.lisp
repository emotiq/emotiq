;; schema.lisp -- Database Schema representations
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

;; -----------------------------------------------------------------

;; ------------------------------------------------------------------

(defun resources-name (name)
  (format nil "Sys/Resources/~A" (translate-to-filename name)))

;; ------------------------------------

(defclass column ()
  ((name
    :reader column-name
    :initarg :name)

   (type
    :reader column-type
    :initarg :type)

   (equality-predicate
    :reader column-equality-predicate
    :initarg :equality-predicate)

   (compare-function
    :reader column-compare-function
    :initarg :compare-function)

   (default-value
    :reader column-default-value
    :initarg :default-value
    :initform nil)

   (constraint
    :reader column-constraint
    :initarg :constraint
    :initform nil)
   
   (value-normalizer
    :reader column-value-normalizer
    :initarg :value-normalizer
    :initform nil)

   (c-access-spec
    :reader column-c-access-spec
    :initform nil
    :initarg :c-access-spec)

   (offset
    :reader column-offset
    :initform nil
    :initarg :offset)
   
   (c-type
    :reader column-c-type
    :initform nil
    :initarg :c-type)

   (fetch-function
    :reader column-fetch-function
    :initarg :fetch-function
    :initform 'fetch-table-value)

   (store-function
    :reader column-store-function
    :initarg :store-function
    :initform 'store-table-value)
   
   (indexed
    :reader column-indexed
    :initarg :indexed
    :initform nil) ;; nil, t, or :unique

   (btree-index
    :reader column-btree-index
    :initarg :btree-index
    :initform nil)
))

;; -------------------------------------------------

(defmethod compare ((column column) key val)
  ;; cmpfn is stored in the column spec.
  ;; it should be a compare< function, e.g., string<, char<, <, etc.
  (let* ((cmpfn (column-compare-function column)))
    (cond ((funcall cmpfn key val) -1)
          ((funcall cmpfn val key)  1)
          (t 0))
    ))

;; ------------------------------------------------------------------------
(defgeneric make-column (name type &key &allow-other-keys))
;; ------------------------------------------------------------------------

(defmethod make-column (name (type (eql :number))
                             &rest args
                             &key
                             (compare-function '<)
                             (equality-predicate '=)
                             (default-value 0)
                             c-type
                             &allow-other-keys)
  (apply 'make-instance 'column
         :name               name
         :type               (mmf:lisp-type-for-c-type c-type)
         :compare-function   compare-function
         :equality-predicate equality-predicate
         :default-value      default-value
         args))

(defmethod make-column (name (type (eql :string))
                             &rest args
                             &key
                             (compare-function 'string<)
                             (equality-predicate 'string=)
                             &allow-other-keys)
  (apply 'make-instance 'column
         :name               name
         :type               'string
         :compare-function   compare-function
         :equality-predicate equality-predicate
         :value-normalizer   'not-nullable
         :constraint         'stringp
         :c-type             'off_t
         :fetch-function     'fetch-table-string
         :store-function     'store-table-string
         args))

(defun not-nullable (value column)
  (or value
      (error "Column ~A can't be null" (column-name column))
      ))

;; -------------------------------------------------

(defclass schema ()
  ((name
    :reader   schema-name
    :initarg  :name)
   
   (c-type
    :reader   schema-c-type
    :initarg  :c-type)
   
   (row-size
    :reader   schema-row-size
    :initarg  :row-size)
   
   (column-specs
    :reader   schema-column-specs
    :initarg  :column-specs)
   
   (indices
    :reader   schema-indices
    :initform nil)
   
   (needs-index-counter
    :reader   schema-needs-index-counter
    :initform nil)
  ))

(defclass file-schema (schema)
  ((name
    :reader   schema-name
    :initarg  name)
   
   (uuid
    :reader   schema-uuid
    :initarg  :uuid)
   ))
   
;; ------------------------------------------------------------------

(defmethod initialize-instance :after ((schema schema) &key &allow-other-keys)
  (let ((ctype       (schema-c-type schema))
        (have-unique nil)
        (indices     nil)
        (tree-index  0))
    (loop for col in (schema-column-specs schema)
          for col-pos from 1
          do
          (if ctype ;; fixed-length records?
              (setf (slot-value col 'offset)
                    (mmf:foreign-slot-offset ctype (column-c-access-spec col))))
          (when-let (indexing (column-indexed col))
            (if (eq :unique indexing)
                (if have-unique
                    (error "Only one column can be uniquely indexed")
                  ;; else
                  (setf have-unique col)))
            (push col indices)
            (setf (slot-value col 'btree-index) tree-index)
            (incf tree-index)))
    
    (unless have-unique
      (let* ((ct-type   'uint64)
             (ct-size   (mmf:size-of ct-type))
             (ct-offset (if ctype
                            (align-pwr2 (mmf:size-of ctype) ct-size)
                          0))
             (index-col (make-column :record-number   :number
                                     :c-type          ct-type
                                     :offset          ct-offset
                                     :indexed         :unique
                                     :btree-index     tree-index)))
        (setf-slots schema
                    'needs-index-counter  t
                    'row-size             (+ ct-size ct-offset))
        (push index-col (slot-value schema 'column-specs))
        (push index-col indices)))
    (setf (slot-value schema 'indices) indices)
    ))

;; ------------------------------------------------------------------

(defun compute-schema-slots (column-specs c-type)
  `(:column-specs ,(mapcar #'(lambda (column-spec)
                               (apply 'make-column column-spec))
                           column-specs)
    :c-type       ,c-type
    :row-size     ,(if c-type
                       (mmf:size-of c-type)
                     0)))

(defun make-schema (name column-specs &key c-type)
  (apply 'make-instance 'schema
         :name name
         (compute-schema-slots column-specs c-type)))

(defun make-file-schema (name column-specs &key c-type)
  (apply 'make-instance 'file-schema
         :name name
         :uuid (mkstr (make-uuid))
         (compute-schema-slots column-specs c-type)))

;; ------------------------------------------------------------------

(defun create-new-file-schema (schema-name schema)
  (let ((uuid (schema-uuid schema)))
    (create-folder-item (resources-name uuid) schema)
    (create-folder-item schema-name uuid)
    schema))

;; ------------------------------------------------------------------

(defun find-file-schema (schema-name)
  (ignore-errors
    (let ((uuid (retrieve-folder-item schema-name)))
      (retrieve-folder-item (resources-name uuid))) ))

(defun require-file-schema (schema-name)
  (or (find-file-schema schema-name)
      (error "No schema named ~A" schema-name)))

;; ------------------------------------------------------------------

(defun ensure-schema (name cols &key force c-type)
  (when (or force
            (not (find-file-schema name)))
    (let ((s (make-file-schema name cols :c-type c-type)))
      (create-new-file-schema name s)) ))

#+:LISPWORKS
(editor:setup-indent "ensure-schema" 2 2)

;; ------------------------------------------------------------------

(defun fixed-length-records-p (schema)
  (schema-c-type schema))

;; -------------------------------------------------

(defun normalize-row (names-and-values schema)
  (loop
   for column in (schema-column-specs schema)
   for name = (column-name column)
   for value = (let* ((val (getf names-and-values name +absent+)))
                 (if (eq val +absent+)
                     (column-default-value column)
                   val))
   collect name
   collect (normalize-for-column value column)))

(defun normalize-for-column (value column)
  (let* ((normalizer (column-value-normalizer column))
         (val (if normalizer (funcall normalizer value column) value))
         (constraint (column-constraint column)))
    (when (and constraint
               (not (funcall constraint val)))
      (error "Value fails constraint: ~A, col: ~A" value (column-name column)))
    val))
                                     
(defun prescrub-column-value (val column)
  (let* ((nval (normalize-for-column val column))
         (type (column-type column)))
    (coerce nval type)))

;; -------------------------------------------------

(defun get-column (schema name)
  (find name (schema-column-specs schema) :key 'column-name))

;; -------------------------------------------------

(defmethod get-column-names ((schema schema))
  (loop for column in (schema-column-specs schema)
        for name = (column-name column)
        collect name))

(defmethod get-key-column-name ((schema schema))
  (column-name (get-key-column schema)))

(defmethod get-key-column-test ((schema schema))
  (column-equality-predicate (get-key-column schema)))

(defmethod get-key-column ((schema schema))
  (find :unique (schema-indices schema) :key 'column-indexed))


