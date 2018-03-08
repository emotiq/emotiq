;; internal-schema.lisp -- Object ID Sets and OID files
;; --------------------------------------------------------------------------------------
;; OkeanosMM -- memory mapped database system
;;
;; DM/RAL  03/09
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

(defclass file-tree-rep ()
  ((name        :reader  file-tree-rep-name
                :initarg :name)
   (btree-pos   :reader  file-tree-rep-btree-pos
                :initarg :btree-pos)
   (compare     :reader  file-tree-rep-compare
                :initarg :compare)
   (key         :reader  file-tree-rep-key
                :initarg :key)))

(defun create-oid-set (key)
  (let* ((pos  (alloc (mmf:size-of 'btree_t)
                              +tree-header+))
         (phdr (db-ptr pos :type 'btree_t))
         (tree (make-file-btree key phdr
                                :compare 'compare-oid
                                :key     'fetch-oid))
         (rep  (make-instance 'file-tree-rep
                              :name       key
                              :btree-pos  (mmf:pointer-address tree)
                              :compare    'compare-oid
                              :key        'fetch-oid)))
    (initialize-file-btree tree)
    (create-folder-item key rep)
    rep))

(defun fetch-oid (ptr)
  (make-oid :uuid (mmf:fetch ptr :type 'uuid_t)))

(defun make-oid-set (rep)
  (make-file-btree (file-tree-rep-name rep)
                   (file-tree-rep-btree-pos rep)
                   :compare (file-tree-rep-compare rep)
                   :key     (file-tree-rep-key rep)))

(defun find-file-oid-set (key)
  (when (probe-file (db-path key))
    (make-oid-set (retrieve-folder-item key))))

;; -------------------------------------------
;; -------------------------------------------
;; Encoded string compare -- permits encoded strings to be compared
;; lexicographically. That can't happen under normal comparison in encoded form
;; because short strings would compare lower than long strings, regardless
;; of textual content. These routines correct that situation.

(defstruct encoded-string
  s memo)

(defun make-encoded-string-from-object (obj)
  (let ((robj (ref obj)))
    (make-encoded-string
     :memo robj
     :s    (encode-object-to-string robj))))

(defun get-encoded-string-value (s)
  (or (encoded-string-memo s)
      (setf (encoded-string-memo s) (decode-string-to-object (encoded-string-s s)))))

(defmethod ord:compare ((a encoded-string) (b encoded-string))
  (if (eql (type-of (get-encoded-string-value a))
           (type-of (get-encoded-string-value b)))
      (ord:compare (get-encoded-string-value a) (get-encoded-string-value b))
    (ord:compare (encoded-string-s a) (encoded-string-s b)) ))

;; -------------------------------------------
;; encoded values convert objects into fast string-cache entries

(defmethod make-column (name (type (eql :encoded-value))
                             &rest args
                             &key
                             (compare-function #|'encoded-string<|# 'ord:compare<)
                             (equality-predicate 'string=)
                             &allow-other-keys)
  (apply 'make-instance 'column
         :name               name
         :type               'encoded-string
         :compare-function   compare-function
         :equality-predicate equality-predicate
         :value-normalizer   'encode-object-to-string-for-column
         :fetch-function     'fetch-encoded-object ;; 'fetch-table-string
         :store-function     'store-encoded-object ;; 'store-table-string
         args))

(defun encode-object-to-string-for-column (obj col)
  (declare (ignore col))
  ;; (encode-object-to-string (ref obj))
  (make-encoded-string-from-object obj))

(defun fetch-encoded-object (table prow &key type offset)
  (make-encoded-string
   :s (fetch-table-string table prow :type type :offset offset)))

(defun store-encoded-object (estr table prow &key type offset)
  (store-table-string (encoded-string-s estr) table prow :type type :offset offset))

;; currently unused...
(defun fetch-decode-table-string (table prow &key type offset)
  (decode-string-to-object (fetch-table-string table prow
                                               :type type :offset offset)))

;; -------------------------------------------
;; UUID columns refer to stored UUID's

(defmethod make-column (name (type (eql :uuid))
                             &rest args
                             &key
                             &allow-other-keys)
  (apply 'make-instance 'column
         :name               name
         :type               'uuid:uuid
         :compare-function   'uuid:uuid<
         :equality-predicate 'uuid:uuid=
         :value-normalizer   'normalize-uuid
         :fetch-function     'fetch-table-uuid
         :store-function     'store-table-uuid
         :default-value      #.(uuid:make-null-uuid)
         args))

(defun normalize-uuid (val col)
  (declare (ignore col))
  (cond ((typep val 'uuid:uuid) val)
        ((integerp val) (uuid:integer-to-uuid val))
        ((stringp val)  (uuid:make-uuid-from-string val))
        ((keywordp val) (uuid:make-uuid-from-string (symbol-name val)))
        ((vectorp val)  (uuid:byte-array-to-uuid val)) ))

(defun fetch-table-uuid (table prow &rest args)
  (declare (ignore table))
  (apply 'mmf:fetch prow args))

(defun store-table-uuid (val table prow &rest args)
  (declare (ignore table))
  (apply 'mmf:store val prow args))

;; -------------------------------------------
;; OID columns refer to stored UUID's converting them to OID's and back

(defmethod make-column (name (type (eql :oid))
                             &rest args
                             &key
                             &allow-other-keys)
  (apply 'make-instance 'column
         :name               name
         :type               'oid
         :compare-function   'oid<
         :equality-predicate 'oid=
         :constraint         'oid-p
         :value-normalizer   'normalize-oid
         :fetch-function     'fetch-table-oid
         :store-function     'store-table-oid
         args))

(defun normalize-oid (val col)
  (cond ((oid-p val) val)
        (t (make-oid :uuid (normalize-uuid val col))) ))

(defun fetch-table-oid (table prow &rest args)
  (declare (ignore table))
  (make-oid :uuid (apply 'mmf:fetch prow args)))

(defun store-table-oid (val table prow &rest args)
  (declare (ignore table))
  (apply 'mmf:store (oid-uuid val) prow args))

;; -------------------------------------------
;; ref columns automatically convert raw OID UUID's into REF's and back
#| Nah! KISS!!

(defmethod make-column (name (type (eql :ref))
                             &rest args
                             &key
                             &allow-other-keys)
  (apply 'make-instance 'column
         :name               name
         :type               'oid
         :compare-function   'oid<
         :equality-predicate 'same-ref
         ;; :value-normalizer   'make-persistent-for-column
         :fetch-function     'fetch-table-ref
         :store-function     'store-table-ref
         args))

(defmethod uuid-to-oid ((uuid uuid:uuid))
  (make-oid
   :uuid uuid))

;; the prow points to an off_t which in turn points to the OID table entry
;; which begins with a UUID
(defun fetch-table-ref (table prow &rest args)
  (declare (ignore table))
  (uuid-to-oid
   (mmf:fetch prow
              :address (apply 'mmf:fetch prow args)
              :type    'oid_t)))

(defun store-table-ref (val table prow &rest args)
  (declare (ignore table))
  (let ((puuid (ensure-oid-row val)))
    (apply 'mmf:store (mmf:pointer-address puuid) prow args)))

(defun ensure-oid-row (val)
  (let ((row   `(:oid ,val))
        (table (oid-mappings)))
    (or (locate-row row table)
        (progn
          (insert-row row table)
          (locate-row row table)) )))
|#

#|
(defun make-persistent-for-column (obj col)
  (declare (ignore col))
  (make-persistent obj))
|#
;; -------------------------------------------

(fli:define-c-struct oid-index-entry_t
  (:byte-packing 1)
  (keyi_p      off_t)
  (oidi_p      oid_t))

(defun ensure-oid-index-entry-schema (&key force)
  (ensure-schema +oid-index-schema-path+
      '((:key           :encoded-value
         :c-type        off_t
         :c-access-spec keyi_p
         :indexed       :unique)
        
        (:oid           :oid
         :c-type        oid_t
         :c-access-spec oidi_p))

    :c-type 'oid-index-entry_t
    :force  force))

;; -------------------------------------------
;; -------------------------------------------

(fli:define-c-struct oid-index-collection_t
  (:byte-packing 1)
  (keyic_p    off_t)
  (oidic_p    oid_t)) ;; this oid points to a collection of oid's

(defun ensure-oid-index-collection-schema (&key force)
  (ensure-schema +oid-index-collection-schema-path+
      '((:key           :encoded-value
         :c-type        off_t
         :c-access-spec keyic_p
         :indexed       :unique)
        
        (:oid           :oid
         :c-type        oid_t
         :c-access-spec oidic_p))
    
    :c-type 'oid-index-collection_t
    :force  force))
               
                
;; -------------------------------------------

