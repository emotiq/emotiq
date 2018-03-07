;; persistent-object.lisp -- persistent objects
;; --------------------------------------------------------------------------------------
;; Okeanos Database
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  03/09
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

(defclass persistent-object ()
  ()
  (:metaclass persistent-kernel-class)
  (:oid       #/oid/{00000000-0000-0000-0000-000000000005}))

(defmethod after-retrieve :after ((obj persistent-object))
  ;; initialize all the non-persistent slots
  (let* ((class      (class-of obj))
         (slot-names (mapcar 'clos:slot-definition-name
                             (remove-if 'is-persistent-slot
                                        (clos:class-slots class)))))
    ;; caveat emptor - CLOS initializes any slots mentioned in the default-initargs
    ;; even if the slot already has a value (slot-boundp)
    ;; So be sure to elide any persistent slots from your default-initargs
    (apply 'shared-initialize obj slot-names
           (clos:class-default-initargs class)) ))

;; -----------------------------------------------------------

(defmethod initialize-instance :after ((obj persistent-object) &key &allow-other-keys)
  (persist obj))

;; -------------------------------------------

(defclass persistent-kernel-object (persistent-object)
  ()
  (:metaclass persistent-kernel-class)
  (:oid       #/oid/{00000000-0000-0000-0000-000000000006}))

;; -------------------------------------------
;; used for encoding client objects before being sent to proxy server

(defmethod make-item-for-rawbytes-object (item (obj persistent-object) deleted)
  (let* ((class         (class-of obj))
         (indexed-slots (persistent-class-indexed-slots class))
         (new-indexes   (loop for slot in indexed-slots ;; actually (slotd . table-name) pairs
                              for slot-name = (clos:slot-definition-name (car slot))
                              for schema-type = (is-indexed-slot (car slot))
                              collect
                              `(,(basic-slot-value obj slot-name)
                                ,slot-name
                                ,schema-type
                                ,(cdr slot)) ))) ;; table name
    
    (make-persistent-rawbytes-item
     :oid            (persistent-item-oid item)
     :object         (unless deleted
                       (make-rawbytes :bytes (loenc:encode obj :prefix-length 4)))
     :dirty          (persistent-item-dirty item)
     :instance-table (persistent-class-instance-table class)
     :indexes        new-indexes) ))

;; -------------------------------------------

(defmethod ask-server-to-instantiate-copy ((obj persistent-object) &rest args &key &allow-other-keys)
  (let ((item (persistent-object-item obj)))
    (optima:ematch (rpc `(create-persistent-object-checking-oid
                         ,(class-name (class-of obj))
                         ,(persistent-item-oid item)
                         ,@args))
      ( (list :REINTERN old-oid)
        (reintern-persistent-item-with-oid item old-oid))
      
      ( :OK) )))

(defun reintern-persistent-item-with-oid (item new-oid)
  (remove-key-value (oid-hashkey (persistent-item-oid item)) (oid->item-mappings))
  (setf (persistent-item-oid item) new-oid)
  (setf (get-key-value (oid-hashkey new-oid) (oid->item-mappings)) item))

(defun create-persistent-object-checking-oid (class-name oid &rest args)
  (let* ((obj  (apply 'make-instance class-name args))
         (item (persistent-object-item obj)))
    (if (is-new-item item)
        (progn
          (reintern-persistent-item-with-oid item oid)
          :OK)
      ;; else -- we hit an existing entry - ask client to reintern
      (let ((old-oid (persistent-item-oid item)))
        `(:REINTERN ,old-oid) )))) ;; have the client use the old OID too

;; -----------------------------------------------------------

(defmethod pre-commit-for-object ((obj persistent-object) item)
  (declare (ignore item))
  (pre-commit-for-object (class-of obj) nil))

(defmethod pre-commit-for-object ((obj persistent-kernel-object) item)
  (declare (ignore item)))

;; -----------------------------------------------------------
;; Indexed slots maintenance

(defun schema-name-for-index-type (type)
  (case type
    (:unique  +oid-index-schema-path+)
    (:indexed +oid-index-collection-schema-path+)))
    
;; -------------------------------------------------------------------------

(defun is-new-item (item)
  (find :new-object (persistent-item-dirty item) :key 'first))

(defun get-index-table (table-name schema-type)
  (or (find-file-table table-name)
      (create-new-file-table
       table-name
       (schema-name-for-index-type schema-type)
       :use-heap 1000)))

(defmethod validate-object-commit ((obj rawbytes) (item persistent-rawbytes-item))
  (factored-validate-object-commit item (persistent-item-indexes item)
                                   'destructure-for-rawbytes-item))

(defmethod validate-object-commit ((obj persistent-object) (item persistent-item))
  (let* ((class          (class-of obj))
         (indexed-slots  (persistent-class-indexed-slots class)))
    (factored-validate-object-commit item indexed-slots
                                     (curry 'destructure-for-persistent-object-slots obj))))

(defun destructure-for-rawbytes-item (slot)
  (destructuring-bind (key slot-name schema-type table-name) slot
    (values key slot-name schema-type table-name)))

(defun destructure-for-persistent-object-slots (obj slot)
  (destructuring-bind (slotd . table-name) slot
    (let ((slot-name (clos:slot-definition-name slotd))
          (schema-type (is-indexed-slot slotd)))
      (values (basic-slot-value obj slot-name)
              slot-name
              schema-type
              table-name)) ))

(defun factored-validate-object-commit (item indexed-slots normfn)
  (let ((is-new-obj     (is-new-item item))
        (is-deleted     (is-deleted item))
        (oid            (persistent-item-oid item)))

    (dolist (slot indexed-slots)
      (multiple-value-bind (key slot-name schema-type table-name) (funcall normfn slot)
        (let ((old-key (get-old-key item slot-name)))
          
          (when (or old-key ;; this slot may not have changed
                    is-new-obj
                    is-deleted)
            
            (let* ((table (get-index-table table-name schema-type)))

              (case schema-type
              (:unique
               (when-let (existing (fetch-row `(:key ,key) table))
                 (let ((prev-oid (oid-for-object (getf existing :oid))))
                   (unless (oid= oid prev-oid)
                     (error "Attempting to define duplicate of unique object: ~A"
                            (persistent-item-object item))
                     #|
                     ;; make a pointer chain from old oid to new oid to new object
                     ;; raw oids dereference until no longer an oid
                     (intern-persistent-item (make-persistent-item
                                              :oid    prev-oid
                                              :object (make-instance 'ioid
                                                                     :oid oid)
                                              :dirty  '((:new-object)))) ))
                     |#
                     ))))
              (:indexed
               (when old-key
                 (validate-key-collection (third old-key) table))
               (validate-key-collection key table))) )) )) )))


(defun validate-key-collection (key table)
  (when-let (row (fetch-row `(:key ,key) table))
    (let ((coll (deref (getf row :oid))))
      (validate-commit (persistent-object-item coll))
      )))

;; -------------------------------------------------------------------------

(defun get-or-create-index-collection (table key)
  (if-let (row (fetch-row `(:key ,key) table))
      (deref (getf row :oid))
    
    ;; else - no entry yet, so make one
    (let ((coll (persist (list nil))))
      (insert-row `(:key ,key :oid ,(ref coll)) table)
      coll)))

(defmethod commit-object-changes :after ((obj rawbytes) (item persistent-rawbytes-item))
  (factored-commit-object-changes item
                                  (persistent-item-instance-table item)
                                  (persistent-item-indexes item)
                                  'destructure-for-rawbytes-item))

(defmethod commit-object-changes :after ((obj persistent-object) (item persistent-item))
  (let* ((class          (class-of obj))
         (instance-table (persistent-class-instance-table class))
         (indexed-slots  (persistent-class-indexed-slots class)))
    (factored-commit-object-changes item instance-table indexed-slots
                                    (curry 'destructure-for-persistent-object-slots obj))))

(defun factored-commit-object-changes (item instance-table indexed-slots normfn)
  (let* ((oid            (persistent-item-oid item))
         (is-new-obj     (is-new-item item)))
    
    ;; add instance to class instance set
    (when is-new-obj
      (let ((btree (or (find-file-oid-set instance-table)
                       (make-oid-set (create-oid-set instance-table))))
            (prow  (locate-row `(:oid ,oid) (oid-mappings))))
        (btree:insert-item btree oid prow)))

    ;; add instance to various index tables
    (dolist (slot indexed-slots)
      (multiple-value-bind (key slot-name schema-type table-name) (funcall normfn slot)
        (let ((old-key (get-old-key item slot-name)))
          
          (when (or is-new-obj
                    old-key)
            (let ((table (get-index-table table-name schema-type)))
        
              (case schema-type
                (:unique
                 (when old-key
                   (delete-row `(:key ,(third old-key)) table))
                 (insert-row `(:key ,key :oid ,(ref item)) table))
              
                (:indexed
                 (when old-key
                   (remove-key-from-collection (third old-key) oid table))
                 (let ((coll (get-or-create-index-collection table key)))
                   (unless (member oid (car coll) :test 'oid=)
                     (push oid (car coll))
                     (mark-dirty coll))))
                )))
          ))
      )))

;; -----------------------------------------------------------
#|
(open-okeanos-db)
(map-rows (get-class-table) 'print)
(map-rows (get-class-table) #'(lambda (row)
                              (print `(:class ,(decode-string-to-object (getf row :key))
                                       :oid   ,(getf row :oid)))))
(map-rows (find-file-table "Classes/OKEANOS/ADDRESS-ENTRY/Index/LOCATION") 'print)
(map-rows (find-file-table "Classes/OKEANOS/ADDRESS-ENTRY/UIndex/NAME") 'print)
(close-okeanos-db)
|#
;; -----------------------------------------------------------

(defun get-persistent-class (class-name &optional (errorp t))
  (or (find-class class-name nil)
      (deref (perform 'get-persistent-class-oid class-name))
      (if errorp
          (error "Can't find class: ~A" class-name))
      ))

(defun get-persistent-class-oid (class-name)
  (if-let (tbl (get-class-table))
      (if-let (row (fetch-row `(:key ,class-name) tbl))
          (getf row :oid)) ))

;; -----------------------------------------------------------

(defmethod commit-object-deletion :after ((obj rawbytes) (item persistent-rawbytes-item))
  (factored-commit-object-deletion item
                                   (persistent-item-instance-table item)
                                   (persistent-item-indexes item)
                                  'destructure-for-rawbytes-item))


(defmethod commit-object-deletion :after ((obj persistent-object) (item persistent-item))
  (let* ((class          (class-of obj))
         (instance-table (persistent-class-instance-table class))
         (indexed-slots  (persistent-class-indexed-slots class)))
    (factored-commit-object-deletion item instance-table indexed-slots
                                     (curry 'destructure-for-persistent-object-slots obj))))


(defun factored-commit-object-deletion (item instance-table indexed-slots normfn)
  (let ((oid (persistent-item-oid item)))
    
    ;; remove instance from class instance set
    (let ((btree (or (find-file-oid-set instance-table)
                     (make-oid-set (create-oid-set instance-table)))))
      (btree:delete-item btree oid))

    ;; remove instance from various index tables
    (dolist (slot indexed-slots)
      (multiple-value-bind (key slot-name schema-type table-name) (funcall normfn slot)
        (declare (ignore slot-name))
        (when-let (table (find-file-table table-name))
          (case schema-type
            (:unique  (delete-row `(:key ,key) table))
            (:indexed (remove-key-from-collection key oid table))
            ))
        ))))

(defun remove-key-from-collection (key oid table)
  (when-let (row (fetch-row `(:key ,key) table))
    (let* ((coll (deref (getf row :oid)))
           (rem  (delete oid (car coll) :test 'oid=)))
      (if rem
          (progn
            (setf (car coll) rem)
            (mark-dirty coll))
        (progn
          (discard-persistent-object coll)
          (delete-row `(:key ,key) table)))
      )))

;; -----------------------------------------------------------
;;
;; -------------------------------------------------------

(defvar *print-persistent-fields* t)

(defmethod print-object ((obj persistent-object) stream)
  (if *print-persistent-fields*
      (print-unreadable-object (obj stream :type t :identity t)
        (format stream "~{~S ~A~^ ~}"
                (foldl #'(lambda (acc slotd)
                            (let ((name (clos:slot-definition-name slotd)))
                              (list* (intern (string name) 'keyword)
                                     (if (slot-boundp obj name)
                                         (slot-value obj name)
                                       '*unbound*)
                                     acc)))
                          nil (remove-if (complement 'is-persistent-slot)
                                         (clos:class-slots (class-of obj))) )))
    (call-next-method)))
