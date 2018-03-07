;; persistent-class.lisp -- persistent objects
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


;; ===========================================================================
;; -------------------------------
;; Persistent Objects

(defclass persistent-class (#+:LISPWORKS standard-class
                            #+:ALLEGRO   clos::standard-class)
  
  ((indexed-slots    :accessor persistent-class-indexed-slots)
   ;; indexed-slots -- a list of slotdefs that are indexed
   
   (instance-table   :accessor persistent-class-instance-table)
   ;; instance-table -- the name of the Table file for instances of this class

   (oid              :reader   persistent-class-oid
                     :initarg  :oid)

   (def-oid          :accessor persistent-class-def-oid
                     :initform nil)
   (def              :accessor persistent-class-def
                     :initform nil)
   ))

(defclass persistent-kernel-class (persistent-class)
  ())

;; -------------------------------------------------------------------
;; Make Standard-Class a valid superclass of Persistent-class
;; This allows mixins and inheritance from normal classes.
;; The resulting class will have all the inherited slots persisted.

(defmethod clos:validate-superclass ((class persistent-class)
                                     (super standard-class))
  t)

;; -------------------------------

(defmethod initialize-instance :around ((class persistent-class) &rest args &key &allow-other-keys)
  (apply #'call-next-method
         class
         (apply 'fixup-persistent-class-initargs class args)))

;; -------------------------------

(defmethod reinitialize-instance ((class persistent-class) &rest args &key &allow-other-keys)
  (unregister-persistent-class class)
  (setf (persistent-class-def class) nil)
  (apply #'call-next-method
         class
         (apply 'fixup-persistent-class-initargs class args)))

;; -------------------------------

(defvar *registered-persistent-classes* (make-hash-table))

(defmethod initialize-instance :after ((class persistent-class) &key &allow-other-keys)
  (register-persistent-class class))

(defmethod reinitialize-instance :after ((class persistent-class) &key &allow-other-keys)
  (register-persistent-class class))

(defmethod after-retrieve ((class persistent-class))
  (register-persistent-class class))

(defun register-persistent-class (class)
  (setf (get-key-value (oid-hashkey (car (persistent-class-oid class))) *registered-persistent-classes*)
        class))

(defun unregister-persistent-class (class)
  (if (slot-boundp class 'oid)
      (remove-key-value (oid-hashkey (car (persistent-class-oid class))) *registered-persistent-classes*)))

(defmethod lookup-registered-class ((oid oid))
  (get-key-value (oid-hashkey oid) *registered-persistent-classes*))

;; -------------------------------

(defun fixup-persistent-class-initargs (class &rest initargs
                                              &key
                                              direct-superclasses
                                              direct-slots
                                              &allow-other-keys)
  (list*
   :direct-superclasses (maybe-add-persistent-object-class
                         class
                         direct-superclasses)
   :direct-slots        (fixup-persistent-slotdefs direct-slots)
   ;; Tell Lispworks it shouldn't bypass slot-value-using-class
   #+:LISPWORKS :optimize-slot-access #+:LISPWORKS nil
   initargs))
  
;; -----------------------------------------------------------
;; Fixup direct slot specs to allow
;;   :allocation :persistent
;;   :indexed    :unique or t
;;   :persistent :unique, :indexed, t, or nil
;;   default to persistent slots

(defun fixup-persistent-slotdefs (slotdefs)
  (loop for slotdef in slotdefs
        collect (apply 'fixup-persistent-slotdef slotdef)))

(defun fixup-persistent-slotdef (&rest initargs
                                       &key
                                       (allocation :persistent)
                                       indexed
                                       &allow-other-keys)
  (if (eq allocation :persistent)
      (let ((args  (copy-list initargs))
            (ptype (if indexed
                       (if (eq indexed :unique)
                           :unique
                         :indexed)
                     t)))
        (remf args :indexed)
        (setf (getf args :allocation) :instance
              (getf args :persistent) ptype)
        args)
    ;; else
    initargs))

;; -----------------------------------------------------------

(defun maybe-add-persistent-object-class (class direct-superclasses)
  ;; Add PERSISTENT-OBJECT to the superclass list if necessary.
  (bind*
      ((root-class       (find-class 'persistent-object nil))
       (persistent-class (find-class 'persistent-class)))
    
    (if (or (null root-class)
            (eql class root-class)
            (find-if #'(lambda (direct-superclass)
                       (member persistent-class
                               (clos:compute-class-precedence-list
                                (class-of direct-superclass))))
                     direct-superclasses))
        direct-superclasses
      (cons root-class direct-superclasses))))

;; -----------------------------------------------------------

(defun get-direct-slotd (class slot-name)
  (find slot-name (clos:class-direct-slots class)
        :key 'clos:slot-definition-name))

;; -----------------------------------------------------------

(defun locate-class-defining-slot (class slot-name)
  (nlet iter ((class class))
    (if (get-direct-slotd class slot-name)
        (return-from locate-class-defining-slot class)
      (dolist (super (clos:class-direct-superclasses class))
        (iter super))
      )))

(defun make-slot-table-name-pair (class slotd)
  (let* ((slot-name  (clos:slot-definition-name slotd))
         (root-class (locate-class-defining-slot class slot-name))
         (class-name (clos:class-name root-class)))
    (cons slotd
          (table-name-for-class-and-slot class-name slot-name)) ))

(defun table-name-for-class-and-slot (class-name slot-name)
  (format nil "Classes/~A/~A/Indexes/~A"
          (translate-to-filename (um:true-package-name (symbol-package class-name)))
          (translate-to-filename class-name)
          (translate-to-filename slot-name)))

(defun compute-indexed-slots (class)
  (bind*
      ((effective-slots (clos:class-slots class))
       (indexed-slots (remove-if (complement 'is-indexed-slot)
                                 effective-slots)))
    (mapcar (curry 'make-slot-table-name-pair class) indexed-slots)))
  
(defmethod clos:finalize-inheritance :after ((class persistent-class))
  (setf (persistent-class-indexed-slots class)
        (compute-indexed-slots class)
        
        (persistent-class-instance-table class)
        (table-name-for-class (clos:class-name class)) ))

(defun table-name-for-class (class-name)
  (format nil "Classes/~A/~A/Instances"
          (translate-to-filename (um:true-package-name (symbol-package class-name)))
          (translate-to-filename class-name)))

;; -----------------------------------------------------------

(defclass persistent-slot-definition (clos:standard-slot-definition)
  ((persistence :initarg :persistent)))

(defclass indexed-slot-mixin ()
  ())

(defclass unique-slot-mixin ()
  ())

(defmethod is-persistent-slot (slotd)
  (declare (ignore slotd)))

(defmethod is-persistent-slot ((slotd persistent-slot-definition))
  slotd)


(defmethod is-indexed-slot (slotd)
  (declare (ignore slotd)))

(defmethod is-indexed-slot ((slotd indexed-slot-mixin))
  :indexed)

(defmethod is-indexed-slot ((slotd unique-slot-mixin))
  :unique)

(defmethod is-unique-index-slot (slotd)
  (declare (ignore slotd)))

(defmethod is-unique-index-slot ((slotd unique-slot-mixin))
  :unique)

;; -----------------------------------------------

(defclass persistent-direct-slot-definition
          (clos:standard-direct-slot-definition
           persistent-slot-definition)
  ())

(defclass indexed-persistent-direct-slot-definition
          (indexed-slot-mixin persistent-direct-slot-definition)
  ())

(defclass unique-persistent-direct-slot-definition
          (unique-slot-mixin indexed-persistent-direct-slot-definition)
  ())



(defmethod clos:direct-slot-definition-class
           ((class persistent-class) &rest initargs &key persistent &allow-other-keys)
  (if persistent
      (case persistent
        (:unique  (find-class 'unique-persistent-direct-slot-definition))
        (:indexed (find-class 'indexed-persistent-direct-slot-definition))
        (t        (find-class 'persistent-direct-slot-definition)) )
    (call-next-method)))

(defmethod clos:process-a-slot-option 
           ((class persistent-class) option value
            already-processed-options slot)
  ;; Handle the :function option by adding it to the
  ;; list of processed options.
  ;; The slot is the user's list of slot options
  (if (eq option :indexed)
      (list* :indexed value already-processed-options)
    (call-next-method)))

;; -----------------------------------------------

(defmethod effective-slot-class (slotd)
  (declare (ignore slotd)))

;; -----------------------------------------------

(defclass persistent-effective-slot-definition
          (clos:standard-effective-slot-definition
           persistent-slot-definition)
  ())

(defmethod effective-slot-class ((slotd persistent-direct-slot-definition))
  (find-class 'persistent-effective-slot-definition))

;; ---------------

(defclass indexed-persistent-effective-slot-definition
          (indexed-slot-mixin persistent-effective-slot-definition)
  ())

(defmethod effective-slot-class ((slotd indexed-persistent-direct-slot-definition))
  (find-class 'indexed-persistent-effective-slot-definition))

;; ---------------

(defclass unique-persistent-effective-slot-definition
          (unique-slot-mixin indexed-persistent-effective-slot-definition)
  ())

(defmethod effective-slot-class ((slotd unique-persistent-direct-slot-definition))
  (find-class 'unique-persistent-effective-slot-definition))

;; ---------------

(defun locate-direct-slot (class slot-name)
  (nlet iter ((class class))
    (if-let (slotd (get-direct-slotd class slot-name))
        (return-from locate-direct-slot slotd)
      (dolist (super (clos:class-direct-superclasses class))
        (iter super)) 
      )))

(defmethod clos:effective-slot-definition-class
           ((class persistent-class) &rest initargs &key name &allow-other-keys)
  (or (effective-slot-class (locate-direct-slot class name))
      (call-next-method)))
    
(defmethod clos:compute-effective-slot-definition ((class persistent-class)
                                                   slot-name
                                                   direct-slot-definitions)
  (let ((effective-slotdef (call-next-method)))
    (when (typep effective-slotdef 'persistent-slot-definition)
      (dolist (slotd direct-slot-definitions)
        (when (typep slotd 'persistent-slot-definition)
          (setf (slot-value effective-slotdef 'persistence)
                (slot-value slotd 'persistence))
          (return))))
    effective-slotdef))

;; -----------------------------------------------------------

(defun get-slotd (class slot-name)
  (find slot-name (clos:class-slots class)
        :key 'clos:slot-definition-name))

;; -----------------------------------------------------------

(defmethod clos:slot-value-using-class ((class persistent-class) object slot-name)
  (if (and *dereference-slots*
             (let ((slotd (get-slotd class slot-name)))
               (typep slotd 'persistent-slot-definition)))
        (deref (call-next-method))
      (call-next-method)))

(defun basic-slot-value (object slot-name)
  (let ((*dereference-slots* nil))
    (slot-value object slot-name)))

;; -----------------------------------------------------------

(defmethod (setf clos:slot-value-using-class)
           (value (class persistent-class) object slot-name)
  (if *retrieving* ;; don't want to mark slots dirty during a retrieval
      (call-next-method)
    ;; else
    (let ((slotd (get-slotd class slot-name)))
      (if (typep slotd 'persistent-slot-definition)
          (let ((old-val (and (slot-boundp object slot-name)
                              (basic-slot-value object slot-name)))
                (new-val (ref value)))
            
            (cond ((or (same-ref new-val old-val)
                       (equal new-val old-val))
                   value)

                  (t (call-next-method new-val class object slot-name)
                     (if (is-indexed-slot slotd)
                         (mark-changed-index-slot object slot-name old-val)
                       (mark-dirty object))
                     value) ))
        ;; else
        (call-next-method)) )))

;; -----------------------------------------------------------

(defun get-old-key (item slot-name)
  (find-if #'(lambda (val)
               (and (eq :change-slot (first val))
                    (eq slot-name (second val))))
           (persistent-item-dirty item)))

(defun mark-changed-index-slot (obj slot-name old-val)
  (let ((item (persistent-object-item obj)))
    (cond ((is-deleted item))

          ;; not updated before? save the old value for the slot.
          ((not (get-old-key item slot-name))
           (push `(:change-slot ,slot-name ,old-val) (persistent-item-dirty item)))
          ;;
          ;; Otherwise, already marked as changed. No problem,
          ;; old value already recorded. Just overwrite the last change.
          )))

;; -----------------------------------------------------------

(defmethod pre-commit-for-object ((class persistent-class) item)
  (declare (ignore item))
  (labels ((mark-superclasses ()
             (let ((classes (clos:compute-class-precedence-list class)))
               (dolist (super (reverse (rest classes)))
                 (pre-commit-for-object super nil)))))
    (let ((actual (compute-slots-supers class)))
      (if (ignore-errors (deref (car (persistent-class-oid class)))) ;; get DB version, if any
          ;; calling DEREF on the OID forces a read if not already persisted
          ;; Once persisted, it just returns the object
          ;; If class not present, deref would produce error - we ignore
          (unless (same-def actual (deref (persistent-class-def-oid class)))
            (setf (persistent-class-def-oid class) (ref (persist actual)))
            (mark-dirty class)
            (mark-superclasses))
        (progn
          (setf (persistent-class-def-oid class) (ref (persist actual)))
          (persist-with-oid class (car (persistent-class-oid class)))
          (mark-superclasses))))
    ))

(defmethod pre-commit-for-object ((class persistent-kernel-class) item)
  (declare (class item)))

;; ---------

(defun same-def (lst1 lst2)
  (or (eq lst1 lst2)
      (ignore-errors
        (destructuring-bind (e-slot-names1 class-name1 supers1 d-slot-names1 d-slot-types1) lst1
          (destructuring-bind (e-slot-names2 class-name2 supers2 d-slot-names2 d-slot-types2) lst2
            (and (equal e-slot-names1 e-slot-names2)
                 (eq class-name1 class-name2)
                 (equal d-slot-names1 d-slot-names2)
                 (equal d-slot-types1 d-slot-types2)
                 (every 'same-ref supers1 supers2)) )))))
    
;; ---------

(defmethod is-storable-super (super)
  (declare (ignore super)))

(defmethod is-storable-super ((super persistent-class))
  super)

(defmethod is-storable-super ((super persistent-kernel-class))
  (declare (ignore super)))

;; ---------

(defmethod persist ((class persistent-class))
  class) ;; don't let the user decide this

;; ---------

(defmethod ref ((class persistent-kernel-class))
  ;; kernel classes just return their name
  (class-name class))

(defmethod ref ((class persistent-class))
  (car (persistent-class-oid class)))

;; ---------

(defun get-supers-list (class)
  (cons class
        (remove-if (complement 'is-storable-super)
                   (rest (clos:compute-class-precedence-list class)))))

(defun compute-slots-supers (class)
  (or-setf (persistent-class-def class)
           (let* ((d-slotds  (remove-if (complement 'is-persistent-slot)
                                        (clos:class-direct-slots class)))
                  ;; slot-names = a list of symbols
                  (d-slot-names (mapcar 'clos:slot-definition-name d-slotds))
                  
                  ;; slot-types = a list of :unique, :indexed, or nil
                  (d-slot-types (mapcar #'(lambda (slotd)
                                            (or (is-indexed-slot slotd) t))
                                        d-slotds))
                  
                  ;; a list of all effective slot-names
                  (e-slot-names (mapcar 'clos:slot-definition-name
                                        (remove-if (complement 'is-persistent-slot)
                                                   (clos:class-slots class))))
                  
                  ;; supers = a list of oid's
                  (supers     (mapcar 'ref (get-supers-list class))))
             
             `(,e-slot-names
               ,(class-name class)
               ,supers
               ,d-slot-names
               ,d-slot-types))))

;; ----------------------------------------------------------
;; Persistent Class validation and commit

(defun get-class-table ()
  (get-index-table +class-table-path+ :unique))

(defun record-class-in-direcctory (class-name oid)
  (let ((tbl (get-class-table)))
    (insert-row `(:key ,class-name :oid ,oid) tbl)))

#| ... what the heck??
(defmethod validate-object-commit :after ((class persistent-class) item)
  (declare (ignore item))
  (let ((tbl (get-class-table)))
    (when-let (row (fetch-row `(:key ,(class-name class)) tbl))
      (let ((obj (deref (getf row :oid))))
        (validate-commit (persistent-object-item obj)))
      )))
|#

(defmethod commit-object-changes :after ((class persistent-class) item)
  (record-class-in-direcctory (class-name class) (ref item)))

(defun get-persistent-classes ()
  (um:accum acc
    (map-rows (get-class-table)
              #'(lambda (row)
                  (acc `(:class ,(get-encoded-string-value (getf row :key))
                         :oid   ,(getf row :oid)))))
    ))

;; -------------------------------------------------------

#|
(defmacro def-persistent-class (name supers &rest body)
  ;; a macro to make new class definitions automatically inherit
  ;; behavior from the Persistent-class metaclass
  `(defclass ,name ,supers
     ,@body
     (:metaclass persistent-class)))
|#

#+:LISPWORKS
(editor:setup-indent "def-persistent-class" 2 2)

(defmethod make-item-for-rawbytes-object (item (obj persistent-class) deleted)
  (make-persistent-rawbytes-class-item
   :oid            (persistent-item-oid item)
   :object         (unless deleted
                     (make-rawbytes
                      :bytes (loenc:encode obj
                                           :prefix-length 4)))
   :dirty          (persistent-item-dirty item)
   :class-name     (class-name obj) ))

(defmethod commit-object-changes :after ((obj rawbytes) (item persistent-rawbytes-class-item))
  (record-class-in-direcctory (persistent-item-class-name item)
                              (persistent-item-oid item)))

;; -------------------------------------------------------

(defun get-persistent-class-info (class-name)
  (let* ((class   (get-persistent-class class-name))
         (indexes (persistent-class-indexed-slots class))
         (supers  (remove-if (complement (rcurry 'typep 'persistent-class))
                             (clos:class-direct-superclasses class)))
         (slotds  (remove-if (complement 'is-persistent-slot)
                             (clos:class-slots class))))
    (values class indexes supers slotds)))
  
(defun describe-persistence (class-name &optional (stream t))
  (multiple-value-bind (class indexes supers slotds)
      (get-persistent-class-info class-name)
    (declare (ignore class))
    (format stream "~&Class: ~A" class-name)
    (format stream "~&  Inherits Persistence: ~A"
            (mapcar 'class-name supers))
    (format stream "~&  Indexed Slots: ~{(~A ~S)~^ ~}"
            (loop for slotd in indexes
                  collect (clos:slot-definition-name (car slotd))
                  collect (is-indexed-slot (car slotd))))
    (format stream "~&  Slots:~{~&    ~A ~20,0TInitargs: ~S~^~}"
            (loop for slotd in slotds
                  collect (clos:slot-definition-name slotd)
                  collect (clos:slot-definition-initargs slotd))) ))
            
;; -------------------------------------------------------

(defun show-persistent-class-definition (class-name &optional (stream t))
  (multiple-value-bind (class indexes supers slotds)
      (get-persistent-class-info class-name)
    (declare (ignore indexes))
    (format stream
            "~&(defclass ~A ~A~%  (~{~A~^~%   ~})~%  (:metaclass ~A)~%  (:oid ~A))"
            class-name
            (mapcar 'class-name supers)
            (mapcar #'(lambda (slotd)
                        (let ((initargs  (clos:slot-definition-initargs slotd))
                              (slot-name (clos:slot-definition-name slotd))
                              (indexed   (is-indexed-slot slotd)))
                          (with-output-to-string (s)
                            (format s "(~A" slot-name)
                            (when indexed
                              (format s "  :indexed ~S"
                                      (or (eq indexed :indexed)
                                          indexed)))
                            (dolist (initarg initargs)
                              (format s "  :initarg ~S" initarg))
                            (format s ")")) ))
                    slotds)
            (class-name (class-of class))
            (car (persistent-class-oid class))) ))
