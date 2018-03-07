;; serialization.lisp -- Database Object Serialization/Deserialization
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

;; -----------------------------------------------------------
;; Persistent data serialization / deserialization

(defparameter +persistent-class-code+  (loenc:register-code 101 'persistent-class))
(defparameter +persistent-object-code+ (loenc:register-code 102 'persistent-object))
(defparameter +oid-code+               (loenc:register-code 103 'oid))
(defparameter +ioid-code+              (loenc:register-code 104 'ioid))
(defparameter +built-in-object-code+   (loenc:register-code 105 'persistent-kernel-object))

;; -----------------------------------------------------------
;; Persistent Slotdef Details

(defmethod sdle-store:get-slot-details ((slotd persistent-slot-definition))
  (nconc (call-next-method)
         (list :persistent (or (is-indexed-slot slotd) t)) ;; :unique, :indexed, or t
         ))

;; -----------------------------------------------------------
;; 

(defun do-with-saved-file-position (fn stream)
  (let ((here (file-position stream)))
    (hcl:unwind-protect-blocking-interrupts-in-cleanups
        (funcall fn)
      (file-position stream here))
    ))

(defmacro with-saved-file-position ((stream) &body body)
  `(do-with-saved-file-position
    (deferred
      ,@body)
    ,stream))

#+:LISPWORKS
(editor:setup-indent "with-saved-file-position" 1)

;; -----------------------------------------------------------
;; Persistent Object Instances

(loenc:defstore (obj persistent-object stream)
  (bind*
      ((class      (class-of obj))
       (def-oid    (persistent-class-def-oid class)) ;; a ref cell
       (slot-names (car (deref def-oid))))
    
    (loenc:store-count +persistent-object-code+ stream)
    (loenc:store-object def-oid stream) ;; an oid
    
    (dolist (slot-name slot-names)
      (if (slot-boundp obj slot-name)
          ;; ensure that slots containing persistent objects become refs
          (loenc:store-object (ref (basic-slot-value obj slot-name)) stream)
        (loenc:store-object sdle-store:$unbound-marker stream))
      )))

(loenc:defrestore (persistent-object stream)
  (let* ((def-oid (loenc:restore-object stream)) ;; an oid
         class
         slot-names)

    (let (;; (sdle-store:*nuke-existing-classes* t) ;; disallow - fails to allow evolution
          ;; #+:LISPWORKS (dspec:*redefinition-action* nil)
          )
      (with-saved-file-position (stream)
          (destructuring-bind (e-slot-names class-name supers &rest ignored)
              (deref def-oid)
            (declare (ignore ignored))
            (setf class (or (find-class class-name nil)
                            (deref (car supers)))
                  slot-names e-slot-names))))
      
    ;; resident kernel classes provide only their name
    (let ((new-instance (allocate-instance (if (symbolp class)
                                               (find-class class)
                                             class))))
      (dolist (slot-name slot-names)
        (let ((val (loenc:restore-object stream)))
          (when (clos:slot-exists-p new-instance slot-name)
            (unless (eq val sdle-store:$unbound-marker)
              (let ((*retrieving* t))
                (setf (slot-value new-instance slot-name) val))))))

      (with-saved-file-position (stream)
        (after-retrieve new-instance))
      new-instance)))

;; -----------------------------------------------------------
;; Persistent Kernel Object Instances

(loenc:defstore (obj persistent-kernel-object stream)
  (bind*
      ((class      (class-of obj))
       (slotds     (remove-if (complement 'is-persistent-slot) (clos:class-slots class))))
    
    (loenc:store-count +built-in-object-code+ stream)
    (loenc:store-object (type-of obj) stream)
    (dolist (slotd slotds)
      (let ((slot-name (clos:slot-definition-name slotd)))
        (if (slot-boundp obj slot-name)
            ;; ensure that slots containing persistent objects become refs
            (loenc:store-object (ref (basic-slot-value obj slot-name)) stream)
          (loenc:store-object sdle-store:$unbound-marker stream))
        ))))

(loenc:defrestore (persistent-kernel-object stream)
  (let* ((class-name (loenc:restore-object stream)) ;; a symbol
         (class      (find-class class-name))
         (slotds     (remove-if (complement 'is-persistent-slot) (clos:class-slots class)))
         (obj        (allocate-instance class)))
    (dolist (slotd slotds)
      (let ((slot-name (clos:slot-definition-name slotd))
            (val       (loenc:restore-object stream)))
        (unless (eql val sdle-store:$unbound-marker)
          (let ((*retrieving* t))
            (setf (slot-value obj slot-name) val))) ))
    (with-saved-file-position (stream)
      (after-retrieve obj))
    obj))

;; -----------------------------------------------------------
;; Persistent Class Instances

(loenc:defstore (obj persistent-class stream)
  (loenc:store-count +persistent-class-code+ stream)
  (loenc:store-object (persistent-class-def-oid obj) stream)) ;; a ref

(loenc:defrestore (persistent-class stream)
  (let ((def-oid (loenc:restore-object stream)))
    (with-saved-file-position (stream)
        (destructuring-bind (e-slot-names class-name supers d-slot-names d-slot-types)
            (deref def-oid)
          (declare (ignore e-slot-names))

          ;; e-slot-names = a list of effective slot names when the class was persisted
          ;; class-name = a symbol
          ;; supers = a list of oid's with this class first
          ;; d-slot-names = a list of symbols denoting direct slots
          ;; d-slot-types = a list of :unique, :indexed, or t
          
          (dolist (super (reverse (rest supers))) ;; instantiate all superclasses first
            (deref super))
          
          (let ((class (or (find-class class-name nil)
                           (let ((class
                                  (clos:ensure-class class-name
                                   :direct-superclasses (list
                                                         (if-let (super (cadr supers))
                                                             (if (symbolp super)
                                                                 (find-class super)
                                                               (lookup-registered-class super))
                                                           (find-class 'persistent-object)))
                                   :metaclass 'persistent-class
                                   :oid       (list
                                               (if (symbolp (car supers))
                                                   (persistent-class-oid (find-class (car supers)))
                                                 (car supers))))))

                             (setf (clos:class-direct-slots class)
                                   (loop for slot-name in d-slot-names
                                         for slot-type in d-slot-types
                                         collect
                                         (make-instance
                                          (case slot-type
                                            (:unique  'unique-persistent-direct-slot-definition)
                                            (:indexed 'indexed-persistent-direct-slot-definition)
                                            (t        'persistent-direct-slot-definition))
                                          :name        slot-name
                                          :allocation  :instance
                                          :initargs    (list (intern (string slot-name) 'keyword))
                                          :persistent  slot-type)))                             
                             
                             (after-retrieve class)
                             class))))
            (setf (persistent-class-def-oid class) def-oid)
            class) ))))

#| --------------------------------------------------------------------------------
;; test out how MOP works...
(setf x
      (let* ((class-name   'junk5)
             (d-slot-names '(a b c))
             (d-slot-types '(:unique :indexed t))
             (supers       '(#/oid/{F2E79A8A-8568-11DE-81FE-00254BAF81A0}))
             (class        (clos:ensure-class
                            class-name
                            :direct-superclasses (list
                                                  (if-let (super (cadr supers))
                                                      (if (symbolp super)
                                                          (find-class super)
                                                        (lookup-registered-class super))
                                                    (find-class 'persistent-object)))
                            :metaclass 'persistent-class
                            :oid (list
                                  (if (symbolp (car supers))
                                      (persistent-class-oid (find-class (car supers)))
                                    (car supers))))))
        (setf (clos:class-direct-slots class)
              (loop for slot-name in d-slot-names
                    for slot-type in d-slot-types
                    collect
                    (make-instance
                     (case slot-type
                       (:unique  'unique-persistent-direct-slot-definition)
                       (:indexed 'indexed-persistent-direct-slot-definition)
                       (t        'persistent-direct-slot-definition))
                     :name        slot-name
                     :allocation  :instance
                     :initargs    (list (intern (string slot-name) 'keyword))
                     :readers     `(,slot-name)
                     :writers     `((SETF ,slot-name))
                     ;; :initform    (list nil)
                     :persistent  slot-type)))
        class))
|#
;; -----------------------------------------------------------
;; OID Objects

(loenc:defstore (obj oid stream)
  (loenc:store-count +oid-code+ stream)
  (write-raw-uuid (oid-uuid obj) stream))

(loenc:defrestore (oid stream)
  (make-instance 'oid
                 :uuid (read-raw-uuid stream)))

;; -----------------------------------------------------------

(defun write-raw-uuid (uuid stream)
  (let ((bytes (uuid:uuid-to-byte-array uuid)))
    (write-sequence bytes stream)))

(defun read-raw-uuid (stream)
  (let ((bytes (make-array 16 :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream)
    (uuid:byte-array-to-uuid bytes)))

;; -----------------------------------------------------------
;; IOID Objects

(loenc:defstore (obj ioid stream)
  (loenc:store-count +ioid-code+ stream)
  (write-raw-uuid (oid-uuid (ioid-oid obj)) stream))

(loenc:defrestore (ioid stream)
  (make-instance 'ioid
                 :oid (make-instance 'oid
                                     :uuid (read-raw-uuid stream))))

;; -----------------------------------------------------------
