;; oid-class.lisp
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

(defclass oid ()
  ;; creation is controlled, only get-new-oid can make one...
  ((uuid :reader   oid-uuid
         :initarg  :uuid
         :initform (make-uuid))
   ))

(defmethod make-load-form ((oid oid) &optional environment)
  (declare (ignore environment))
  `(make-oid-from-string ,(format nil "~S" (oid-uuid oid))))

(um:set-/-dispatch-reader "oid"
                          (let ((fn (um:get-/-dispatch-reader "uuid")))
                            #'(lambda (stream)
                              (make-instance 'oid
                                             :uuid (funcall fn stream)) )))

(um:set-$-dispatch-reader :oid
                          #'(lambda (sym)
                            ;; symbol or string acceptable
                            (make-oid-from-string (string sym))))

(defun make-oid-from-string (uuid-str)
  (make-instance 'oid :uuid (uuid:make-uuid-from-string uuid-str)))

(defun make-oid (&rest args)
  (apply 'make-instance 'oid args))

(defmethod oid-p (obj)
  (declare (ignore obj)))

(defmethod oid-p ((oid oid))
  t)

(defun get-new-oid ()
  (make-instance 'oid))

(defmethod deref ((oid oid))
  (get-persistent-object oid))

(defmethod same-ref ((a oid) (b oid))
  (oid= a b))

(defmethod oid< (a b)
  (uuid:uuid< (oid-uuid a) (oid-uuid b)))

(defmethod oid= (a b)
  (uuid:uuid= (oid-uuid a) (oid-uuid b)))

(defun ref< (a b)
  (oid< a b))

(defmethod compare-oid ((oid1 oid) (oid2 oid))
  (uuid:compare-uuid (oid-uuid oid1) (oid-uuid oid2)))

(defmethod oid-hashkey ((oid oid))
  (uuid:uuid-to-integer (oid-uuid oid)))

(defmethod when-created ((oid oid))
  (when-created (oid-uuid oid)))

(defmethod print-object ((oid oid) stream)
  (format stream (if *print-readably*
                     "#$(OID ~A)"
                   "~A")
          (oid-uuid oid)))

(defun oid (uuid-str)
  ;; symbol or string acceptable
  (make-oid-from-string (string uuid-str)))

(defmethod oid-for-object ((oid oid))
  oid)

(defmethod persist ((oid oid))
  oid)

(defmethod valid-ref? ((oid oid))
  (or (get-key-value (oid-hashkey oid) (oid->item-mappings))
      (valid-oid? oid)))

;; -------------------------------------------

(defclass ioid ()
  ((oid  :accessor ioid-oid
         :initarg  :oid)))

(defmethod deref ((ioid ioid))
  (let ((obj (deref (ioid-oid ioid))))
    (if (same-ref ioid obj)
        (ioid-oid ioid)
      (deref obj))))

(defmethod same-ref ((a ioid) (b ioid))
  (same-ref (ioid-oid a) (ioid-oid b)))

