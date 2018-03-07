;; persist.lisp -- persistent objects
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

;; -------------------------------

(defun is-persistent (obj)
  (get-key-value obj (obj->item-mappings)))

(defun persistent-object-item (obj)
  (or (is-persistent obj)
      (error "Object ~A not persistent" obj)))

(defun persistent-item-for-oid (oid)
  (get-key-value (oid-hashkey oid) (oid->item-mappings)))

;; -------------------------------

(defun is-deleted (item)
  (let ((dirty (persistent-item-dirty item)))
    (find :deleted dirty :key 'first)))

(defun deleted-item-object (item)
  (second (is-deleted item)))

;; -------------------------------

(defmethod oid-for-object (obj)
  (when-let (item (is-persistent obj))
    (persistent-item-oid item)))

;; -------------------------------

(defun get-new-ts ()
  (uuid:copy-v1-uuid-replacing-time
   (+ *ts-offset* (uuid:uuid-time (make-uuid)))
   (tls-archetype-uuid (get-tls))))

(defun merge-ts-with-mac (ts-for-time)
  (uuid:copy-v1-uuid-replacing-time
   (uuid:uuid-time ts-for-time)
   (tls-archetype-uuid (get-tls))))
  
(defun get-new-host-ts ()
  (make-uuid))

(defun ts< (a b)
  (< (uuid:uuid-time a)
     (uuid:uuid-time b)))

(defmethod when-created ((uuid uuid:uuid))
  (multiple-value-bind (utc frac)
      (uuid:uuid-to-universal-time uuid)
    (multiple-value-bind (ss mm hh d m y)
        (decode-universal-time utc 0)
      (let ((mac (uuid:uuid-mac uuid)))
        (format nil
                "~4D~{-~2,'0D~} ~{~2,'0D~^\:~}.~7,'0D UTC ~{~2,'0X~^\:~}"
                y (list m d) (list hh mm ss)
                frac
                (list (ldb (byte 8 40) mac)
                      (ldb (byte 8 32) mac)
                      (ldb (byte 8 24) mac)
                      (ldb (byte 8 16) mac)
                      (ldb (byte 8  8) mac)
                      (ldb (byte 8  0) mac)) )))))

;; -------------------------------

(defmethod persist (obj)
  (assert *current-connection*)
  (when obj
    (unless (is-persistent obj)
      (internal-persist obj) ))
  obj)

(defmethod internal-persist (obj)
  (persist-with-oid obj (get-new-oid)))

(defun persist-with-oid (obj oid)
  ;; creating a new item
  (intern-persistent-item (make-persistent-item
                           :oid    oid
                           :object obj
                           :dirty  '((:new-object))) ))

(defun intern-persistent-item (item)
  (let ((oid (persistent-item-oid item))
        (obj (persistent-item-object item)))
    (setf (get-key-value obj (obj->item-mappings)) item
          (get-key-value (oid-hashkey oid) (oid->item-mappings)) item)))

;; -------------------------------
;; Lookup a named object from some class.
;; Look first in the transaction cache,
;; else lookup in the database.

(defun locate-in-cache (item class-name &key (test 'equal) (key 'identity))
  (map-keys-values #'(lambda (k v)
                       (if (and (typep k class-name)
                                (funcall test item (funcall key k)))
                           (return-from locate-in-cache (persistent-item-oid v))))
                   (obj->item-mappings)))

(defun locate-indexed-name (name class-name key-fn)
  (locate-in-cache (encode-object-to-string name) class-name
                   :key (compose 'encode-object-to-string key-fn)))

(defun find-indexed-object (name class-name key-fn)
  (or (locate-indexed-name name class-name key-fn)
      (car (fetch-instances-for-slot class-name 'name name))))

;; -------------------------------

(defun delete-persistent-object (obj)
  (when-let (item (is-persistent obj))
    (remove-key-value obj (obj->item-mappings))
    (setf (persistent-item-object item) nil
          (persistent-item-dirty  item) `((:deleted ,obj)))
    )
  obj)

(defun delete-oid (oid)
  (if-let (item (persistent-item-for-oid oid))
      (delete-persistent-object (persistent-item-object item))
    (setf (get-key-value (oid-hashkey oid) (oid->item-mappings))
          (make-persistent-item
           :oid   oid
           :obj   nil
           :dirty `((:deleted nil)))) ))

;; -------------------------------

(defmethod get-persistent-object ((oid oid) &optional (nprev 0))
  (if-let (item (and (not (minusp nprev))
                        (get-key-value (oid-hashkey oid) (oid->item-mappings))))
      (persistent-item-object item)
    ;; else
    (if-remote
        (remote-fetch-from-logfile oid nprev)
      (fetch-from-logfile oid nprev))))

(defun remote-fetch-from-logfile (oid nprev)
  (optima:match (rpc `(remote-fetch-bytes-from-logfile ,oid ,nprev))
    ( (list :RAW-FETCH bytes)
      (let ((obj (loenc:decode bytes)))
        (when (zerop nprev)
          (intern-persistent-item (make-persistent-item
                                   :oid    oid
                                   :object obj)))
        (cond ((or (typep obj 'ok-set)
                   (typep obj 'ok-map)
                   (typep obj 'ok-schema))
               (rpc `(fetch-from-logfile-then-ok ,oid ,nprev)))

              ((typep obj 'ok-table)
               ;; must do the fixup here, and not in after-retrieve
               ;; becuase if nprev <> 0 then we won't be persistent
               ;; and there will be no way to ask for a server copy
               ;; without an OID.
               (setf (table-sequence obj)
                     (rpc `(fetch-ok-table-from-logfile ,oid ,nprev))))
              )
        obj))
    ;; otherwise - must be an object
    ( obj  obj) ))

(defun fetch-from-logfile-then-ok (oid nprev)
  (fetch-from-logfile oid nprev)
  :OK)

(defun fetch-ok-table-from-logfile (oid nprev)
  (let ((obj (fetch-from-logfile oid nprev)))
    (table-sequence obj)))

;; -------------------------------
#| ;; like Smalltalk's BECOME operator - unused

(defun replace-persistent-object (old-obj new-obj)
  (if-let (item (is-persistent old-obj))
      (progn
        (remove-key-value old-obj (obj->item-mappings))
        (setf (persistent-item-object item) new-obj)
        (when new-obj
          (setf (get-key-value new-obj (obj->item-mappings)) item)
          (mark-dirty new-obj))
        new-obj)
    ;; else
    (error "Not a persistent object")
    ))
|#

;; -------------------------------

(defun mark-dirty (obj)
  (when-let (item (is-persistent obj))
    (setf (persistent-item-dirty item) (or (persistent-item-dirty item)
                                           `((:dirty))))
    ))

;; -------------------------------

(defun discard-persistent-object (obj)
  (when-let (item (is-persistent obj))
    (remove-key-value obj (obj->item-mappings))
    (remove-key-value (oid-hashkey (persistent-item-oid item)) (oid->item-mappings)) )
  obj)

;; -------------------------------
;; -----------------------------------------------------------
;; References to persistent data items

(defmethod ref (obj)
  (if-let (item (is-persistent obj))
      (persistent-item-oid item)
    obj))

(defmethod ref ((item persistent-item))
  (persistent-item-oid item))



(defmethod deref (obj)
  obj)

(defun map-deref (lst)
  (mapcar 'deref lst))

(defmethod same-ref (a b)
  (eql a b))



(defmethod valid-ref? (obj)
  nil)

;; -----------------------------------------------------------
;; Access to persistent arrays

(defmethod paref ((arr array) &rest indices)
  (if *dereference-slots*
      (deref (apply 'aref arr indices))
    (apply 'aref arr indices)))

(defmethod (setf paref) (value (arr array) &rest indices)
  (prog1
      (setf (apply #'aref arr indices) (ref value))
    (mark-dirty arr)))

;; -----------------------------------------------------------
;; Easy make for persistent items

(defun make-persistent (obj)
  (ref (persist obj)))

(defun make-persistent-instance (class-name &rest args)
  (make-persistent (apply 'make-instance class-name args)))

;; -----------------------------------------------------------
;; Access to persistent instances

(defun pslot-value (obj slot)
  (slot-value (deref obj) slot))

(defun (setf pslot-value) (val obj slot)
  (setf (slot-value (deref obj) slot) val))

