;; ok-sets.lisp -- Persistent Sets
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

(defclass ok-set (ok-set-in-memory persistent-kernel-object)
  ;; the persistent portion
  ((name      :reader     ok-set-name
              :allocation :persistent
              :indexed    :unique
              :initarg    :name
              :initform   (mkstr (make-uuid)))
   
   (btree-pos :reader     ok-set-btree-pos
              :allocation :persistent
              :initform   nil))
  (:metaclass persistent-kernel-class)
  (:oid       #/oid/{00000000-0000-0000-0000-000000000001}))

(defclass ok-set-in-memory ()
  ((btree     :reader     ok-set-btree
              :initform   nil)
   (additions :accessor   ok-set-additions
              :initform   (make-hash-table :test 'equal))
   (deletions :accessor   ok-set-deletions
              :initform   (make-hash-table :test 'equal))
   (set-lock  :accessor   ok-set-lock
              :initform   (mpcompat:make-lock :name "OK-Set"))
   ))

(defmethod initialize-instance :after ((obj ok-set) &rest args
                                       &key &allow-other-keys)
  (when-remote
    ;; need to give the server a chance to tell us that the object's OID
    ;; needs to be changed to match an older existing object, when we
    ;; have the same primary index key value.
    (apply 'ask-server-to-instantiate-copy obj args)))
  
;; -------------------------------------------

(defmacro with-locked-set ((set) &body body)
  `(mpcompat:with-lock ((ok-set-lock ,set))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-locked-set" 1)

;; -------------------------------------------

(defun make-ok-set-key (obj)
  (make-encoded-string-from-object obj))

;; -------------------------------------------

(defun make-ok-set-btree (name pos)
  (make-file-btree name pos
                   :compare 'ord:compare ;; 'encoded-string-compare
                   :key     'fetch-encoded-object-for-btree ;; 'fetch-string
                   ))

(defun fetch-encoded-object-for-btree (pkey)
  (make-encoded-string
   :s (fetch-string pkey (mmf:pointer-address pkey))))

(defmethod sdle-store:after-retrieve :after ((obj ok-set))
  (unless-remote
    (setf (slot-value obj 'btree) (make-ok-set-btree (ok-set-name obj)
                                                     (ok-set-btree-pos obj)))))

;; -------------------------------------------

(defun allocate-ok-set-btree (obj)
  (let* ((pos   (alloc (mmf:size-of 'btree_t) +tree-header+))
         (btree (make-ok-set-btree (ok-set-name obj) pos)))
    (initialize-file-btree btree)
    (setf-slots obj
                'btree-pos  pos
                'btree      btree)))

#|
(defmethod commit-object-deletion :after ((obj ok-set) item)
  (declare (ignore item))
  (when (ok-set-btree obj)
    (discard (ok-set-btree-pos obj))))
|#

(defconstant +log-set-add+    (char-code #\S))
(defconstant +log-set-delete+ (char-code #\T))

(defun wr-ok-log (pref oid obj)
  (let ((v-oid (loenc:encode oid :prefix-length 1))
        (v-enc (loenc:encode obj :prefix-length 4)))
    (write-byte pref *current-commit-logfile*)
    (write-sequence v-oid *current-commit-logfile*)
    (write-sequence v-enc *current-commit-logfile*) ))

(defmethod commit-object-changes ((obj ok-set) item)
  (unless (ok-set-btree obj)
    (allocate-ok-set-btree obj))
  
  (let ((btree (ok-set-btree obj))
        (oid   (persistent-item-oid item)))
    
    (if (find :add/remove (persistent-item-dirty item) :key 'first)
        (update-wts oid)
      (call-next-method))
    
    (map-keys-values #'(lambda (k v)
                         (btree:delete-item btree v)
                         (wr-ok-log +log-set-delete+ oid k))
                     (ok-set-deletions obj))
    (clear-keys-values (ok-set-deletions obj))
    
    (map-keys-values #'(lambda (k v)
                         (let ((poff (string-pool-offset k)))
                           (btree:insert-item btree v poff)
                           (wr-ok-log +log-set-add+ oid k)))
                     (ok-set-additions obj))
    (clear-keys-values (ok-set-additions obj))
    ))

(defun mark-me-add/remove (obj)
  (let ((me (persistent-object-item obj)))
    (unless (persistent-item-dirty me)
      (setf (persistent-item-dirty me) `((:add/remove))))
    ))

;; ----------------------------------------------------------------

(defmethod set-member-p ((obj ok-set) val)
  (with-read-lock ()
    (with-locked-set (obj)
      (let* ((key    (make-ok-set-key val))
             (keystr (encoded-string-s key)))
        (unless (get-key-value keystr (ok-set-deletions obj))
          (or (get-key-value keystr (ok-set-additions obj))
              (if-remote
                  (rpc `(set-member-p-oid ,(oid-for-object obj) ,val))
                ;; else
                (when-let (btree (ok-set-btree obj))
                  (multiple-value-bind (ptr found-it)
                      (btree:find-item btree key)
                    (declare (ignore ptr))
                    found-it))
                )))))))

(defmethod set-member-p-oid ((oid oid) val)
  (set-member-p (get-persistent-object oid) val))

;; -------------------------------------------

(defmethod add-to-set ((obj ok-set) val)
  (with-read-lock ()
    (with-locked-set (obj)
      (let* ((key    (make-ok-set-key val))
             (keystr (encoded-string-s key)))
        (remove-key-value keystr (ok-set-deletions obj))
        (setf (get-key-value keystr (ok-set-additions obj)) key)
        (mark-me-add/remove obj)
        val))))

;; -------------------------------------------

(defmethod remove-from-set ((obj ok-set) val)
  (with-read-lock ()
    (with-locked-set (obj)
      (let* ((key    (make-ok-set-key val))
             (keystr (encoded-string-s key)))
        (remove-key-value keystr (ok-set-additions obj))
        (setf (get-key-value keystr (ok-set-deletions obj)) key)
        (mark-me-add/remove obj)
        val))))

;; -------------------------------------------

(defmethod map-set ((obj ok-set) fn &key max-records)
  (with-read-lock ()
    (with-locked-set (obj)
      (if-remote
          (dolist (str (rpc `(collect-set-values-for-oid ,(oid-for-object obj)
                                                         ,max-records)))
            (unless (get-key-value str (ok-set-deletions obj))
              (funcall fn (decode-string-to-object str)) ))
        ;; else
      (when-let (btree (ok-set-btree obj))
        (btree:map-tree btree
                        #'(lambda (ptr)
                            (let ((str (fetch-string ptr)))
                              (unless (get-key-value str (ok-set-deletions obj))
                                (funcall fn (decode-string-to-object str)))
                              ))
                        :max-records max-records)))
      (map-keys-values #'(lambda (k v)
                           (declare (ignore k))
                           (funcall fn (get-encoded-string-value v)))
                       (ok-set-additions obj)) )))

(defmethod collect-set-values-for-oid ((oid oid) max-records)
  (let ((obj  (get-persistent-object oid)))
    (um:accum acc
      (when-let (btree (ok-set-btree obj))
        (btree:map-tree btree (um:compose #'acc #'fetch-string)
                        :max-records max-records))
      )))

(defmacro doset ((var obj) &body body)
  `(map-set ,obj #'(lambda (,var) ,@body)))

;; ------------------------------------------

(defmethod set-count ((obj ok-set))
  ;; should rollback to get accurate count
  ;; some deletions may be attempting to delete elements
  ;; that aren't in the set.
  (with-read-lock ()
    (with-locked-set (obj)
      (+ (if-remote
          (rpc `(set-count-for-oid ,(oid-for-object obj)))
          (if-let (btree (ok-set-btree obj))
              (btree:items-count btree)
            0))
         (- (key-value-count (ok-set-additions obj))
            (key-value-count (ok-set-deletions obj))) ))))

(defmethod set-count-for-oid ((oid oid))
  (set-count (get-persistent-object oid)))

;; -------------------------------------------

(defun find-ok-set (name)
  ;; because we are indexed...
  (find-indexed-object name 'ok-set 'ok-set-name))

(defmethod query ((set ok-set) selection &optional pred)
  (perform 'query
           (if-remote
            set
            (ok-set-btree set))
           selection
           pred))

;; -------------------------------------------

(defmethod print-object ((obj ok-set) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "Name: ~A" (ok-set-name obj)) ))

;; -------------------------------------------

(defmethod update-from-remote ((obj ok-set) adds deletes)
  (clear-keys-values (ok-set-additions obj))
  (clear-keys-values (ok-set-deletions obj))
  (dolist (add adds)
    (setf (get-key-value add (ok-set-additions obj)) (make-encoded-string :s add)))
  (dolist (del deletes)
    (setf (get-key-value del (ok-set-deletions obj)) (make-encoded-string :s del)))
  (when (or adds deletes)
    (mark-me-add/remove obj)) )

(defun collect-ok-set-vals (tbl)
  (let ((items nil))
    (map-keys-values #'(lambda (k v)
                       (declare (ignore v))
                       (push k items))
                     tbl)
    items))

(defmethod get-additions ((obj ok-set))
  (collect-ok-set-vals (ok-set-additions obj)))

(defmethod get-deletions ((obj ok-set))
  (collect-ok-set-vals (ok-set-deletions obj)))
