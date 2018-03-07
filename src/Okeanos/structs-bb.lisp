;; structs.lisp
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

;; -------------------------------------------
;; database -- the grand struct that represents
;; the cached state of an open okeanos file.

(defclass database ()
  ;; r/w locks for this database
  ((locks        :accessor database-locks
                 :initform (make-ld-state))
   ;; directory name for this database
   (dirname      :accessor database-dirname
                 :initarg  :dirname)
   ;; uuid = unique ID for this database in this session
   (uuid         :accessor database-uuid
                 :initarg  :uuid)
   ))

(defmethod is-file-database ((db database)))
(defmethod is-remote-database ((db database)))

(defclass file-database (database)
  ;; refc = nbr of active opens on this file database
  ((refc             :accessor database-refc
                     :initform 1)
   ;; used as the heap access mapper
   (mapper           :accessor database-mapper
                     :initarg  :mapper)
   ;; transaction directory
   (trans-dir        :accessor database-trans-dir
                     :initform nil)
   ;; users directory
   (users-dir        :accessor database-users-dir
                     :initform nil)
   ;; main OID mappings table
   (oid-mappings     :accessor database-oid-mappings
                     :initform nil)
   ;; caches for table objects
   (table-caches     :accessor database-table-caches
                     :initform (make-hash-table))
   ;; list of logfiles
   (files-list       :accessor database-files-list
                     :initform (make-array 0 :adjustable t :fill-pointer t))
   ))

(defmethod is-file-database ((db file-database))
  t)

(defclass remote-database (database)
  ;; the pid of the remote client-proxy
  ((server    :accessor database-server
              :initarg  :server)
   ))

(defmethod is-remote-database ((db remote-database))
  t)
   
(defmacro with-database (db &body body)
  `(do-with-database ,db (deferred ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-database" 1)

(defun do-with-database (db fn)
  (let ((*current-okeanos-db* db))
    (funcall fn)))

;; -------------------------------

(defun do-databases (fn &optional (databases nil databases-p))
  (dolist (triple (or databases
                      (and (not databases-p)
                           (cnx-info-mappings *current-connection*))))
    (funcall fn triple)))

(defmacro! with-databases ((&optional databases) &body body)
  `(do-databases #'(lambda (,g!triple)
                   (with-database (car ,g!triple)
                     ,@body))
                 ,@(when databases
                     `(,databases))))

#+:LISPWORKS
(editor:setup-indent "with-databases" 1)

;; -------------------------------

(defstruct cnx-info  ;; per-thread, per-DB hashtables
  db-state           ;; current global transaction WTS for thread
  mappings)          ;; objects are the same under EQL (database oid->item . obj->item)

;; -------------------------------

(defun oid->item-mappings ()
  (cadr (ensure-mappings)))

(defun obj->item-mappings ()
  (cddr (ensure-mappings)))

(defun ensure-mappings ()
   (ensure-assoc (*current-okeanos-db* (cnx-info-mappings *current-connection*))
     (cons (make-hash-table)
           (make-hash-table))))

;; ----------------------------------------------------------------

(defun current-db-state ()
  (cnx-info-db-state *current-connection*))

;; ----------------------------------------------------------------

(defun oid-mappings ()
  (or-setf (database-oid-mappings *current-okeanos-db*)
           (find-file-table +oid-mappings-path+)))

(defun user-mappings ()
  (or-setf (database-users-dir *current-okeanos-db*)
           (find-file-table +user-mappings-path+)))

(defun transaction-directory ()
  (or-setf (database-trans-dir *current-okeanos-db*)
           (find-file-table +transaction-directory-path+)))

;; -------------------------------------------
;; Thread-local storage

(defstruct tls
  timer          ;; lock lease timer
  timer-refc     ;; lock nesting level
  archetype-uuid ;; uuid from user, local or remote, contains MAC address
  cnx-info       ;; used by atomic transactions
  user-id        ;; logname@hostname
  user-oid       ;; assigned first time we see a new user-id
  needs-user-registration) ;; true when new user-oid has been assigned

(defun get-tls-var (name)
  (get-pid-property name))

(defun set-tls-var (name val)
  (setf (get-pid-property name) val))

(defsetf get-tls-var (name) (val)
  `(set-tls-var ,name ,val))

(defun get-tls ()
  (or-setf (get-tls-var 'tls)
           (make-tls
            :archetype-uuid (make-uuid)
            :cnx-info       (make-cnx-info)
            :user-id        (get-user-id-from-system) )))

(defun get-tls-info ()
  (let ((tls (get-tls)))
    (list :archetype-uuid (tls-archetype-uuid tls)
          :cnx-info       (cnx-info-db-state (tls-cnx-info tls))
          :user-id        (tls-user-id tls)
          :user-oid       (tls-user-oid tls)
          :needs-user-registration (tls-needs-user-registration tls))))

(defun get-user-id-from-system ()
  (let* ((s (with-output-to-string (s)
             (sys:call-system-showing-output "logname"
                                             :output-stream s)
             (sys:call-system-showing-output "hostname"
                                             :output-stream s)))
         (items (split-string s :delims '(#\space #\; #\newline))))
    (mkstr (second items) #\@ (fourth items))))

(defun get-user-id ()
  (let ((tls (get-tls)))
    (or-setf (tls-user-id tls)
             (get-user-id-from-system))))

(defun get-user-oid ()
  (let ((tls (get-tls)))
    (cdr
     (ensure-assoc (*current-okeanos-db* (tls-user-oid tls))
       (let ((user-id (get-user-id)))
         (if-let (row (fetch-row `(:user-id ,user-id) (user-mappings)))
             (getf row :user-oid)
           (let ((user-oid (make-oid :uuid (tls-archetype-uuid tls))))
             (persist-with-oid user-id user-oid)
             (push *current-okeanos-db* (tls-needs-user-registration tls))
             user-oid) )) ))))

;; -----------------------------------------------------

(defmethod get-key-value (key (table hash-table) &optional default)
  (gethash key table default))

(defmethod (setf get-key-value) (val key (table hash-table))
  (setf (gethash key table) val))

(defmethod remove-key-value (key (table hash-table))
  (remhash key table))

(defmethod clear-keys-values ((table hash-table))
  (clrhash table))

(defmethod key-value-count ((table hash-table))
  (hash-table-count table))

(defmethod map-keys-values (fn (table hash-table))
  (maphash fn table))

;; -----------------------------------------------------

(defstruct table-cache
  (cache (make-instance 'um:2-way-cache
                        :nlines 1))
  (lock  (mpcompat:make-lock :name "File-Table-Cache")))

(defmacro with-locked-cache ((cache) &body body)
  `(mpcompat:with-lock ((table-cache-lock ,cache))
     ,@body))

#+:LISPWORKS
(editor:setup-indent "with-locked-cache" 1)

#|
;; not thread safe... (does it need to be?)
(defun allocate-table-cache (ptr)
  (let ((tbl  (database-table-caches *current-okeanos-db*))
        (addr (mmf:pointer-address ptr)))
    (or-setf (get-key-value addr tbl)
             (make-table-cache))))
|#

(defun allocate-table-cache (ptr)
  (let ((tbl  (database-table-caches *current-okeanos-db*))
        (addr (mmf:pointer-address ptr)))
    (hcl:gethash-ensuring addr tbl #'make-table-cache) ))

;; -----------------------------------------------------

(defun remote-access-p ()
  (is-remote-database *current-okeanos-db*))

(defun remote-server ()
  (database-server *current-okeanos-db*))

(defmethod perform-with-db ((db file-database) fn args)
  (apply fn args))

(defmethod perform-with-db ((db remote-database) fn args)
  (rpc `(apply ,fn ,args)))

(defun perform (fn &rest args)
  (perform-with-db *current-okeanos-db* fn args))

;; -----------------------------------------------------

(defmethod db-offset ((pos integer))
  pos)

(defmethod db-offset (ptr)
  (mmf:pointer-address ptr))

;; -----------------------------------------------------

(defmethod db-ptr ((pos integer) &key type addr rel-addr)
  (mmf:copy-pointer (database-mapper *current-okeanos-db*)
                    :type    type
                    :address (or addr pos)
                    :rel-address (or rel-addr 0)))

(defmethod db-ptr (ptr &key type addr rel-addr)
  (mmf:copy-pointer (database-mapper *current-okeanos-db*)
                    :type type
                    :address (or addr
                                 (mmf:pointer-address ptr))
                    :rel-address (or rel-addr
                                     0)))

;; -------------------------------------------
;; Magic file header -- used as a generic header at the front of every file.
;; All other headers incorporate this in addition to their own unique fields.

(fli:define-c-union byte-order-word_t
  (s   uint16)
  (a   (:c-array uint8 2)))

(fli:define-c-struct magic-file-header_t
  (:byte-packing 1)
  (magic         (:c-array :char 4))  ;; 4
  (byte-order    byte-order-word_t)   ;; 2
  (hdr-size      ssize_t)             ;; 2
  (version-major uint16)              ;; 2
  (version-minor uint16))             ;; 2, total = 12

(defclass magic-file-header ()
  ((byte-order-s  :accessor      magic-file-header-byte-order-s
                  :allocation    :persistent
                  :c-access-spec '(byte-order s)
                  :c-type        uint16)
   
   (hdr-size      :accessor      magic-file-header-hdr-size
                  :allocation    :persistent
                  :c-access-spec 'hdr-size
                  :c-type        ssize_t
                  :constraint    'mmf:i>0)
   
   (version-major :accessor      magic-file-header-version-major
                  :allocation    :persistent
                  :c-access-spec 'version-major
                  :c-type        uint16)
   
   (version-minor :accessor      magic-file-header-version-minor
                  :allocation    :persistent
                  :c-access-spec 'version-minor
                  :c-type        uint16))
  
  (:metaclass mmf:persistent-metalevel-class)
  (:c-type magic-file-header_t))

(defun make-magic-file-header (mapper)
  (make-instance 'magic-file-header :mapper mapper :address 0))

;; -------------------------------------------
;; Data File Header -- used at the front of the DATA file.

(fli:define-c-struct data-file-header_t
  (:byte-packing 1)
  (hdr                 magic-file-header_t) ;; 12
  (filler1             uint32)              ;; 4
  (next-avail          off_t)               ;; 8
  (free-list           off_t)               ;; 8
  (string-pool         string-pool_t)       ;; 26
  (filler2             uint16)              ;; 2
  (filler3             uint32)              ;; 4
  (last-ts             ts_t)                ;; 16
  (other         (:c-array off_t 16)))      ;; 8 * 16 = 128, total = 208 = 13*16

(defclass data-file-header ()
  ((version-major :accessor      data-file-header-version-major
                  :allocation    :persistent
                  :c-access-spec '(hdr version-major)
                  :c-type        uint16)
   
   (version-minor :accessor      data-file-header-version-minor
                  :allocation    :persistent
                  :c-access-spec '(hdr version-minor)
                  :c-type        uint16)
   
   (next-avail    :accessor      data-file-header-next-avail
                  :allocation    :persistent
                  :c-access-spec 'next-avail
                  :c-type        off_t)
   
   (free-list     :accessor      data-file-header-free-list
                  :allocation    :persistent
                  :c-access-spec 'free-list
                  :c-type        off_t)
   
   (hdr-size      :accessor      data-file-header-hdr-size
                  :allocation    :persistent
                  :c-access-spec '(hdr hdr-size)
                  :c-type        ssize_t
                  :constraint    'mmf:i>0)

   (string-pool   :reader        string-pool-pointer
                  :allocation    :derived
                  :function      #'(lambda (mapper)
                                     (make-string-pool
                                      (data-file-header-name mapper)
                                      (mmf:foreign-slot-pointer mapper 'string-pool))))

   (last-ts       :accessor      data-file-header-last-ts
                  :allocation    :persistent
                  :c-access-spec 'last-ts
                  :c-type        ts_t)

   (name          :reader        data-file-header-name
                  :initarg       :name)
   )
                 
  (:metaclass mmf:persistent-metalevel-class)
  (:c-type data-file-header_t)
  (:cacheable t))

(defun make-data-file-header (name mapper)
  (make-instance 'data-file-header
                 :name   name
                 :mapper mapper
                 :address 0))

(defun sync ()
  (when *current-okeanos-db*
    (mmf:sync (database-mapper *current-okeanos-db*))))
      
;; ---------------------------------------------------

(defun collect-all-strings ()
  (collect-strings
   (string-pool-pointer (database-mapper *current-okeanos-db*))))

;; ---------------------------------------------------
;; all main-tree data items must begin with a
;; key pointer (off_t) as their first element

(fli:define-c-struct data-object
  (:byte-packing 1)
  (key     off_t))

(defun encode-object-to-string (obj)
  (coerce
   (map 'vector 'code-char
        (ubstream:with-output-to-ubyte-stream (s)
          (loenc:serialize obj s)))
   'string))

(defun decode-string-to-object (str)
  (ubstream:with-input-from-ubyte-stream
      (s (map 'vector 'char-code str))
    (loenc:deserialize s)))

;; -------------------------------------------

(defun find-string-pointer (str)
  (let ((mapper (database-mapper *current-okeanos-db*)))
    (find-string (string-pool-pointer mapper) str)))

(defun get-string-pointer (str)
  (let ((mapper (database-mapper *current-okeanos-db*)))
    (insert-string (string-pool-pointer mapper) str)))

(defun string-pool-offset (str)
  (mmf:pointer-address (get-string-pointer str)))

;; -----------------------------------------------------------
;; Sync RPC support

(defun rpc (msg)
  (with-rpc-timeout *timeout*
    (call-sync (remote-server) msg)))

;; -------------
;; Async RPC support

(define-condition servers-down-exception (error)
  ((servers  :accessor servers-down-exception-servers
             :initarg  :servers)))

(defun check-replies (tags)
  (when tags
    (optima:ematch (!?-recv-multi tags *timeout*)
      ((list :ok answers) answers)
      ((list :timeout _ down-tags)
       (error (make-instance 'servers-down-exception
                             :servers (mapcar 'cdr down-tags))))
      )))
  
(defun rpc-send (msg)
  (!?-send (remote-server) msg))

(defun send (msg)
  (call-async (remote-server) msg))

;; -------------------------------

(defmacro if-remote (t-clause &optional f-clause)
  `(if (remote-access-p)
       ,t-clause
     ,f-clause))

(defmacro when-remote (&body body)
  `(when (remote-access-p)
     ,@body))

(defmacro unless-remote (&body body)
  `(unless (remote-access-p)
     ,@body))

(defmacro defrpc (name args &body body)
  ;; macro to invoke rpc on the body of the function
  `(defun ,name ,args
     (if-remote
      (rpc '(,name ,@args))
      ;; else
      (progn
        ,@body))))

#+:LISPWORKS
(editor:setup-indent "defrpc" 1)

;; -------------------------------------------

(defrpc get-file-uuid ()
  (retrieve-folder-item +db-uuid-path+))

(defun get-last-ts ()
  (let ((mmapper (database-mapper *current-okeanos-db*)))
    (if mmapper
        (slot-value mmapper 'last-ts)
      (uuid:make-null-uuid))))

(defun set-last-ts (val)
  (let ((mmapper (database-mapper *current-okeanos-db*)))
    (when mmapper
      (setf (slot-value mmapper 'last-ts) val))))

;; -------------------------------------------
;; Generic routines for reading the length code at the
;; start of every allocated data region

(defun read-data-length (file pos)
  (let ((p (mmf:cast file 'off_t :address pos)))
    (mmf:fetch p)))

(defun write-data-length (val file pos)
  (let ((p (mmf:cast file 'off_t :address pos)))
    (mmf:store val p)))

;; ----------------------------------------------

(defun serialize-data-object (obj
                              &key
                              (type-uuid +data-item-uuid+)
                              (start 0))
  (let* ((image  (loenc:encode obj))
         (imglen (length image))
         (pos    (alloc (+ imglen start) type-uuid))
         ;; point past key
         (imgptr (db-ptr pos
                         :type `(:c-array uint8 ,imglen)
                         :rel-addr start)))
    (mmf:store-array (subseq image 0 imglen) imgptr)
    (db-ptr pos :type 'off_t)))

;; ----------------------------------------------

(defun deserialize-data-object (ptr &key (start 0))
  (ubstream:with-input-from-ubyte-stream
      (s (mmf:copy-pointer ptr :type 'uint8)
         :start  start
         :reader #'(lambda (ptr index)
                     (mmf:fetch ptr :index index)))
    (loenc:deserialize s)))

;; -----------------------------------------------

(defun setf-slots (obj &rest slots&vals)
  (unless (endp slots&vals)
    (destructuring-bind (slot-name val &rest tl) slots&vals
      (setf (slot-value obj slot-name) val)
      (apply #'setf-slots obj tl))))
  
;; -------------------------------------------------

;; -------------------------------
;; Basic persistent data objects

(defclass persistent-item ()
  ((oid     :accessor  persistent-item-oid
            :initarg   :oid)
   (object  :accessor  persistent-item-object
            :initarg   :object
            :initform  nil)
   (dirty   :accessor  persistent-item-dirty
            :initarg   :dirty
            :initform  nil)))

(defun make-persistent-item (&rest args)
  (apply 'make-instance 'persistent-item args))

;; -------------------------------

(defclass persistent-rawbytes-item (persistent-item)
  ((instance-table :accessor persistent-item-instance-table
                   :initarg  :instance-table
                   :initform nil)
   (indexes        :accessor persistent-item-indexes
                   :initarg  :indexes
                   :initform nil)))

(defun make-persistent-rawbytes-item (&rest args)
  (apply 'make-instance 'persistent-rawbytes-item args))

;; -------------------------------

;; -------------------------------

(defclass persistent-rawbytes-class-item (persistent-item)
  ((class-name     :accessor persistent-item-class-name
                   :initarg  :class-name)
   ))

(defun make-persistent-rawbytes-class-item (&rest args)
  (apply 'make-instance 'persistent-rawbytes-class-item args))


