;; oid.lisp -- Object ID Maintenance
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

(fli:define-c-struct oid-entry_t
  (:byte-packing 1)
  (oid      oid_t)
  (rts      ts_t)
  (wts      ts_t)
  (file-id  uint32)
  (file-pos uint32))

(defun ensure-oid-pointers-schema (&key force)
  (ensure-schema +oid-pointers-schema-path+
      '((:oid              :oid
         :c-access-spec    oid
         :c-type           oid_t
         :indexed          :unique)

        (:file-id          :number
         :c-access-spec    file-id
         :c-type           uint32)
        
        (:file-pos         :number
         :c-access-spec    file-pos
         :c-type           uint32)
        
        (:rts              :uuid
         :c-access-spec    rts
         :c-type           ts_t)

        (:wts              :uuid
         :c-access-spec    wts
         :c-type           ts_t))
    
    :c-type 'oid-entry_t
    :force  force))

;; ----------------------------------------------------------------

(fli:define-c-struct transaction-entry_t
  (:byte-packing 1)
  (td-ts       ts_t)
  (td-wuid     oid_t)
  (td-file-id  uint32)
  (td-file-pos uint32)
  (td-file-end uint32))

(defun ensure-trans-dir-schema (&key force)
  (ensure-schema +transaction-directory-schema-path+
      '((:tid              :uuid
         :c-access-spec    td-ts
         :c-type           ts_t
         :indexed          :unique)

        (:wuid             :oid
         :c-access-spec    td-wuid
         :c-type           oid_t)
        
        (:file-id          :number
         :c-access-spec    td-file-id
         :c-type           uint32)
        
        (:file-pos         :number
         :c-access-spec    td-file-pos
         :c-type           uint32)
    
        (:file-end         :number
         :c-access-spec    td-file-end
         :c-type           uint32))
    
    :c-type 'transaction-entry_t
    :force  force))

;; ----------------------------------------------------------------

(fli:define-c-struct user-id-entry_t
  (:byte-packing 1)
  (uid-name    off_t)
  (uid-oid     oid_t))

(defun ensure-user-mappings-schema (&key force)
  (ensure-schema +user-mappings-schema-path+
      '((:user-id          :string
         :c-access-spec    uid-name
         :c-type           off_t
         :indexed          :unique)

        (:user-oid         :oid
         :c-access-spec    uid-oid
         :c-type           oid_t))
    
    :c-type 'user-id-entry_t
    :force  force))

;; ----------------------------------------------------------------

(defun write-oid-file-vector (filenames)
  (create-folder-item +files-list-path+ filenames))

(defun read-oid-file-vector ()
  (retrieve-folder-item +files-list-path+))
  
(defun get-next-log-serno ()
  (1+ (length (database-files-list *current-okeanos-db*))))

(defun create-new-log-file ()
  (let* ((dirname     (database-dirname *current-okeanos-db*))
         (file-vector (database-files-list *current-okeanos-db*))
         (logfilename (format nil "log~4,'0d.dam" (get-next-log-serno))))
    (close (open (merge-pathnames logfilename dirname)
                 :direction :output
                 :if-exists :supersede
                 :if-does-not-exist :create))
    (vector-push-extend logfilename file-vector)
    (write-oid-file-vector file-vector)))

(defun logfile-path (file-id)
  (let* ((file-vector (database-files-list *current-okeanos-db*))
         (directory   (database-dirname    *current-okeanos-db*))
         (ix          (if (eq file-id :last)
                          (1- (length file-vector))
                        file-id)))
    (merge-pathnames (aref file-vector ix) directory)
    ))

;; -------------------------------------------------------

(defconstant +logfile-max-size+ (ash 1 28)) ;; 256 MB

(defun commit-to-logfile (dirty-items)
  ;; write-locked from above
  (when dirty-items
    (tagbody
     :again
     (with-open-file (f (logfile-path :last)
                        :direction :output
                        :if-exists :append
                        :element-type '(unsigned-byte 8))
       (when (> (file-position f) +logfile-max-size+)
         (go :needs-new-logfile))
       (let* ((commit-id (current-db-state))
              (msg       (mkstr (when-created commit-id) " by " (get-user-id)))
              (vstart    (loenc:encode (list commit-id msg) :prefix-length 1))
              (vend      (loenc:encode commit-id :prefix-length 1))
              (trans-pos (file-position f)))
         ;; make an entry into the transaction index first,
         ;; then perform the transaction logging
         (let ((*current-commit-logfile* f))
           (register-user)
           (log-transaction commit-id (current-commit-logfile-id) trans-pos)
           (if (uuid:uuid< (get-last-ts) commit-id)
               (set-last-ts commit-id))
           (write-byte #.(char-code #\B) f)
           (write-sequence vstart f)
           (commit-objects-to-logfile dirty-items)
           (write-byte #.(char-code #\E) f)
           (write-sequence vend f)
           (record-transaction-endpoint commit-id (file-position f))
           (sync)
           )))
     (return-from commit-to-logfile)
       
     :needs-new-logfile
     (create-new-log-file)
     (go :again)
     )))

;; ----------------------------------------------------------

(defun current-commit-logfile-id ()
  (1- (length (database-files-list *current-okeanos-db*))))

(defmethod encode-data (obj)
  (loenc:encode obj :prefix-length 4))

(defmethod encode-data ((obj rawbytes))
  (rawbytes-bytes obj))

(defmethod commit-object-changes (obj item)
  (let* ((oid   (persistent-item-oid item))
         ;; prefix counts exclude their own width
         (v-oid (loenc:encode oid :prefix-length 1))
         ;; (v-enc (loenc:encode obj :prefix-length 4))
         (v-enc (encode-data obj))
         (old-fnbr 0)
         (old-fpos 0)
         (old-wts  #.(uuid:make-null-uuid)))
    (write-byte #.(char-code #\O) *current-commit-logfile*)
    (write-sequence v-oid *current-commit-logfile*)
    (when-let (row (fetch-row `(:oid ,oid) (oid-mappings)))
      (setf old-fnbr (getf row :file-id)
            old-fpos (getf row :file-pos)
            old-wts  (getf row :wts)))
    (let ((pos (file-position *current-commit-logfile*))
          (v-prev (loenc:encode (list* old-fnbr old-fpos old-wts) :prefix-length 1)))
      (write-sequence v-prev *current-commit-logfile*)
      (write-sequence v-enc *current-commit-logfile*)
      (store-oid-pointer oid (current-commit-logfile-id) pos))
    ))

;; ----------------------------------------------------------

(defmethod commit-object-deletion (obj item)
  (declare (ignore obj))
  (let* ((oid       (persistent-item-oid item))
         (v-oid     (loenc:encode oid :prefix-length 1))
         (old-fnbr  0)
         (old-fpos  0)
         (old-wts   #.(uuid:make-null-uuid)))
    (write-byte #.(char-code #\X) *current-commit-logfile*)
    (write-sequence v-oid *current-commit-logfile*)
    ;; leave in OID table with negated file position
    (when-let (row (fetch-row `(:oid ,oid) (oid-mappings)))
      (setf old-fnbr (getf row :file-id)
            old-fpos (getf row :file-pos)
            old-wts  (getf row :wts))
      ;; doing this allows a later get-oid-pointer to perform
      ;; proper read transaction checking
      (store-oid-pointer oid old-fnbr (- old-fpos))) ;; mark pos < 0 => deleted
    ;; record previous position
    (let ((v-prev (loenc:encode (list* old-fnbr old-fpos old-wts) :prefix-length 1)))
      (write-sequence v-prev *current-commit-logfile*))
    ;; obj is already deleted from obj->item-mappings table
    (remove-key-value oid (oid->item-mappings))))

;; ----------------------------------------------------------------

(defun locate-logfile-data (oid nprev found-fn)
  (with-read-lock ()
    (let* ((oid-ptr  (get-oid-pointer oid))
           (file-id  (getf oid-ptr :file-id))
           (filename (logfile-path file-id))
           (file-pos (getf oid-ptr :file-pos)))
      (nlet-tail iter ((ctr nprev))
        (if (minusp file-pos)
            (when (minusp ctr)
              (setf file-pos (- file-pos))
              (iter (1+ ctr)))
          (when (plusp file-pos) ;; pos <= 0 is deleted record
            (with-open-file (f filename
                               :direction :input
                               :element-type '(unsigned-byte 8))
              (or (file-position f file-pos)
                  (error "Can't seek: ~A (~A)" filename file-pos))
              (let ((prev (loenc:deserialize f :prefix-length 1)))
                (when (eq prev f) ;; retured stream f on EOF
                  (loenc:early-eof))
                (if (minusp ctr)
                    (progn
                      (setf file-id  (car prev)
                            file-pos (cadr prev)
                            filename (logfile-path file-id))
                      (iter (1+ ctr)))
                  ;; else - get this version
                  (funcall found-fn f))) )))) )))   ;; returns NIL otherwise

;; ----------------------------------------------------------------

(defun fetch-from-logfile (oid &optional (nprev 0))
  ;; default return is NIL
  (locate-logfile-data oid nprev
                       #'(lambda (f)
                         ;; performed inside of read-lock
                         (let ((obj (loenc:deserialize f :prefix-length 4)))
                           (when (zerop nprev) ;; persist only if current version
                             (intern-persistent-item (make-persistent-item
                                                      :oid      oid
                                                      :object   obj)))
                           obj)) ))

;; ----------------------------------------------------------------

(defun remote-fetch-bytes-from-logfile (oid &optional (nprev 0))
  ;; default return is NIL
  (locate-logfile-data oid nprev
                       #'(lambda (f)
                         ;; performed inside of read-lock
                         `(:RAW-FETCH 
                           ,(loenc:read-data f :prefix-length 4)) )))

;; --------------------------------------------------

(defun register-user ()
  (let ((tls (get-tls)))
    (dolist (*current-okeanos-db* (shiftf (tls-needs-user-registration tls) nil))
      (let ((oid (cdr (assoc *current-okeanos-db* (tls-user-oid tls)))))
        (insert-row `(:user-id  ,(tls-user-id tls)
                      :user-oid ,oid)
                    (user-mappings))))))

;; ----------------------------------------------------------------
#|
(open-database)
(map-rows (oid-mappings) 'print)
(map-rows (oid-mappings) (let ((prev (make-oid :uuid (uuid:make-null-uuid))))
                           #'(lambda (row)
                             (let ((oid  (getf row :oid)))
                               (assert (not (oid= oid (shiftf prev oid))))
                               (print (list oid (get-persistent-object oid)))))))
(view-btree (oid-mappings))
(map-rows (transaction-directory) 'print)
(map-rows (transaction-directory) #'(lambda (row)
                                    (let ((tid (getf row :tid)))
                                      (print (list (when-created tid) row)) )))
(map-rows (user-mappings) #'(lambda (row)
                            (let ((usr (getf row :user-id))
                                  (oid (getf row :user-oid)))
                              (print (list usr oid))) ))
(close-database)
|#
;; ----------------------------------------------------------------

(defrpc valid-oid? (oid)
  (when-let (row (fetch-row `(:oid ,oid) (oid-mappings)))
    (plusp (getf row :file-pos)) ))

(defun validate-commit (item)
  ;; Timestamp ordering (TO)
  (let ((wts (current-db-state))
        (oid (persistent-item-oid item)))
    (when-let (row (fetch-row `(:oid ,oid) (oid-mappings)))
      (cond ((ts< wts (getf row :rts))
             ;; check to be sure object on file hasn't been read
             ;; by a more recent session
             (raise-rollback-exception))
            
            ((ts< wts (getf row :wts))
             ;; nobody would ever see our write, so just discard it
             (setf (persistent-item-dirty item) nil) ;; just being cautious...
             (remove-key-value (persistent-item-object item) (obj->item-mappings))
             (remove-key-value oid (oid->item-mappings))
             (return-from validate-commit))
             ))
    
    (validate-object-commit
     (if (is-deleted item)
         (deleted-item-object item)
       (persistent-item-object item))
     item)))

(defmethod validate-object-commit (obj item)
  (declare (ignore obj item))
  t)

;; ---------------------------------------------

;; ---------------------------------------------

(defun log-transaction (trans-id trans-file-id trans-file-pos)
  (insert-row `(:tid      ,trans-id
                :wuid     ,(get-user-oid)
                :file-id  ,trans-file-id
                :file-pos ,trans-file-pos)                
              (transaction-directory)))

(defun record-transaction-endpoint (trans-id file-position)
  (let* ((trans-dir (transaction-directory))
         (row (fetch-row `(:tid ,trans-id) trans-dir)))
    (setf (getf row :file-end) file-position)
    (insert-row row trans-dir)))

;; ---------------------------------------------

(defun update-wts (oid)
  (let ((wts (current-db-state)))
    (when-let (row (fetch-row `(:oid ,oid) (oid-mappings)))
        (when (ts< (getf row :wts) wts)
          (setf (getf row :wts) wts)
          (insert-row row (oid-mappings)))
      )))

#| ;; unused
(defun update-rts (oid)
  (let ((rts (current-db-state)))
    (when-let (row (fetch-row `(:oid ,oid) (oid-mappings)))
        (when (ts< (getf row :rts) rts)
          (setf (getf row :rts) rts)
          (insert-row row (oid-mappings)))
      )))
|#

;; ---------------------------------------------

(define-condition oid-not-found-error (error)
  ((oid   :accessor oid-not-found-oid
          :initarg  :oid))
  (:report report-oid-not-found-error))

(defun report-oid-not-found-error (err stream)
  (format stream "OID not found: ~A" (oid-not-found-oid err)))


(defun get-oid-pointer (oid)
  ;; Timestamp ordering (TO)
  (let ((rts (current-db-state)))
    (if-let (row (fetch-row `(:oid ,oid) (oid-mappings)))
      (cond ((ts< rts (getf row :wts)) ;; trying to read from future
             (raise-rollback-exception))

            ((ts< (getf row :rts) rts)
             ;; update the read-timestamp
             (setf (getf row :rts) (current-db-state))
             (insert-row row (oid-mappings))
             row)
            
            (t row)
            )
      ;; else
      (error (make-instance 'oid-not-found-error
                            :oid oid))) ))

(defun store-oid-pointer (oid new-file-id new-file-pos)
  (let ((wts  (current-db-state))
        (tbl  (oid-mappings)))

    (if-let (row (fetch-row `(:oid ,oid) tbl))
        (when (ts< (getf row :wts) wts)
          (setf (getf row :file-id)  new-file-id
                (getf row :file-pos) new-file-pos
                (getf row :wts)      wts)
          (insert-row row tbl))

      ;; else - new record
      (insert-row `(:oid      ,oid
                    :file-id  ,new-file-id
                    :file-pos ,new-file-pos
                    :wts      ,wts
                    :rts      ,wts)
                  tbl))
    ))

;; unused - we don't physically delete OID's from the OID-Table
;; We just change their file-position to negative
#|
(defun delete-oid-pointer (oid)
  (delete-row `(:oid ,oid) (oid-mappings)))
|#

;; -------------------------------------------------------------------

