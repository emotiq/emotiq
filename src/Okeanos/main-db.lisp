;; main-db.lisp -- persistent objects
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

;; -------------------------------------------------

(defun create-db-lock (&optional (dirname *default-db-directory*))
  (let ((uuid (make-uuid)))
    (create-folder-item "DB-Lock"  uuid
                        :if-exists :error
                        :directory dirname)
    uuid))

(defun db-lock-holder-uuid (&optional (dirname *default-db-directory*))
  (retrieve-folder-item "DB-Lock" dirname))

(defun break-lock (&optional (dirname *default-db-directory*))
  (delete-folder-item "DB-Lock" dirname))
  
;; -------------------------------------------------

(defmethod open-okeanos-database (&key (dirname *default-db-directory*) (use-heap 1000) force)
  (let ((uuid (handler-case
                  (create-db-lock dirname)
                (file-error (exn)
                  exn))))
    (typecase uuid
      (uuid:uuid
       (let ((db
              #|
              (handler-case
                  (open-okeanos-file-database dirname use-heap uuid force)
                (error (exn)
                  (break-lock dirname)
                  (error exn)))
              |#
              (open-okeanos-file-database dirname use-heap uuid force)
              ))
         (setf *current-okeanos-db* db
               *current-connection* (or *current-connection*
                                        (make-cnx-info)))
         (push *current-okeanos-db* *db-files*)
         ;; (do-atomic 'get-user-oid 10)
         (init-server) ;; idempotent
         "DB opened"))

      (file-error
       (um:if-let (lock-holder
                   (let ((lock-holder-uuid (db-lock-holder-uuid dirname)))
                     (find lock-holder-uuid *db-files*
                           :key 'database-uuid
                           :test 'uuid:uuid=)) )
           (progn
             (incf (database-refc lock-holder))
             (set-current-database lock-holder)
             (setf *current-connection* (make-cnx-info))
             "DB opened")
         
         ;; else
         (error "Database locked: ~A" dirname)))

      (error
       (error uuid))
      )))

;; -------------------------------------------

(defun close-okeanos-database (&optional (db *current-okeanos-db*) force)
  (when (or force
            (not (plusp (decf (database-refc db)))))
    (close-okeanos-file-database db)
    (break-lock (database-dirname db))
    (setf *db-files* (delete db *db-files*))
    "DB closed"))

;; -------------------------------

(defmacro! with-open-database ((&key dirname) &body body)
  `(progn
     (open-okeanos-database :dirname ,dirname)
     (let ((,g!db *current-okeanos-db*))
       (hcl:unwind-protect-blocking-interrupts-in-cleanups
           (progn
             ,@body)
         (close-okeanos-database ,g!db)) )))
    

#+:LISPWORKS
(editor:setup-indent "with-open-database" 2)

;; -------------------------------
#|
;; superseded in remote-access.lisp
#+:LISPWORKS
(let ((lw:*handle-existing-action-in-action-list* '(:silent :skip)))
  (lw:define-action "When quitting image"
                    "Close database"
                    'close-database))
|#
