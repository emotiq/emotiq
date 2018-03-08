;; main-db.lisp -- persistent objects
;; --------------------------------------------------------------------------------------
;;
;; DM/RAL  08/08
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
