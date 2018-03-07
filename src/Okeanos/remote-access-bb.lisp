;; remote-access.lisp -- Remote Access to Okeanos through Butterfly
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

;; ---------------------------------------------------------------
(defun make-okeanos-client-proxy (user-id user-uuid database)
  (let ((tls (get-tls)))
    (setf (tls-user-id tls)        user-id
          (tls-archetype-uuid tls) user-uuid))
  
  (let ((*current-okeanos-db* nil)
        (*current-connection* (make-cnx-info)))
    
    (with-open-database (:dirname (or database *default-db-directory*))
        (funcall (make-bfly-service-loop
                  (make-service (:standard)
                    ()
                    (t (fn-name &rest args)
                       (apply fn-name args)))
                  nil nil)))
      ))

#|
(connect-to-database)
(rollback)
(!? (remote-server) :SYNC 2)
(find-instances 'ok-table)
(find-instances 'okdb::ok-table)
(disconnect-from-database)

|#


(defun parse-db-name (dbname)
  ;; dbname is okdb://host:port/file-path
  ;; e.g., okdb://RoadKill.local:12010/okeanosdb
  (multiple-value-bind (start len vstarts vends)
    (#~m|^okdb://([a-zA-Z0-9.]+(:[0-9]+)?)(/([a-zA-Z0-9\_\-/.+={}\(\)\[\]<>@#$%^&~]+))?$| dbname)
    (declare (ignore len))
    (when start
      (let ((strs (map 'list #'(lambda (start end)
                               (when start
                                 (subseq dbname start end)))
                       vstarts vends)))
        (values (first strs)
                (fourth strs))) )))

#|
(parse-db-name "okdb://RoadKill.local:12010/okeanosdb")
(parse-db-name "okdb://RoadKill.local/okeanosdb")
(parse-db-name "okdb://RoadKill.local")
(parse-db-name "okdb://RoadKill.local:12010")
|#

(defun make-remote-db-watcher-service ()
  (make-service (:bfly
                 :trap-exits-p t
                 :register :okeanos-connection-manager
                 :spawn-fn 'spawn-link)
    ((current-db nil))

    (:connect (server database)
     (remote-spawn-link server 'make-okeanos-client-proxy
                        :args (list (get-user-id-from-system)
                                    (make-uuid)
                                    database)
                        :name :okeanos-connection))

    (:set-current-db (db)
     (setf current-db db))
    
    (:exit-message (from-pid reason arg)
     (log-warning :system-log #"A remote database has disconnected: ~S
Database may have been damaged."#
                  (make-exit-message
                   :from-pid from-pid
                   :reason   reason
                   :arg      arg))
     (disconnect-from-database current-db)
     (exit :abnormal))
    ))
       
(defun connect-to-database (&optional db-path force)
  (multiple-value-bind (server database)
      (when db-path
        (parse-db-name db-path))
    (multiple-value-bind (new-db banner)
        (cond (server
               (let* ((mgr (make-remote-db-watcher-service))
                      (pid (call-sync mgr `(:connect
                                            ,(resolve-server server)
                                            ,database))))
                 (with-database (make-instance 'remote-database
                                               :dirname (format nil "~A@~A"
                                                                database server)
                                               :server  pid
                                               :uuid    (make-uuid))
                   (call-async mgr `(:set-current-db ,*current-okeanos-db*))
                   (setf *current-connection* (or *current-connection*
                                                  (make-cnx-info :db-state nil)))
                   (values *current-okeanos-db*
                           "Database connected") )))
              
              (t
               (open-okeanos-database :dirname (or db-path *default-db-directory*)
                                      :force   force)
               (values *current-okeanos-db*
                       "Database opened")) )
      
      (pushnew new-db *db-files*)
      (set-current-database new-db)
      ;;; (needs-lock-daemon) ;; idempotent
      (rollback)
      (values new-db banner) )))

(defun connect-to-databases (&rest db-paths)
  ;; connect to all or none
  (let (dbs)
    (handler-case
        (progn
          (dolist (db db-paths)
            (push (connect-to-database db) dbs))
          (nreverse dbs))
      
      (error (exn)
        (mapc 'disconnect-from-database dbs)
        (error exn)) )))

(defun set-current-database (db)
  (setf *current-okeanos-db* db))

(defun disconnect-from-database (&optional (db (or *current-okeanos-db*
                                                   (car *db-files*))) force)
  (when db
    (when (eq db *current-okeanos-db*)
      (setf *current-okeanos-db* nil
            *current-connection* nil))
    (with-database db
      (if-remote
          (progn
            (setf *db-files* (delete db *db-files*))
            (ignore-errors
              (call-async (remote-server) 'exit))
            "Database disconnected")
        ;; else -- local file
        (close-okeanos-database db force))) ))

(defun disconnect-from-databases (dbs)
  (dolist (db dbs)
    (disconnect-from-database db)))

(defun forcibly-disconnect-from-all-databases (&rest args)
  (declare (ignore args))
  (let ((*shutting-down* t))
    (dolist (db *db-files*)
      (disconnect-from-database db :force))))

(defun lw-start-okeanos (&rest ignored)
  (declare (ignore ignored))
  (unless (user::has-super-user-dongle-p)
    (um:when-let (pkg (find-package :com.sd.okeanos.int))
      (delete-package pkg))
    (unless (user::okno-security-dongle-present-p)
      (um:when-let (pkg (find-package :com.sd.okeanos.user))
        (delete-package pkg))
      (error "OKNO runtime dongle not present"))
    (unless (user::has-okno-api-license-p)
      (um:when-let (pkg (find-package :com.sd.okeanos.user))
        (delete-package pkg)))))

#+:LISPWORKS
(let ((lw:*handle-existing-action-in-action-list* '(:silent :skip)))
  (lw:define-action "When quitting image"
                    "Disconnect from databases"
                    'forcibly-disconnect-from-all-databases)


  (lw:define-action "Initialize LispWorks Tools"
                    "Start up Okeanos"
                    'lw-start-okeanos
                    :after "Run the environment start up functions"
                    :once)

  #+:LISPWORKS6
  (lw:define-action "Save Session After"
                    "Restart Okeanos"
                    'lw-start-okeanos))

