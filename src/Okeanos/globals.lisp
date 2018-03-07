;; lock-daemon.lisp -- Read / Write Locking
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  07/09
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

(defvar *default-db-directory* (translate-logical-pathname "PROJECTS:LISP;OkeanosDB;"))

(defvar *db-files*           nil) ;; list of connected database database structs

(defvar *shutting-down*      nil) ;; only true during Lisp shutdown advice

(defvar *ts-offset*            0) ;; offset to bring TS up to file last TS

(defvar *timeout*             10) ;; timeout used for remote access

(defvar *dereference-slots*    t) ;; set to nil when marshaling references
(defvar *retrieving*         nil) ;; true when retrieving an object from DB

(defvar *current-okeanos-db* nil) ;; database instance when database is open/connected
(defvar *current-connection* nil) ;; cnx-info when in transaction

(defvar *current-commit-logfile* nil)

;; -------------------------------------------

(defgeneric persist (obj))
(defgeneric ref (obj))
(defgeneric deref (obj))
(defgeneric same-ref (a b))
(defgeneric valid-ref? (obj))

