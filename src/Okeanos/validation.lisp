;; validation.lisp -- 
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

;; Some quick validation tools
;; ----------------------------------------

;; Check last transaction log to see if it completed properly.
;; If so, then the transaction entry should have a non-zero :file-end field.

(defun quick-validate ()
  (print "Validating Database")
  (with-read-lock ()
    (when-let (row (fetch-last-row (transaction-directory)))
      (unless (plusp (getf row :file-end))
        (error "The last transaction to the database never completed.
The database may be inconsistent.") ))))
