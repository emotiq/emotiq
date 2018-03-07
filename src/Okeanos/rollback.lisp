;; rollback.lisp -- transaction rollback for persistent objects
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

(defun rollback (&optional (newstate nil newstate-p))
  (let ((highest-wts (get-new-ts)))

    (labels ((vote (ts)
               (if (uuid:uuid< highest-wts ts)
                   (setf highest-wts ts))))
      
      (when *current-connection*
        
        ;; cull out the databases that are no longer available
        (deletef-if (cnx-info-mappings *current-connection*)
                    (complement (compose (rcurry 'member *db-files*)
                                         'car)))
        
        (do-databases
         #'(lambda (triple)
           (clear-keys-values (cadr triple))
           (clear-keys-values (cddr triple))))
        
        (let ((tags nil))
          (with-databases ()
            (if-remote
             (push (list (rpc-send `(rollback :QUERY))) tags)
             ;; else
             (vote (get-last-ts))))

          (dolist (reply (check-replies tags))
            (vote (cdr reply))) ))

      (cond ((and newstate-p
                  (null newstate))
             (setf (cnx-info-db-state *current-connection*) nil))
            
            ((eq newstate :QUERY) highest-wts)
            
            ((not (remote-access-p))
             (set-timestamp highest-wts))
            ))))

(defun set-timestamp (wts)
  (setf (cnx-info-db-state *current-connection*) wts)
  (with-databases ()
    (when-remote
      (rpc `(set-timestamp ,wts)) )))
       
;; -------------------------------

(define-condition rollback-exception ()
  ())

(defun raise-rollback-exception ()
  (rollback)
  (error (make-condition 'rollback-exception)))

(define-condition serious-rollback-exception (error)
  ((msg  :reader  rbe-message
         :initarg :message)
   (args :reader  rbe-args
         :initarg :args
         :initform nil))
  (:report report-rbe-exn))

(defun report-rbe-exn (err stream)
  (apply 'format stream (rbe-message err) (rbe-args err)))

(defun raise-serious-rollback-exception (msg &rest args)
  (rollback)
  (error (make-condition 'serious-rollback-exception
                         :message msg
                         :args    args)))

        
        
