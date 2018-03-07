;; transactions.lisp -- atomic transactions for persistent objects
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

;; -----------------------------------------------------------
;; Atomic and OrElse

(defun current-transaction ()
  (tls-cnx-info (get-tls)))

(define-condition abort-exn ()
  ())

(defun do-atomic (fn retries)
  (let* ((*current-connection* (current-transaction))
         (db-save              (current-db-state)))
    (cond (db-save
           ;; nested transactions already have a timestamp
           (funcall fn))

          (t ;; outer transactions begin with no timestamp
             (unwind-protect
                 (nlet iter ((retry retries))
                   (handler-case
                       (progn
                         (rollback)
                         (prog1
                             (funcall fn)
                           (commit)))
                     
                     (rollback-exception (exn)
                       (if (or (null  retries)
                               (plusp retry))
                           (progn
                             (back-off)
                             (print "Restarting transaction")
                             (iter (and retries (1- retry))))
                         ;; else -- too many attempts
                         (error "Aborting ATOMIC transaction after ~D attempts"
                                (1+ retries))))

                     (abort-exn (exn)
                       (when db-save
                         (error exn))) ))
               
               ;; unwind
               (rollback nil) ))
          )))

(defmacro atomic ((&key (retries 10)) &body body)
  `(do-atomic (lambda () ,@body) ,retries))

(defun do-orelse (retries &rest fns)
  (nlet iter ((retry retries))
    (dolist (fn fns)
      (handler-case
          (return-from do-orelse (do-atomic fn 0))                   

        (rollback-exception (exn)
          (declare (ignore exn))) ))
    
    (if (or (null retries)
            (plusp retry))
        (progn
          (back-off)
          (print "Restarting transaction")
          (iter (and retries (1- retry))))
      ;; else
      (error "Aborting ORELSE transaction after ~D attempts"
             (1+ retries)) )))

(defmacro orelse ((&key (retries 10)) &rest clauses)
  `(apply 'do-orelse ,retries
          ,(mapcar #'(lambda (clause)
                     `(lambda ()
                        ,clause))
                   clauses)))

(defun abort-transaction ()
  (error (make-condition 'abort-exn)))

;; --------------------------------------------------------

(defmethod open-okeanos-database :after (&key &allow-other-keys)
  (atomic () (get-user-oid))
  (rollback))


