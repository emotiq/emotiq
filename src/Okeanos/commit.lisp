;; commit.lisp -- transaction commit for persistent objects
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

(defmacro with-timeout-trapping (clause recovery-clause)
  `(do-with-timeout-trapping (deferred ,clause) (deferred ,recovery-clause)))

#+:LISPWORKS
(editor:setup-indent "with-timeout-trapping" 2)

(defun do-with-timeout-trapping (fn exn-fn)
  (handler-case
      (funcall fn)
    (bfly-exn-timeout (exn)
      (declare (ignore exn))
      (funcall exn-fn)) ))

(defun back-off ()
  (sleep #+:LISPWORKS (lw:mt-random 1.0)
         #+:ALLEGRO   (random 1.0)))

;; -------------------------------

(defun commit (&key (retry 10) (timeout *timeout*))
  ;; since only one Okeanos VM process can have a database opened at one time
  ;; we only need process in-memory write-locks instead of disk-based write-locks.
  (let* ((*timeout* timeout)
         (mappings  (sorted-oid-mappings-collection)) ;; all databases needing commit
         (rems      (collect-remote-databases mappings)))
    
    (prep-remote-databases-for-commit rems)
    
    (unwind-protect
        (when mappings
          (try-commit-with-retries retry mappings rems))
      (rollback))))

(define-condition cant-get-commit-locks (error)
  ())

(define-condition commit-failed-to-complete (error)
  ())

(defun try-commit-with-retries (retry mappings rems)
  (nlet-tail iter ((retry retry)) ;; all or none write locking on all databases
    (let ((status (try-commit mappings rems)))
      (case status
        (:ok)
        (:failed
         (log-warning :system-log "OKEANOS ALERT!
A commit failed to complete within the write-lock lease period.
Will attempt rollback, but might not succeed.")
         (error (make-condition 'commit-failed-to-complete)))

        (t
         (if (plusp retry)
             (progn
               (back-off)
               (unless (eq :need-remote-lock status)
                 ;;
                 ;; not perfect -- seems we really need continuations here
                 ;; in order to grab a lock and then restart without
                 ;; giving up the lock. In this case, we grab with timeout
                 ;; and if we succeed we give it up again, in the hopes it will
                 ;; still be available when we try again upstairs...
                 ;;
                 ;; To do otherwise, by calling iter from inside the lock
                 ;; we risk filling up the stack...
                 (with-database status ;; status is local db file
                   (with-write-lock (:timeout *timeout*)) ))
               (iter (1- retry)))
           ;; else
           (error (make-condition 'cant-get-commit-locks)) ))
        ))))

(defun try-commit (mappings rems)
  ;; ask remote client-proxies to grab write locks
  (let ((rem-tags (multicast-grab-write-locks rems)))
    (unwind-protect
        (nlet with-local-locks ((triples mappings))
          ;; grab write locks on all the resident databases
          ;; remote databases also have a resident portion, grab those too
          (if triples
              (with-database (caar triples)
                (with-timeout-trapping
                    (with-write-lock (:timeout 0.1)
                      (with-local-locks (cdr triples)))
                    
                    ;; timeout failure - return db file needing lock
                    *current-okeanos-db*))
        
            ;; else
            (with-timeout-trapping
                (progn
                  ;; now verify write locks on the remote servers
                  (check-replies rem-tags)
                  
                  ;; now holding all the necessary write locks
                  (with-timeout-trapping
                      ;; trap timeouts so that we don't think we need another
                      ;; run-query at all the locks...
                      (progn
                        (pre-commit-verify-all mappings)
                        (post-verify-commit-all mappings)
                        :OK) ;; return successful...
                      
                      ;; timeout clause
                      ;; timeout here indicates failure, should not retry locks
                      :FAILED ))
                
            ;; remote verify timeout clause
            :need-remote-lock) )) ;; indicate failure, should retry locks
      ;; unwind
      (multicast-release-write-locks rems)) ))
        
;; -------------------------------

(defun collect-remote-databases (mappings)
  (remove-if (complement 'is-remote-database)
             mappings
             :key 'car))

;; -------------------------------

(defun prep-remote-databases-for-commit (rems)
  (with-databases (rems)
    (remote-prep-for-commit)))

;; -------------------------------

(defun multicast-grab-write-locks (rems)
  ;; returns a list of tags for later reply checking
  (with-databases (rems)
    (rpc-send `(grab-write-lock 0.1))))

(defun multicast-release-write-locks (rems)
  ;; ignore returned list of tags
  (with-databases (rems)
    (send `(release-write-lock))))


;; -------------------------------

(defun multicast-do-and-check (mappings fn)
  (let ((tags nil))
    (with-databases (mappings)
      (if-remote
       (push (funcall fn) tags) ;; fn should return a tag from rpc-send
       (funcall fn)))
    (check-replies tags)))

(defun pre-commit-verify-all (mappings)
  ;; do the pre-commit verification across all databases
  ;; multicast on remote databases
  (multicast-do-and-check mappings 'pre-commit-verify))

(defun post-verify-commit-all (mappings)
  ;; having passed that, go ahead and commit all databases
  ;; multicast on remote databases
  (multicast-do-and-check mappings 'post-verify-commit))
                              
;; -------------------------------

(defun has-dirty-items (tbl)
  (map-keys-values #'(lambda (k item)
                       (declare (ignore k))
                       (if (persistent-item-dirty item)
                           (return-from has-dirty-items t)))
                   tbl))

(defun sorted-oid-mappings-collection ()
  (let ((all-mappings (cnx-info-mappings *current-connection*)))
    (loop for mappings in all-mappings
          do (pre-commit-scan mappings))
    ;; get db's with dirty items and present in consistent order for
    ;; multilocking
    (sort
     (remove-if (complement 'has-dirty-items) all-mappings
                :key 'cadr) ;; using the oid->item mappings table
     'uuid:uuid<
     :key (um:compose 'database-uuid 'car))))

;; -------------------------------

(defun pre-commit-scan (mappings)
  (with-database (car mappings)
    (dolist (item (collect-dirty-items))
      (pre-commit item)) ))

(defun pre-commit (item)
  (pre-commit-for-object (persistent-item-object item) item))

(defmethod pre-commit-for-object (obj item)
  (declare (ignore obj item)))

;; -------------------------------

(defun pre-commit-verify ()
  (if-remote
   (list (rpc-send `(local-pre-commit-verify))) ;; list for nconc, return tag for remote database
   (progn
     (local-pre-commit-verify)
     nil))) ;; return nil for local database for nconc

;; -------------------------------

(defun post-verify-commit ()
  (if-remote
   (list (rpc-send `(post-verify-commit))) ;; list for nconc, return tag for remote database
   ;; scan again because we might have removed some...
   (progn
     (commit-to-logfile (collect-dirty-items))
     nil))) ;; return nil for local database for nconc

;; -------------------------------

(defun collect-dirty-items ()
  (let ((dirty-items nil))
    (map-keys-values #'(lambda (oid item)
                         (declare (ignore oid))
                         (when (persistent-item-dirty item)
                           (push item dirty-items)))
                     (oid->item-mappings))
    dirty-items))

(defun local-pre-commit-verify ()
  (dolist (item (collect-dirty-items))
    (validate-commit item)))

;; -------------------------------

(defun remote-prep-for-commit ()
  (let ((items nil))
    (dolist (item (collect-dirty-items))
      (let ((obj (persistent-item-object item)))
        (typecase obj

          ((or ok-set ok-map)
           (rpc `(update-from-remote-then-ok
                  ,(persistent-item-oid item)
                  ,(get-additions obj)
                  ,(get-deletions obj))))
          
          (ok-schema
           (rpc `(update-ok-schema-from-remote-then-ok
                  ,(persistent-item-oid item)
                  ,(persistent-object-item (slot-value obj 'schema)))))
          
          (ok-table
           (rpc `(update-ok-table-from-remote-then-ok
                  ,(persistent-item-oid item)
                  ,(get-pendings obj)
                  ,(persistent-object-item (slot-value obj 'schema))
                  ,(table-sequence obj))))

          (t
           (push (convert-item-to-rawbytes item) items))
          )))
    (rpc `(import-items-from-client ,items))))

(defmethod update-from-remote-then-ok ((oid oid) adds deletes)
  (update-from-remote (get-persistent-object oid) adds deletes)
  :OK)

(defmethod update-ok-schema-from-remote-then-ok ((oid oid) schema-item)
  (let ((obj (get-persistent-object oid)))
    ;; NOTE: interning a persistent item retains the item's dirty state
    (intern-persistent-item schema-item)
    (setf (slot-value obj 'schema) (persistent-item-oid schema-item))
    :OK))

(defmethod update-ok-table-from-remote-then-ok ((oid oid) adds schema-item seqno)
  (let ((obj (get-persistent-object oid)))
    ;; NOTE: interning a persistent item retains the item's dirty state
    (intern-persistent-item schema-item)
    (setf (slot-value obj 'schema) (persistent-item-oid schema-item))
    (update-ok-table-from-remote obj adds seqno)
    :OK))

;; -------------------------------

(defmethod convert-item-to-rawbytes ((item persistent-rawbytes-item))
  item)

(defmethod convert-item-to-rawbytes ((item persistent-item))
  (let ((del (is-deleted item)))
    (make-item-for-rawbytes-object item
                                   (if del
                                       (prog1
                                           (deleted-item-object item)
                                         (setf (second del) nil)) ;; we won't need it
                                     (persistent-item-object item))
                                   del)))

(defmethod make-item-for-rawbytes-object (item obj deleted)
  (make-persistent-item
   :oid     (persistent-item-oid item)
   :object  (unless deleted
              (make-rawbytes :bytes (loenc:encode obj :prefix-length 4)))
   :dirty   (persistent-item-dirty item)))

(defmethod make-item-for-rawbytes-object (item (obj schema) deleted)
  (declare (ignore deleted))
  item)

;; -------------------------------

(defun import-items-from-client (dirty-items)
  (dolist (item dirty-items)
    (intern-persistent-item item))
  :OK)

;; -------------------------------

(defun commit-objects-to-logfile (dirty-items)
  (nlet-tail iter ((dirty-list dirty-items))
    (when dirty-list
      (dolist (item dirty-list)
        ;; do the actual commit
        (cond ((is-deleted item)
               (commit-object-deletion (deleted-item-object item)
                                       item))
              
              (t (commit-object-changes (persistent-item-object item)
                                        item)) )
        (setf (persistent-item-dirty item) nil))

      ;; scan again because we might have added some... e.g., persistent-classes
      (iter (collect-dirty-items)))
    ))

