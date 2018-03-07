;; find-map-classes.lisp -- Database Class Search & Mapping
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

;; -----------------------------------------------------------

(defun get-mapping-table-name-and-type (class-name slot-name)
  (let* ((class         (get-persistent-class class-name))
         (indexed-slots (persistent-class-indexed-slots class))
         (pair          (find slot-name indexed-slots
                              :test 'eq
                              :key  (compose 'clos:slot-definition-name
                                                'first))))
    (unless pair
      (no-index-error class-name slot-name))
    (destructuring-bind (slotd . table-name) pair
      (values table-name (is-indexed-slot slotd)) )))

(defun no-index-error (class-name slot-name)
  (error "No index for class: ~A slot: ~A" class-name slot-name))

(defun get-instance-table-name (class-name)
  (table-name-for-class class-name))

;; -----------------------------------------------------------

(defun find-mapping-table (class-name slot-name)
  ;; return the index table and the kind of index (:unique, :indexed)
  (multiple-value-bind (table-name index-type)
      (get-mapping-table-name-and-type class-name slot-name)
    (values (find-file-table table-name)
            index-type) ))

(defun find-instance-table (class-name)
  (find-file-oid-set (get-instance-table-name class-name)))
  
;; -----------------------------------------------------------
;; map-instances of a class,
;; and map-instances* for all of class and all subclasses

(defun fetch-instance-oid (ptr)
  (make-oid
   :uuid (mmf:fetch ptr :type 'oid_t)))

(defun map-instances (class-name fn &key max-records)
  (let ((table-name (get-instance-table-name class-name)))
    (if-remote
     (dolist (inst (rpc `(collect-class-instances-from-table
                          ,table-name ,max-records)))
       (funcall fn inst))
     ;; else
     (factored-map-instances table-name fn max-records)) ))

(defun factored-map-instances (table-name fn max-records)
  (when-let (btree (find-file-oid-set table-name))
    (with-read-lock ()
      (btree:map-tree btree
                      #'(lambda (ptr)
                        (funcall fn (fetch-instance-oid ptr)))
                      :max-records max-records) )))

(defun collect-class-instances-from-table (table-name max-records)
  (um:accum acc
    (factored-map-instances table-name #'acc max-records)))

;; -----------------------------------------------------------

(defun make-counter (&optional max-records)
  (let ((count 0))
    (um:dlambda
      (:finished? ()
       (and max-records
            (>= count max-records)))
      (:remaining ()
       (and max-records
            (- max-records count)))
      (:incr (&optional (incr 1))
       (incf count incr)))
    ))

(defun map-instances* (class-name fn &key max-records)
  (let ((counter (make-counter max-records)))
    (nlet iter ((class (get-persistent-class class-name)))
      (map-instances (class-name class)
                     #'(lambda (oid)
                         (funcall counter :incr)
                         (funcall fn oid))
                     :max-records (funcall counter :remaining))
      (do ((subclasses (clos:class-direct-subclasses class)
                       (cdr subclasses)))
          ((or (funcall counter :finished?)
               (endp subclasses)))
        (iter (car subclasses)))
      )))

;; -----------------------------------------------------------
;; Commentary: Ordinarily, sending in parameters like from, to,
;; direction, would be a whacking waste of time, compared to just
;; pasing in a discriminator function of the user's choosing.
;;
;; But that would entail sending every possible record to the
;; discriminator, which ignores the possibility that the DB has a
;; basic concept of ordering and might be able to selectively pare
;; down the universe for the requestor.
;;
;; And in the face of remote connections that would be even worse,
;; causing potentially massive amounts of traffic over the network.
;;
;; So while range bounding may seem lame, it is probably the best that
;; the DB server can do unless you pass along a discriminator function
;; to the server as well. And while that may also be possible in a
;; Lisp-centric environment, it would only be possible if the
;; discriminator function used only functions that the server knew
;; about.
;;
;; A most general discriminator probably will make use of application
;; specific functions that only the client knows about.
;;
;; So we may as well accept the lameness of the (from, to, direction)
;; paring and let the DB server help us out just a bit in paring down
;; the universe set.

(defun map-instances-for-slot (class-name slot-name fn
                                          &rest args
                                          &key
                                          from to (direction :forward)
                                          max-records)
  (declare (ignore from to direction max-records))
  (multiple-value-bind (table-name index-type)
      (ignore-errors (get-mapping-table-name-and-type class-name slot-name))
    
    (if table-name ;; we have indexed slot?
        (if-remote
         (dolist (pair (rpc `(collect-instances-for-slot-using-table
                              ,table-name ,index-type ,@args)))
           (apply fn pair)) ;; pair should be (key val)
         ;; else
         (apply #'map-instances-for-slot-using-table table-name index-type fn args))
      
      ;; else - slot not indexed
      (if-remote
       (dolist (pair (rpc `(collect-instances-for-slot-not-using-table
                            ,class-name ,slot-name ,@args)))
         (apply fn pair)) ;; pair should be (key val)
       ;; else
       (apply #'map-instances-for-slot-not-using-table class-name slot-name fn args)) )))

(defun get-instances-for-row (row index-type)
  (let ((obj-ref (getf row :oid)))
    (case index-type
      (:unique  (list obj-ref))
      (:indexed (car (deref obj-ref))) )))
  
(defun map-instances-for-slot-using-table (table-name index-type fn &rest args)
  (when-let (table (find-file-table table-name))
    (apply #'map-rows table
           #'(lambda (row)
               (let ((obj-refs (get-instances-for-row row index-type))
                     (key      (decode-string-to-object (getf row :key))))
                 (funcall fn key obj-refs)))
           args)))

(defun general-instance-mapper-for-slot (class-name slot-name fn)
  (map-instances* class-name
                  #'(lambda (oid)
                    (let* ((obj (deref oid))
                           (val (make-encoded-string-from-object
                                 (basic-slot-value obj slot-name))))
                      (funcall fn val oid))) ))

(defun map-instances-for-slot-not-using-table (class-name slot-name fn
                                                          &key
                                                          from to (direction :forward)
                                                          max-records)
  ;; direction only makes sense if just one of (from, to) has been specified.
  (multiple-value-bind (fromv tov)
      (let ((fromv (and from (make-encoded-string-from-object from)))
            (tov   (and to   (make-encoded-string-from-object to))))
        (cond ((and fromv tov)
               (if (ord:compare< fromv tov)
                   (values fromv tov)
                 (values tov fromv)))
              (t (values fromv tov))
              ))
  
    (let ((counter (make-counter max-records)))
      (labels ((mapper (cmpfn)
                 (block mapper
                   (general-instance-mapper-for-slot class-name slot-name
                                                     #'(lambda (key oid)
                                                       (when (funcall cmpfn key)
                                                         (funcall fn key oid)
                                                         (funcall counter :incr)
                                                         (when (funcall counter :finished?)
                                                           (return-from mapper))))) )))
        (cond ((and fromv tov)
               (mapper #'(lambda (val)
                         (and (ord:compare<= fromv val)
                              (ord:compare< val tov)))))
              (fromv
               (mapper #'(lambda (val)
                         (or (and (eq direction :forward)
                                  (ord:compare<= fromv val))
                             (ord:compare<= val fromv)))))

              (tov
               (mapper #'(lambda (val)
                         (or (and (eq direction :forward)
                                  (ord:compare< val tov))
                             (ord:compare<= tov val)))))

              (t
               (mapper #'(lambda (val)
                         (declare (ignore val))
                         t)))
              )))))

(defun collect-kv-pairs (mapper-fn &rest args &key &allow-other-keys)
  (um:accum acc
    (apply mapper-fn (um:compose #'acc #'list) args)))

(defun collect-instances-for-slot-using-table (table-name index-type &rest args
                                                          &key &allow-other-keys)
  (apply 'collect-kv-pairs
         (curry 'map-instances-for-slot-using-table table-name index-type)
         args))

(defun collect-instances-for-slot-not-using-table (class-name slot-name &rest args
                                                              &key &allow-other-keys)
  (apply 'collect-kv-pairs
         (curry 'map-instances-for-slot-not-using-table class-name slot-name)
         args))

;; -----------------------------------------------------------

(defun make-class-cursor (class-name)
  (let ((table-name (get-instance-table-name class-name)))
    (perform 'make-class-cursor-for-table table-name)))

(defun make-class-cursor-for-table (table-name)
  (when-let (btree (find-file-oid-set table-name))
    (with-read-lock ()
      (btree:create-cursor btree))))

;; -----------------------------------------------------------

(defun find-instances (class-name &key max-records)
  (let ((table-name (get-instance-table-name class-name)))
    ;; return a list of instance oid's for the class
    (perform 'find-instances-using-table table-name max-records)))

(defun find-instances-using-table (table-name max-records)
  (um:accum acc
    (factored-map-instances table-name #'acc max-records)))


;; -----------------------------------------------------------

(defun find-instances* (class-name &key max-records)
  (let ((instances nil)
        (counter   (make-counter max-records)))
      
    (nlet iter ((class (get-persistent-class class-name)))
      (let ((more (find-instances (class-name class)
                                  :max-records (funcall counter :remaining))))
        (setf instances (nconc more instances))
        (funcall counter :incr (length more))
        (do ((subclasses (clos:class-direct-subclasses class)
                         (cdr subclasses)))
            ((or (funcall counter :finished?)
                 (endp subclasses)))
          (iter (car subclasses)))
        ))
    instances))

;; -----------------------------------------------------------

(defun find-instances-for-slot (class-name slot-name &rest args
                                           &key from to (direction :forward) max-records)
  (declare (ignore from to direction max-records))
  (multiple-value-bind (table-name index-type)
      (ignore-errors (get-mapping-table-name-and-type class-name slot-name))
    (if table-name
        (apply 'perform 'find-instances-for-slot-using-table table-name index-type args)
      (apply 'perform 'find-instances-for-slot-not-using-table class-name slot-name args)) ))

(defun find-instances-for-slot-using-table (table-name index-type &rest args)
  (flatten
   (um:accum acc
     (apply 'map-instances-for-slot-using-table table-name index-type
            (um:compose #'acc #'cadr) args))))
           
(defun find-instances-for-slot-not-using-table (class-name slot-name
                                                           &rest args
                                                           &key
                                                           (direction :forward)
                                                           &allow-other-keys)
  (let ((ans (maps:empty)))
    (apply 'map-instances-for-slot-not-using-table class-name slot-name
           #'(lambda (key oid)
             (if-let (cell (maps:find key ans))
                 (push oid (car cell))
               (setf ans (maps:add key (list (list oid)) ans))))
           args)
    ;; now ans has a mapping in lexical order
    (let ((oids (maps:fold #'(lambda (k v acc)
                             (declare (ignore k))
                             (nconc (car v) acc))
                           ans nil)))
      (if (eq direction :forward)
              (nreverse oids)
        oids)) ))
        
;; -----------------------------------------------------------

(defun first-instances-for-slot (class-name slot-name)
  (multiple-value-bind (table-name index-type)
      (ignore-errors (get-mapping-table-name-and-type class-name slot-name))
    (if table-name
        (perform 'first-instances-for-slot-using-table table-name index-type)
      (perform 'first-instances-for-slot-not-using-table class-name slot-name))))

(defun first-instances-for-slot-using-table (table-name index-type)
  (when-let (table (find-file-table table-name))
    (when-let (row (fetch-first-row table))
      (get-instances-for-row row index-type)) ))

(defun first-instances-for-slot-not-using-table (class-name slot-name)
  (find-instances-for-slot-not-using-table class-name slot-name
                                           :max-records 1))


(defun last-instances-for-slot (class-name slot-name)
  (multiple-value-bind (table-name index-type)
      (ignore-errors (get-mapping-table-name-and-type class-name slot-name))
    (if table-name
        (perform 'last-instances-for-slot-using-table table-name index-type)
      (perform 'last-instances-for-slot-not-using-table class-name slot-name))))

(defun last-instances-for-slot-using-table (table-name index-type)
  (when-let (table (find-file-table table-name))
    (when-let (row (fetch-last-row table))
      (get-instances-for-row row index-type))))

(defun last-instances-for-slot-not-using-table (class-name slot-name)
  (let ((kmax     nil)
        (last-oid nil))
    (general-instance-mapper-for-slot class-name slot-name
                                      #'(lambda (key oid)
                                        (when (or (null kmax)
                                                  (ord:compare> key kmax))
                                          (setf kmax     key
                                                last-oid oid))) )
    (and last-oid
         (list last-oid)) ))

;; -----------------------------------------------------------

(defun fetch-instances-for-slot (class-name slot-name key)
  (multiple-value-bind (table-name index-type)
      (ignore-errors (get-mapping-table-name-and-type class-name slot-name))
    (if table-name
        (perform 'fetch-instances-for-slot-using-table key table-name index-type)
      (perform 'fetch-instances-for-slot-not-using-table class-name slot-name key))))

(defun fetch-instances-for-slot-using-table (key table-name index-type)
  (when-let (table (find-file-table table-name))
    (when-let (row (fetch-row `(:key ,key) table))
      (get-instances-for-row row index-type))))

(defun fetch-instances-for-slot-not-using-table (class-name slot-name key)
  (let ((kval (make-encoded-string-from-object key)))
    (general-instance-mapper-for-slot class-name slot-name
                                      #'(lambda (key oid)
                                        (if (ord:compare= kval key)
                                            (return-from fetch-instances-for-slot-not-using-table
                                              (list oid) ))) )))

;; -----------------------------------------------------------

(defun delete-instances-for-slot (class-name slot-name key)
  (multiple-value-bind (table-name index-type)
      (get-mapping-table-name-and-type class-name slot-name)
    (perform 'delete-instances-for-slot-using-table key table-name index-type)))

(defun delete-instances-for-slot-using-table (key table-name index-type)
  (when-let (table (find-file-table table-name))
    (when-let (row (fetch-row `(:key ,key) table))
      (dolist (oid (get-instances-for-row row index-type))
        (delete-oid oid))) ))
  
