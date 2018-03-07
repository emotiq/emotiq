;; logfiles.lisp -- Database Log File operations
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
;; Logfiles
;; -----------------------------------------------------------
#|
LogFiles contain all transactions as well as the database data for OO store.
All items in the logfile, except for the prefix type-code, are stored in unreadable
encoded form, preceeded by a count of bytes for the following encoding.

Denote the count prefixes as C1, C2, C4, etc.
Count bytes are stored in network order, i.e. BigEndian

Entries in the logfile have the following forms:

<Begin Transaction>
#\B C1 (Transaction-ID Date-TimeUTC-ID-String)
;; Date in YYYY/MM/DD, Time in HH:MM:SS, ID in #NN

<End Transaction>
#\E C1 Transaction-ID

<Object Storage>
#\O C1 OID C1 (Prev-File-ID Prev-File-Pos . Prev-WTS) C4 Encoded-data

<Object Deletion>
#\X C1 OID C1 (Prev-File-ID Prev-File-Pos . Prev-WTS)

<OK-Set Additions> - OID refers to OK-Set
#\S C1 OID C4 Key

<OK-Set Deletions> - OID refers to OK-Set
#\T C1 OID C4 Key

<OK-Map Additions> - OID refers to OK-Map
#\M C1 OID C4 (Key . Ref-Value)

<OK-Map Deletions> - OID refers to OK-Map
#\N C1 OID C4 Key

<OK-Table Headers> - OID refers to OK-Table
#\U C1 OID C4 (Key-Name (Col1 Col2 ...))

<OK-Table Additions> - OID refers to OK-Table
#\V C1 OID C4 (Val1 Val2 ....)

<OK-Table Deletions> - OID refers to OK-Table
#\W C1 OID C4 Key

<OK-Table Bulk Load> - OID refers to OK-Table
#\Z C1 OID C4 Bulk-Data-Array

|#
;; -----------------------------------------------------------

(defun invalid-logfile (str)
  (error "Invalid logfile contents: Prefix = ~A" str))

(defun show-deserialized (type f nprefix nprefix2 stream)
  (let ((oid (loenc:deserialize f :prefix-length 1)))
    (format stream "~& ~A: ~A" type oid)
    (when nprefix
      (let ((obj (loenc:deserialize f :prefix-length nprefix)))
        (format stream " ~S" obj)
        (when nprefix2
          (let ((obj2 (loenc:deserialize f :prefix-length nprefix2)))
            (format stream " ~S" obj2))) )) ))

#|
;; NO NO NO... affects all of the rest of Lisp!
(defmethod show-object (fmt obj)
  (format t (mkstr "~&" fmt ": ~S") obj))

(defmethod show-object (fmt (obj standard-object))
  (format t 
          "~&~A: #<~A ~{~A ~A~^ ~}>"
          fmt
          (type-of obj)
          (foldr #'(lambda (slotd acc)
                      (let ((name (clos:slot-definition-name slotd)))
                        (list* name
                               (if (slot-boundp obj name)
                                   (slot-value obj name)
                                 '*unbound*)
                               acc)))
                    (clos:class-effective-slots (class-of obj)) nil)))
|#

#|
(defun describe-logfile (file-id &optional (stream t) skip-types)
  (atomic (:retries 0)
    (with-open-file (f (logfile-path file-id)
                       :direction    :input
                       :element-type '(unsigned-byte 8))
      (describe-logfile-entries f
                                :stream stream
                                :skip-types skip-types) )
    (rollback)))
|#

(defun describe-logfile-entries (f &key (stream t) skip-types (nbr-entries 100) (nbr-skip 0))    
  (let (last)
    (nlet-tail iter ((nbr-shown 0)
                     (nbr-skipped 0))
      (when (and nbr-entries
                 (> nbr-shown nbr-entries))
        (format stream "~&[More...]")
        (return-from describe-logfile-entries))
      (let ((c (read-char f nil f)))
        (unless (eq c f)
          (setf last c)
          (if (or (member c skip-types)
                  (< nbr-skipped nbr-skip))
              (let ((nskips 
                     (case c
                       ((#\B #\E)  '(1))
                       (#\X        '(1 1))
                       (#\O        '(1 1 4))
                       ((#\T #\S #\N #\M #\U #\V #\W #\Z)  '(1 4))
                       (t (invalid-logfile (mkstr c)))) ))
                (dolist (nskip nskips)
                  (loenc:skip-data f nskip))
                (unless (eql last #\E)
                  (iter nbr-shown
                        (1+ nbr-skipped))))
            
            
            (multiple-value-bind (type pref-len pref-len2)
                (case c
                  (#\B (values "Begin Commit"        ))
                  (#\X (values "DELETE"             1))
                  (#\O (values "OBJECT"           1 4))
                  (#\E (values "End Commit"          ))
                  (#\S (values "OK-Set Add"         4))
                  (#\T (values "OK-Set Delete"      4))
                  (#\M (values "OK-Map Add"         4))
                  (#\N (values "OK-Map Delete"      4))
                  (#\W (values "OK-Table Delete"    4))
                  (#\V (values "OK-Table Insert"    4))
                  (#\U (values "OK-Table Columns"   4))
                  (#\Z (values "OK-Table Bulk-Load" 4))
                  (t   (invalid-logfile (mkstr c))) )
              (show-deserialized type f pref-len pref-len2 stream)
              (unless (eql last #\E)
                (iter (1+ nbr-shown)
                      nbr-skipped)) )) )))
    (unless (eql last #\E)
      (invalid-logfile "Truncated final transaction")) ))

;; --------------
;; Skip-sets

(defconstant +skip-tables+  '(#\U #\V #\W #\Z))
(defconstant +skip-maps+    '(#\M #\N))
(defconstant +skip-sets+    '(#\S #\T))
(defconstant +skip-objects+ '(#\O #\X))
(defconstant +skip-begin+   '(#\B))
(defconstant +skip-end+     '(#\E))

(defun combine-skips (&rest skips)
  (foldl #'(lambda (acc skip)
              (union acc skip))
            nil skips))

#|
(defun show-transactions (file-id &optional (stream t))
  (describe-logfile file-id stream
                    (combine-skips +skip-tables+
                                   +skip-maps+
                                   +skip-sets+
                                   +skip-objects+
                                   +skip-end+)))
                                  
|#

#|
(show-transactions :last)
(describe-logfile :last t '(#\U #\V #\W))
(with-open-file (f "Okeanos/logfile.txt"
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
  (describe-logfile :last f))

(describe-logfile 0)
|#

#|
(open-database)
(close-database)

(rollback)

(progn
  (unless (get-persistent-class 'extended-address-entry nil)
    (def-persistent-class address-entry ()
      ((name     :accessor address-entry-name
                 :initarg    :name
                 :indexed    :unique)
       (location :accessor address-entry-location :initarg :location
                 :indexed    t)))
  
    (def-persistent-class extended-address-entry (address-entry)
      ((telephone :accessor address-entry-telephone
                  :initarg :telephone)))
    )

  (defclass diddly-mixin ()
    ((diddly :accessor diddly
             :initarg :diddly
             :initform nil)))

  (unless (get-persistent-class 'thing nil)
    (def-persistent-class thing (diddly-mixin)
      ((a :accessor a
          :initarg :a
          :initform nil))))
  )

(progn
  (defmethod print-object ((obj thing) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "~{~A ~A~^ ~}"
              (foldl #'(lambda (acc slotd)
                          (let ((name (harlequin-common-lisp:slot-definition-name slotd)))
                            (list* name
                                   (if (slot-boundp obj name)
                                       (slot-value obj name)
                                     '*unbound*)
                                   acc)))
                        nil (harlequin-common-lisp:class-effective-slots (class-of obj))) )))

  (defmethod print-object ((obj address-entry) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "Name: ~A Location: ~A"
              (address-entry-name obj)
              (address-entry-location obj))))

  (defmethod print-object ((obj extended-address-entry) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "Name: ~A Location: ~A Telephone: ~A"
              (address-entry-name obj)
              (address-entry-location obj)
              (address-entry-telephone obj))))
  )

(inspect (obj->item-mappings))
(inspect (oid->item-mappings))

(map-instances* 'thing (compose 'print 'deref))

(defparameter *dave*
  (make-persistent-instance 'extended-address-entry
                            :name "Dave"
                            :location "Tucson"
                            :telephone "520-529-2437"))
(defparameter lst
  (make-persistent
   (list *dave*
         (make-persistent-instance 'address-entry
                                   :name "Helene"
                                   :location "Tucson")
         (make-persistent-instance 'address-entry
                                   :name "Panos"
                                   :location "Boston"))))
(oid-for-object lst)
(oid-for-object *dave*)
(commit)
;;(describe-logfile :last t +skip-tables+)
(rollback)
(collect-dirty-items)
(progn
  (rollback)
  (setf xx (make-persistent-instance 'ok-set))
  (add-to-set (deref xx) 15)
  (add-to-set (deref xx) 32)
  (add-to-set (deref xx) 99)
  (commit))
;;(describe-logfile :last t +skip-tables+)
(find-instances-for-slot 'address-entry 'location "A" :to "Z")
(find-instances-for-slot 'address-entry 'location "Tucson")
(find-instances* 'address-entry)
(mapcar 'deref (first-instances-for-slot 'address-entry 'name))
(mapcar (compose 'print 'get-persistent-object)
        (car (deref (car (last-instance-for-slot 'address-entry 'location)))))
(progn
  (rollback)
  (setf xx (make-persistent-instance 'ok-map :name "MyJunk/Physical Constants"))
  (setf (getmap (deref xx) :c-light) 3e10)
  (setf (getmap (deref xx) :planck)  1.6e-16)
  (commit))
;;(describe-logfile :last t +skip-tables+)

(setf xx (car (find-instances 'ok-map)))
(deref (getmap (deref xx) :c-light))
(map-map (deref xx) #'(lambda (k v) (print (list k (deref v)))))

(setf xx (car (find-instances 'ok-set)))
(map-set (deref xx) 'print)

(setf *dave* (car (find-instances-for-slot 'address-entry 'name "Dave")))
(setf (address-entry-name (deref *dave*)) "Panos") ;; no can do... Panos already exists
(commit)

(pprint (mapcar 'deref (find-instances* 'address-entry)))
(pprint (mapcar 'deref (find-instances-for-slot 'address-entry 'name "A" :to "Z")))

(map 'nil (compose 'print 'deref)
     (find-instances-for-slot 'address-entry 'location "Tucson"))
(map 'nil (compose 'print 'deref)
     (find-instances-for-slot 'address-entry 'location "Tucsonx"))
(map 'nil (compose 'print 'deref)
     (find-instances-for-slot 'address-entry 'name "Dave"))

(defparameter x (make-persistent-instance 'thing :a 1 :diddly :two))
(defparameter y (make-persistent-instance 'thing :a 2 :diddly :three))

;; now try redefining the class

(def-persistent-class thing (diddly-mixin)
  ((a :accessor a
      :initarg :a
      :initform nil)
   (b :accessor b
      :initarg :b
      :initform nil)))

(defparameter z (make-persistent-instance 'thing :a 15 :b lst :diddly :four))
(commit)
;;(describe-logfile :last t +skip-tables+)


|#
