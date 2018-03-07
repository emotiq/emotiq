;; queries.lisp -- Object ID Sets and OID files
;; --------------------------------------------------------------------------------------
;; OkeanosMM -- memory mapped database system
;;
;; DM/RAL  03/09
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

;; ---------------------------------------------------------------------
(defclass <query-binder> ()
  ((all-fn        :accessor query-all-fn        :initarg :all-fn)
   (slot-eq-fn    :accessor query-slot-eq-fn    :initarg :slot-eq-fn)
   (slot-range-fn :accessor query-slot-range-fn :initarg :slot-range-fn)
   ))
;; ---------------------------------------------------------------------

(defun query-engine (selection filter-fn fn-bindings)
  (with-accessors ((all-fn        query-all-fn)
                   (slot-eq-fn    query-slot-eq-fn)
                   (slot-range-fn query-slot-range-fn)) fn-bindings
    (let ((cfilter-fn (and filter-fn
                           (compile nil filter-fn)))
          (universe
           (optima:ematch selection
             ((or :all
                  (list :all))
              (funcall all-fn))

             ((or (list slot-name '= val)
                  (list '= slot-name val))
              (funcall slot-eq-fn slot-name val))

             ((list slot-name :from from-val)
              (funcall slot-range-fn slot-name from-val nil))

             ((list slot-name :to to-val)
              (funcall slot-range-fn slot-name nil to-val))

             ((list slot-name :from from-val :to to-val)
              (funcall slot-range-fn slot-name from-val to-val))
             )))
      (if cfilter-fn
          (remove-if (complement cfilter-fn) universe)
        universe))))

;; ---------------------------------------------------------------------
;; Persistent-Class Instances...

(defmethod run-query ((class-name symbol) selection filter-fn)
  (query-engine selection filter-fn
                (make-instance '<query-binder>
                               :all-fn        (deferred (find-instances* class-name))
                               :slot-eq-fn    (curry #'fetch-instances-for-slot class-name)
                               :slot-range-fn (curry #'find-instances-for-slot class-name))
                ))

(defmethod query ((class-name symbol) selection &optional filter-fn)
  (perform 'run-query class-name selection filter-fn))

;; ---------------------------------------------------------------------
;; File-Tables...

(defmethod run-query ((table file-table) selection filter-fn)
  (query-engine selection filter-fn
                (make-instance '<query-binder>
                               :all-fn        (deferred (fetch-all-rows table))
                               :slot-eq-fn    (curry 'fetch-rows-for-column table)
                               :slot-range-fn (curry 'find-rows-for-column table))
                ))

(defmethod query ((table file-table) selection &optional filter-fn)
  (run-query table selection filter-fn))

;; ---------------------------------------------------------------------
;; File-BTrees...

(defmethod run-query ((btree file-btree) selection filter-fn)
  (let ((keyfn (btree:key-fn btree)))
    (query-engine selection filter-fn
                  (make-instance '<query-binder>
                                 :all-fn         (deferred
                                                     (um:accum acc
                                                       (btree:map-tree btree
                                                                       (compose #'acc keyfn))))
                                 :slot-eq-fn     (curry 'btree:find-item btree)
                                 :slot-range-fn  #'(lambda (slot-ignored from to)
                                                     (declare (ignore slot-ignored))
                                                     (um:accum acc
                                                       (btree:map-tree btree (compose #'acc keyfn)
                                                                       :from from :to to)))
                                 ))))

(defmethod query ((btree file-btree) selection &optional filter-fn)
  (run-query btree selection filter-fn))

;; -----------------------------
;; List of system queries
#|

;; list of users that have read the database
(dolist (user-id (query (user-mappings) :all))
  (print (fetch-row `(:user-id ,user-id) (user-mappings))))

;; list of transctions against the database
(dolist (tid (query (transaction-directory) '(:all)))
  (print (fetch-row `(:tid ,tid) (transaction-directory))))

;; the oid mappings table
(dolist (oid (query (oid-mappings) '(:all)))
  (print (fetch-row `(:oid ,oid) (oid-mappings))))

;; the string pool contents
(dolist (str (query (string-pool-btree
                     (string-pool-pointer
                      (database-mapper *current-okeanos-db*)))
                    '(:all)))
  (print str))

;; the list of persistent classes registered with the database
(dolist (class-estr (query (get-class-table) '(:all)))
  (print (fetch-row `(:key ,(get-encoded-string-value class-estr)) (get-class-table))))

;; the list of OK-Tables registered in the database
(dolist (tbl (query 'ok-table '(:all)))
  (print (ok-table-name (deref tbl))))

;; the list of OK-Maps registered in the database
(dolist (map (query 'ok-map '(:all)))
  (print (ok-map-name (deref map))))

;; the list of OK-Sets registered in the database
(dolist (set (query 'ok-set '(:all)))
  (print (ok-set-name (deref set))))

;; the list of OK-Schema registered in the database
(dolist (schema (query 'ok-schema '(:all)))
  (print (ok-schema-name (deref schema))))
|#

;; ---------------------------------------------------------------------

#| ;; Example

(setf *timeout* 1000)

(get-persistent-class 'address-entry)
(query 'address-entry '(and (= okeanos::name "Dave")
                            (= okeanos::location "Tucson")))
(mapcar 'deref
        (query 'address-entry '(= okeanos::location "Tucson")))
(mapcar 'deref
        (query 'address-entry '(= okno::name "Dave")))

(mapcar 'deref
        (query 'address-entry '(and (= okeanos::location "Tucson"))))

(setf x (deref (car (query 'address-entry '(and (= okno::name "Dave")
                                                (= okno::location "Tucson"))))))
(setf (slot-value x 'okno::location) "Hollywood")
(commit)
(setf x (deref (car (query 'address-entry '(= okno::name "Dave")))))
(setf (slot-value x 'okno::location) "Tucson")
(commit)

(query 'address-entry '(okeanos::name = "Dave"))
                     
|#
