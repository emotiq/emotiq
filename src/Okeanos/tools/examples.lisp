;; examples.lisp -- Okeanos examples
;; DM/RAL  08/09
;; ---------------------------------------------------
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
;; ---------------------------------------------------
(in-package #:okeanos)
;; ---------------------------------------------------

(defclass address-entry ()
  ((name     :accessor address-entry-name
             :initarg    :name
             :indexed    :unique)
   (location :accessor address-entry-location :initarg :location
             :indexed    t)
   )
  (:metaclass persistent-class)
  (:oid       #/oid/{390DE170-854D-11DE-9B87-00254BAF81A0}))


(defclass extended-address-entry (address-entry)
  ((telephone :accessor address-entry-telephone
              :initarg :telephone)
   )
  (:metaclass persistent-class)
  (:oid       #/oid/{50A300D6-854D-11DE-9B87-00254BAF81A0}))

#| ;; evolve the class def
(defclass address-entry ()
  ((name     :accessor address-entry-name
             :initarg    :name
             :indexed    :unique)
   (location :accessor address-entry-location :initarg :location
             :indexed    t)
   (telephone :accessor address-entry-telephone
              :initform :not-available
              :initarg :telephone)
   )
  (:metaclass persistent-class)
  (:oid       #/oid/{390DE170-854D-11DE-9B87-00254BAF81A0}))
|#
;; ---------------------------------------------------

(defclass diddly-mixin ()
  ((diddly :accessor diddly
           :initarg :diddly
           :initform nil)
   ))

(defclass thing (diddly-mixin)
  ((a :accessor a
      :initarg :a
      :initform nil)
   )
  (:metaclass persistent-class)
  (:oid       #/oid/{648195EA-854D-11DE-9B87-00254BAF81A0}))


(defmethod print-object ((obj thing) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~{~A ~A~^ ~}"
            (um:foldl (lambda (acc slotd)
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

#|
;; ----------------------------------------------------
;; OK-Sets
(atomic ()
  (let ((xx (make-instance 'ok-set)))
    (add-to-set xx 15)
    (add-to-set xx 32)
    (add-to-set xx 99)))

(setf xx (car (find-instances 'ok-set)))
(map-set (deref xx) 'print)

;; ----------------------------------------------------
;; OK-Maps
(atomic ()
  (let ((xx   (make-instance 'ok-map :name "MyJunk/Physical Constants")))
    (setf (getmap xx :c-light) 3e10)
    (setf (getmap xx :planck)  1.6e-16)))

(setf xx (car (find-instances 'ok-map)))
(deref (getmap (deref xx) :c-light))
(map-map (deref xx) (lambda (k v) (print (list k (deref v)))))


;; ----------------------------------------------------
;; General Persistent-Objects

(atomic ()
  (make-instance 'address-entry
                 :name "Dave"
                 :location "Tucson")
  (make-instance 'address-entry
                 :name "Mary"
                 :location "San Diego")
  (make-instance 'address-entry
                 :name "Panos"
                 :location "Boston"))

(atomic ()
  (let* ((dave (make-instance 'extended-address-entry
                              :name "Dave"
                              :location "Tucson"
                              :telephone "520-529-2437")))
    (persist
     (list (ref dave)
           (ref (make-instance 'address-entry
                               :name "Helene"
                               :location "Tucson"))
           (ref (make-instance 'address-entry
                               :name "Panos"
                               :location "Boston"))))
    ))

(get-persistent-class 'extended-address-entry)
(find-instances-for-slot 'address-entry 'location :from "A" :to "Z")
(find-instances-for-slot 'address-entry 'location :from "Tucson")
(find-instances* 'address-entry)
(mapcar 'deref (first-instances-for-slot 'address-entry 'name))
(mapcar (compose #'print #'get-persistent-object)
        (car (deref (car (last-instances-for-slot 'address-entry 'location)))))

;; -------------------------------------------------------

(setf dave (car (find-instances-for-slot 'address-entry 'name :from "Dave")))
(setf (slot-value (deref dave) 'name) "Panos") ;; Panos already exists
(commit)

(pprint (mapcar 'deref (find-instances* 'address-entry)))
(pprint (mapcar 'deref (find-instances-for-slot 'address-entry 'name "A" :to "Z")))

(map 'nil (compose #'print #'deref)
     (find-instances-for-slot 'address-entry 'location "Tucson"))
(map 'nil (compose #'print #'deref)
     (find-instances-for-slot 'address-entry 'location "Tucsonx"))
(map 'nil (compose #'print #'deref)
     (find-instances-for-slot 'address-entry 'name "Dave"))

;; -----------------------

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

|#
