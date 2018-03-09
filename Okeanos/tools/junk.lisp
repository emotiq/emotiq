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
;; ----------------------- Virtual Slots --------------------
(in-package "CL-USER")
 
;; Metaclass of objects that might contain virtual slots.
 
(defclass virtual-metaclass (standard-class)
  ()
  )
 
;; Mixin metaclass for virtual slots and methods to make them
;; appear virtual.
 
(defclass virtual-slot-definition 
          (standard-slot-definition)
  ((function :initarg :function 
             :accessor virtual-slot-definition-function))
  )
 
(defmethod slot-definition-allocation 
           ((slotd virtual-slot-definition))
  :virtual)
 
(defmethod (setf slot-definition-allocation) 
           (allocation (slotd virtual-slot-definition))
  (unless (eq allocation :virtual)
    (error "Cannot change the allocation of a ~S"
           'virtual-direct-slot-definition))
  allocation)
 
;; Class of direct virtual slots and methods to construct them
;; when appropriate.
 
(defclass virtual-direct-slot-definition 
          (standard-direct-slot-definition
           virtual-slot-definition)
  ()
  )
 
;; Called when the class is being made, to choose the metaclass of
;; a given direct slot. It should return the class of slot
;; definition required.
 
(defmethod clos:direct-slot-definition-class 
           ((class virtual-metaclass) &rest initargs)
  ;; Use virtual-direct-slot-definition if appropriate.
  (if (eq (getf initargs :allocation) :virtual)
      (find-class 'virtual-direct-slot-definition)
    (call-next-method)))
 
;; Called when the defclass is expanded, to process a slot option.
;; It should return the new list of slot options, based on
;; already-processed-options.
 
(defmethod clos:process-a-slot-option 
           ((class virtual-metaclass) option value
            already-processed-options slot)
  ;; Handle the :function option by adding it to the
  ;; list of processed options.
  (if (eq option :function)
      (list* :function value already-processed-options)
    (call-next-method)))
 
 
;; Class of effective virtual slots and methods to construct
;; them when appropriate.
 
(defclass virtual-effective-slot-definition 
          (standard-effective-slot-definition
           virtual-slot-definition)
  ()
  )
 
;; Called when the class is being finalized, to choose the
;; metaclass of a given effective slot.  It should return the
;; class of slot definition required.
 
(defmethod clos:effective-slot-definition-class 
           ((class virtual-metaclass) &rest initargs)
  ;; Use virtual-effective-slot-definition if appropriate.
  (let ((slot-initargs (getf initargs :initargs)))
    (if (member :virtual-slot slot-initargs)
        (find-class 'virtual-effective-slot-definition)
      (call-next-method))))
 
(defmethod clos:compute-effective-slot-definition 
           ((class virtual-metaclass)
            name
            direct-slot-definitions)
  ;; Copy the function into the effective slot definition
  ;; if appropriate.
  (let ((effective-slotd (call-next-method)))
    (dolist (slotd direct-slot-definitions)
      (when (typep slotd 'virtual-slot-definition)
        (setf (virtual-slot-definition-function effective-slotd) 
              (virtual-slot-definition-function slotd))
        (return)))
    effective-slotd))
 
 
;; Underlying access methods for invoking
;; virtual-slot-definition-function.
 
(defmethod clos:slot-value-using-class 
           ((class virtual-metaclass) object slot-name)
  (let ((slotd (find slot-name (class-slots class) 
                     :key 'slot-definition-name)))
    (if (typep slotd 'virtual-slot-definition)
        (funcall (virtual-slot-definition-function slotd)
                 :get
                 object)
      (call-next-method))))
 
(defmethod (setf clos:slot-value-using-class) 
           (value (class virtual-metaclass) object slot-name)
  (format t "~% setf slot : ~A" slot-name)
  (let ((slotd (find slot-name (class-slots class) 
                     :key 'slot-definition-name)))
    (if (typep slotd 'virtual-slot-definition)
        (funcall (virtual-slot-definition-function slotd)
                 :set
                 object
                 value)
      (call-next-method))))
 
(defmethod clos:slot-boundp-using-class 
           ((class virtual-metaclass) object slot-name)
  (let ((slotd (find slot-name (class-slots class) 
                     :key 'slot-definition-name)))
    (if (typep slotd 'virtual-slot-definition)
        (funcall (virtual-slot-definition-function slotd)
                 :is-set
                 object)
      (call-next-method))))
 
(defmethod clos:slot-makunbound-using-class 
           ((class virtual-metaclass) object slot-name)
  (let ((slotd (find slot-name (class-slots class) 
                     :key 'slot-definition-name)))
    (if (typep slotd 'virtual-slot-definition)
        (funcall (virtual-slot-definition-function slotd)
                 :unset
                 object)
      (call-next-method))))
 
(defmethod clos:slot-exists-p-using-class 
           ((class virtual-metaclass) object slot-name)
  (or (call-next-method)
      (and (find slot-name (class-slots class) 
                 :key 'slot-definition-name)
           t)))
 

#|
;; Example virtual slot which depends on a real slot.
;; Compile this separately after the virtual-metaclass etc.
 
(defclass a-virtual-class ()
  ((real-slot :initarg :real-slot :accessor real-slot
              :initform -1)
   (virtual-slot :accessor virtual-slot 
                 :initarg :virtual-slot
                 :allocation :virtual
                 :function 
                 'a-virtual-class-virtual-slot-function))
  (:metaclass virtual-metaclass))
 
(defun a-virtual-class-virtual-slot-function 
       (key object &optional value)
  (ecase key
    (:get (let ((real-slot (real-slot object)))
            (if (<= 0 real-slot 100)
                (/ real-slot 100.0)
              (slot-unbound (class-of object) 
                            object
                            'virtual-slot))))
    (:set (setf (real-slot object) (* value 100))
          value)
    (:is-set (let ((real-slot (real-slot object)))
               (<= real-slot 100)))
    (:unset (setf (real-slot object) -1))))
|#

;; ----------------------- Virtual Slots --------------------
#|
(setf object (make-instance 'a-virtual-class))


|#

(bfly:exit 5 :lock-error)