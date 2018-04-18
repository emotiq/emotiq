
(in-package :plotter)

;; ------------------------------------------
;; generalized operators to accommodate <carrays> and others
;;

;; -------------------------------------------------------------------
(defmethod coerce-to-vector ((v vector))
  v)

(defmethod coerce-to-vector ((lst list))
  (coerce lst 'vector))

(defmethod coerce-to-vector ((a array))
  (make-array (array-total-size a)
              :element-type (array-element-type a)
              :displaced-to a))

(defmethod coerce-to-vector ((cv c-arrays:<carray>))
  (coerce-to-vector (c-arrays:convert-to-lisp-object cv)))

;;---------
(defmethod length-of (arg)
  (length arg))

(defmethod length-of ((arg array))
  (array-total-size arg))

(defmethod length-of ((arg ca:<carray>))
  (ca:carray-total-size arg))

;;---------
(defmethod vmax-of (arg)
  (vmax arg))

(defmethod vmax-of ((arg array))
  (loop for ix from 0 below (array-total-size arg)
        maximize (row-major-aref arg ix)))

(defmethod vmax-of ((arg ca:<carray>))
  (loop for ix from 0 below (ca:carray-total-size arg)
        maximize (ca:row-major-caref arg ix)))

;;---------
(defmethod vmin-of ((arg sequence))
  (vmin arg))

(defmethod vmin-of ((arg array))
  (loop for ix from 0 below (array-total-size arg)
        minimize (row-major-aref arg ix)))

(defmethod vmin-of ((arg ca:<carray>))
  (loop for ix from 0 below (ca:carray-total-size arg)
        minimize (ca:row-major-caref arg ix)))

;;---------
(defun vextrema-of (arg)
  (vextrema (coerce-to-vector arg)))

;;---------
(defmethod array-total-size-of (arg)
  (array-total-size arg))

(defmethod array-total-size-of ((arg ca:<carray>))
  (ca:carray-total-size arg))

;;---------
(defmethod array-dimension-of (arg n)
  (array-dimension arg n))

(defmethod array-dimension-of ((arg ca:<carray>) n)
  (ca:carray-dimension arg n))

;;---------
(defmethod aref-of (arg &rest indices)
  (apply #'aref arg indices))

(defmethod aref-of ((arg ca:<carray>) &rest indices)
  (apply #'ca:caref arg indices))

;;---------
(defmethod row-major-aref-of (arg ix)
  (row-major-aref arg ix))

(defmethod row-major-aref-of ((arg ca:<carray>) ix)
  (ca:row-major-caref arg ix))

;;---------
(defmethod subseq-of (arg start &optional end)
  (subseq arg start end))

(defmethod subseq-of ((arg array) start &optional end)
  (let* ((limit (array-total-size arg))
         (nel   (- (or end limit) start))
         (ans   (make-array nel :element-type (array-element-type arg))))
    (loop for ix from start below (or end limit)
          for jx from 0
          do
          (setf (aref ans jx) (row-major-aref arg ix)))
    ans))

(defmethod subseq-of ((arg ca:<carray>) start &optional end)
  (let* ((limit  (ca:carray-total-size arg))
         (nel    (- (or end limit) start))
         (ans    (make-array nel
                             :element-type
                             (cond ((ca:is-float-array  arg) 'single-float)
                                   ((ca:is-double-array arg) 'double-float)
                                   (t 'bignum))
                             )))
    (loop for ix from start below (or end limit)
          for jx from 0
          do
          (setf (aref ans jx) (ca:caref arg ix)))
    ans))
          
