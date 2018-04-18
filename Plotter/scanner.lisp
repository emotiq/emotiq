
(in-package :plotter)

;;-------------------------------------------------------------------
;; Abstract superclass <scanner> represent objects that respond to the NEXT-ITEM method
;;

(defclass <scanner> ()
  ())

(defclass <limited-scanner> (<scanner>)
  ((limit  :accessor scanner-limit    :initarg :limit)
   (pos    :accessor scanner-position :initform 0)))

(defclass <counting-scanner> (<limited-scanner>)
  ())

(defclass <vector-scanner> (<limited-scanner>)
  ((vec  :accessor scanner-vector :initarg :vector)))

(defclass <list-scanner> (<limited-scanner>)
  ((lst        :accessor scanner-list :initarg :list)
   (lst-backup :accessor scanner-list-backup)))

(defclass <array-scanner> (<limited-scanner>)
  ((arr  :accessor scanner-array :initarg :array)))

(defclass <carray-scanner> (<limited-scanner>)
  ((arr  :accessor scanner-array :initarg :array)))

;; ===============
(defmethod make-scanner ((limit integer) &key (max-items limit))
  (make-instance '<counting-scanner>
                 :limit (min limit max-items)))

(defmethod make-scanner ((vec vector) &key (max-items (length vec)))
  (make-instance '<vector-scanner>
                 :limit   (min (length vec) max-items)
                 :vector  vec))

(defmethod make-scanner ((lst list) &key (max-items (length lst)))
  (make-instance '<list-scanner>
                 :list  lst
                 :limit (min (length lst) max-items)))

(defmethod initialize-instance :after ((self <list-scanner>)
                                       &rest args &key &allow-other-keys)
  (setf (scanner-list-backup self) (scanner-list self)))

(defmethod make-scanner ((arr array) &key (max-items (array-total-size arr)))
  (make-instance '<array-scanner>
                 :array  arr
                 :limit  (min (array-total-size arr) max-items)))

(defmethod make-scanner ((arr ca:<carray>) &key (max-items (ca:carray-total-size arr)))
  (make-instance '<carray-scanner>
                 :array  arr
                 :limit  (min (ca:carray-total-size arr) max-items)))

;; ===============
;; All scanners pass through NIL as the terminal value
(defmethod next-item ((cscanner <counting-scanner>))
  (with-accessors ((position   scanner-position)
                   (limit      scanner-limit   )) cscanner
    (let ((ans position))
      (when (< ans limit)
        (incf position)
        ans)
      )))

(defmethod next-item ((lscanner <list-scanner>))
  (with-accessors ((limit    scanner-limit   )
                   (position scanner-position)
                   (its-list scanner-list    )) lscanner
    (when (< position limit)
      (incf position)
      (pop its-list))
    ))

(defmethod next-item ((vscanner <vector-scanner>))
  (with-accessors ((position   scanner-position)
                   (limit      scanner-limit   )
                   (its-vector scanner-vector  )) vscanner
  
  (when (< position limit)
    (prog1
        (aref its-vector position)
      (incf position)))
  ))

(defmethod next-item ((ascanner <array-scanner>))
  (with-accessors ((position  scanner-position)
                   (limit     scanner-limit   )
                   (its-array scanner-array   )) ascanner
    (when (< position limit)
      (prog1
          (row-major-aref its-array position)
        (incf position)))
    ))

(defmethod next-item ((cascanner <carray-scanner>))
  (with-accessors ((position  scanner-position)
                   (limit     scanner-limit   )
                   (its-array scanner-array   )) cascanner
    (when (< position limit)
      (prog1
          (ca:row-major-caref its-array position)
        (incf position)))
    ))

;; ===============
(defmethod reset-scanner ((scanner <limited-scanner>))
  (setf (scanner-position scanner) 0))

(defmethod reset-scanner :after ((scanner <list-scanner>))
  (setf (scanner-list scanner) (scanner-list-backup scanner)))

;; ===============
(defclass <transformer> (<scanner>)
  ((src   :accessor transformer-source  :initarg :source)
   (xform :accessor transformer-xform   :initarg :xform)))

(defmethod make-transformer ((src <scanner>) (xform function))
  (make-instance '<transformer>
                 :source src
                 :xform  xform))

(defmethod next-item ((xf <transformer>))
  ;; pass along NIL as a terminal value
  (with-accessors  ((source   transformer-source)
                    (xform    transformer-xform )) xf
    (let ((item (next-item source)))
      (when item
        (funcall xform item)))
    ))

(defmethod reset-scanner ((xf <transformer>))
  (reset-scanner (transformer-source xf)))

;; ===============

(defclass <pair-scanner> (<scanner>)
  ((xsrc   :accessor pair-scanner-xsrc   :initarg :xsrc)
   (ysrc   :accessor pair-scanner-ysrc   :initarg :ysrc)
   (pair   :accessor pair-scanner-values :initform (make-array 2))
   ))

(defmethod make-pair-scanner ((xs <scanner>) (ys <scanner>))
  (make-instance '<pair-scanner>
                 :xsrc  xs
                 :ysrc  ys
                 ))

(defmethod next-item ((pairs <pair-scanner>))
  (with-accessors ((xs    pair-scanner-xsrc  )
                   (ys    pair-scanner-ysrc  )
                   (pair  pair-scanner-values)) pairs
    (let* ((x (next-item xs))
           (y (next-item ys)))
      (when (and x y)
        (setf (aref pair 0) x
              (aref pair 1) y)
        pair))
    ))

(defmethod reset-scanner ((pairs <pair-scanner>))
  (reset-scanner (pair-scanner-xsrc pairs))
  (reset-scanner (pair-scanner-ysrc pairs)))

;; -----------------------------------------------------

(defmethod do-with-pairs ((pairs <pair-scanner>) fn)
  (reset-scanner pairs)
  (loop for pair = (next-item pairs)
        while pair
        do
        (funcall fn (aref pair 0) (aref pair 1))))

(defmacro with-pairs ((pairs x y) &body body)
  `(do-with-pairs ,pairs
                  (lambda (,x ,y)
                    ,@body)))


(defmethod do-with-scanner ((scanner <scanner>) fn)
  (reset-scanner scanner)
  (loop for x = (next-item scanner)
        while x
        do
        (funcall fn x)))

(defmacro with-scanner ((scanner x) &body body)
  `(do-with-scanner ,scanner
                    (lambda (,x)
                      ,@body)))
