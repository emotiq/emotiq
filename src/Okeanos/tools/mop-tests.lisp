
(defclass c1 ()
  ((a :initform 'c1)))

(defclass c2 ()
  ((a :initform 'c2)))

(defclass c3 (c1)
  ((b :initform 'c3)))


(defclass cx (c3 c2)
  ((c :initform 'cx)))


(setf x (make-instance 'cx))
(slot-value x 'a)

;; proves depth-first search for inherited slots
