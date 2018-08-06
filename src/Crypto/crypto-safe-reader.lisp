
(in-package :pbc)

(defun make-pkey (hex-str)
  (make-instance 'public-key
                 :value (bev (hex hex-str))))

(defun make-skey (hex-str)
  (make-instance 'secret-key
                 :value (bev (hex hex-str))))

(defun make-sig (hex-str)
  (make-instance 'signature
                 :value (bev (hex hex-str))))

(defmethod print-object ((obj public-key) out-stream)
  (if *print-readably*
      (format out-stream "~&(make-pkey ~S)" (hex-str obj))
    (call-next-method)))

(defmethod print-object ((obj secret-key) out-stream)
  (if *print-readably*
      (format out-stream "~&(make-skey ~S)" (hex-str obj))
    (call-next-method)))

(defmethod print-object ((obj signature) out-stream)
  (if *print-readably*
      (format out-stream "~&(make-sig ~S)" (hex-str obj))
    (call-next-method)))

(defun read-safely (in-stream)
  (let ((*read-eval* nil)
        (*package*   (find-package :crypto-safe-reader-package)))
    (eval (read in-stream nil nil nil))))
