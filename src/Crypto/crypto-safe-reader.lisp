
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
        (*package*   (find-package :pbc)))
    (labels ((eval-it (inp)
               (cond ((atom inp) inp)
                     
                     ((eql (car inp) 'list)
                      ;; allow list input with prefix 'LIST
                      ;; (this no longer mandatory)
                      (mapcar #'eval-it (cdr inp)))

                     ((and (member (car inp) '(make-pkey make-skey make-sig))
                           (stringp (cadr inp))
                           (every (um:rcurry 'digit-char-p 16) (cadr inp))
                           (null (cddr inp)))
                      ;; one of (MAKE-PKEY str), (MAKE-SKEY str),
                      ;; (MAKE-SIG str), with hex string
                      (funcall (car inp) (cadr inp)))

                     (t
                      ;; allow everything else to be a list of one of
                      ;; the above, or NIL
                      (mapcar #'eval-it inp))
                     )))
      (eval-it (read in-stream nil nil nil))
      )))
