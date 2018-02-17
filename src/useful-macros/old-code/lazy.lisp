;; Lazy.lisp -- class for lazy evaluation and memoization of results
;;
;; DM/MCFA  10/01
;; DM/RAL 12/08 -- more efficient implementation
;; -----------------------------------------------------------

(in-package "LAZY")

(defclass lazy-form ()
  ((expr-fn :reader expr-fn :initarg :expr-fn)))

(defclass memoized-lazy-evaluation ()
  ((expr :reader force :initarg :forced)))

(defmacro lazy (expr)
  `(make-instance 'lazy-form
		  :expr-fn (lambda () ,expr)))

(defmethod force (val)
  val)

(defmethod force ((val lazy-form))
  (let ((ans (funcall (expr-fn val))))
    (change-class val 'memoized-lazy-evaluation
		  :forced ans)
    ans))

