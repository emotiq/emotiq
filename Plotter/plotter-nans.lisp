
(in-package :plotter)

;; ------------------------------------------
;; infinitep true if non-zero numeric arg with zero reciprocal
;; works for plus or minus infinity. As a secondary effect,
;; the truth value will be the largest double precision value.
(defun infinitep (v)
  (and (not (zerop v))
       (zerop (/ v))
       (if (plusp v)
           most-positive-double-float
         most-negative-double-float)))

;; nanp true if numeric v not equal to itself
(defun nanp (v)
  (/= v v))

(defun inf-nan-p (v)
  (or (infinitep v)
      (nanp v)))

(defun simple-real-number (v)
  (and (realp v)
       (not (inf-nan-p v))))

(defun real-eval-with-nans (fn &rest args)
  (handler-case
      (let ((v (apply fn args)))
        (if (simple-real-number v)
            v
          :nan))
    (arithmetic-error (err)
      (declare (ignore err))
      :nan)))

(defun nan-or-infinite-p (v)
  (not (simple-real-number v)))

(defun acceptable-for-log (v)
  (and (simple-real-number v)
       (plusp v)))

;; ---------------------------------------------
;; filtering out nans and infinities
;;

(defun coerce-to-dfloat-vector (xs)
  ;; xs should already be a collection of double-float values
  (make-array (length xs)
              :element-type 'double-float
              :initial-contents xs))

(defun try-dfloat (x)
  (if (realp x)
      (dfloat x)
    x))

(defun filter-xs-ys (xs ys xfn yfn)
  ;; remove paired values if either of the (x,y) pair is nan or infinite
  (let ((filt-xs (priq:make-unsafe-fifo))
        (filt-ys (priq:make-unsafe-fifo)))
    (um:lc ((:do
                (priq:addq filt-xs xf)
                (priq:addq filt-ys yf))
            ((x y) <-// xs ys)
            (xf <-f (try-dfloat x))
            (yf <-f (try-dfloat y))
            (and (funcall xfn xf)
                 (funcall yfn yf))))
    (values (coerce-to-dfloat-vector (priq:contents filt-xs))
            (coerce-to-dfloat-vector (priq:contents filt-ys)))
    ))

(defun filter-xs (xs fn)
  (coerce-to-dfloat-vector
   (um:lc (xf
           (x <- xs)
           (xf <-f (try-dfloat x))
           (funcall fn xf)))))

(defun filter-nans-and-infinities (xs)
  ;; remove values from the sequence if they are nans or infinities
  (filter-xs xs #'simple-real-number))

;; ----------------------------------------------------------------------
;; filter out potential nans and infinities for logarithmic axes

(defun acceptance-test (islog)
  (if islog
      #'acceptable-for-log
    #'simple-real-number))

(defun filter-potential-x-y-nans-and-infinities (xs ys xlog ylog)
  ;; remove paired values if either of the (x,y) pair is nan or infinite
  (filter-xs-ys xs ys (acceptance-test xlog) (acceptance-test ylog)))

(defun filter-potential-nans-and-infinities (xs islog)
  ;; remove values from the sequence if they are nans or infinities
  (filter-xs xs (acceptance-test islog)))

