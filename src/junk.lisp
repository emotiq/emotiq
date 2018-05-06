(in-package :emotiq-user)

(defparameter *ea* nil)
(defparameter *txn* nil)

(defun ed-neutral-point-p (pt)
  (optima:ematch pt
    ((ecc-pt- :x x)     (zerop x))
    ((ed-proj-pt- :x x) (zerop x))
    ))