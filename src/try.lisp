(in-package :cl-user)

(defun unfinished-zero-address ()
  (let ((a (hash:hash/256 "abc")))
    (let ((a-vec (vec-repr::ub8v-vec (hash:hash-val a))))
      (let ((a-copy-vec (copy-seq a-vec)))
        (assert (= 32 (length a-copy-vec))) ;; not true if hash/256 changes
        (fill a-copy-vec 0)
        (setf (hash-val
        a-copy-vec))))))

(defun zero-address ()
  (let* ((h (hash:hash/256 :test))
         (vec (vec-repr:bev-vec h))
         (dum (copy-seq vec)))
    (fill dum 0)
    (setf (slot-value h 'hash::val) (vec-repr:bev dum))
    h))