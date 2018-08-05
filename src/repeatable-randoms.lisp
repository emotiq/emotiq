(in-package :emotiq/random)

;; initial cut of code supplied by svspires

;;; Initialize random seed to a random value for the first time.
;;; Only do this once, to create a brand new random seed.

(defun init-random ()
  "after calling this, (RANDOM 100) will return the same sequence of pseudo-random numbers on each test run"
  ;; see test-random below
  ;; see Rationale for [Funtion] make-random-state in https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node133.html
  (setf *random-state* (make-random-state t))
  ;;; Save initial seed
  (unless (probe-file "~/random-state-ccl.lisp")
    (with-open-file (s "~/random-state-ccl.lisp" :direction :output :if-exists
                       :supersede)
      (format s "~S" *random-state*)))
  ;;; Retrieve initial seed
  (with-open-file (s "~/random-state-ccl.lisp" :direction :input)
    (setf *random-state* (read s))))
  
;;; NOTE: Initial seed must be retrieved by same CL implementation it was
;;;  written with, and usually the same version thereof, or this won't work.
;;;  That's why I added "ccl" to the name of the file. You'll have to write a separate
;;;  file for Lispworks--it's not possible to write a seed with CCL and read it
;;;  with Lispworks, or vice-versa. But as long as you stick to the same implementation
;;;  for reading and writing the file, you'll get repeatable random sequences.

(defun test-random ()
  (emotiq/random:init-random)
  (let ((list1 (dotimes (i 10) (random 100))))
    (let ((list2 (dotimes (i 10) (random 100))))
      (mapc #'(lambda (i j)
                (unless (= i j)
                  (error "random numbers don't match")))
            list1 list2)))
  t)