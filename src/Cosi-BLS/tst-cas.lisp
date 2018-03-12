
(in-package :cosi)

#+:ALLEGRO
(defun tstcas (&optional (n 1000000))
  (let ((cell (list t)))
    (loop repeat 3 do
	  (time 
	   (loop repeat n do
		 (when (excl:atomic-conditional-setf (car cell) t nil)
		   t))))
    (loop repeat 3 do
	  (time
	   (loop repeat n do
		 (when nil
		   t))))))


#+:ALLEGRO
(defun tstlocks(&optional (n 1000000))
  (let ((lock (mpcompat:make-lock)))
    (loop repeat 3 do
	  (time
	   (loop repeat n do
		 (mpcompat:with-lock (lock)
		   (progn nil)))))
    (loop repeat 3 do
	  (time
	   (loop repeat n do
		 (progn
		   (progn nil)))))))

;; ------------------------------------------------

#+:LISPWORKS
(defun do-ustime (fnx fn0 &optional (n 1))
  (labels ((timeit (fn)
             (let ((start (usec:get-time-usec)))
               (progn ;; sys:with-other-threads-disabled
                 (loop repeat n do
                       (funcall fn))
                 (let ((stop (usec:get-time-usec)))
                   (- stop start))))))
    (let ((dtxs  (make-array 3))
          (dt0s  (make-array 3)))
      (loop for ix from 0 below 3 do
            (setf (aref dtxs ix) (timeit fnx)
                  (aref dt0s ix) (timeit fn0)))
      (let* ((mn  (vm:mean (map 'vector '- dtxs dt0s)))
             (sdx (vm:stdev dtxs))
             (sd0 (vm:stdev dt0s))
             (sd  (sqrt (/ (+ (* sdx sdx)
                              (* sd0 sd0))
                           2))))
        (values (float (/ mn n))
                (float (/ sd n)))
        ))))

#+:LISPWORKS
(defmacro ustime ((&optional (n 1)) formx form0)
  `(do-ustime (lambda ()
                ,formx)
              (lambda ()
                ,form0)
              ,n))

#+:LISPWORKS
(defun tstlocks(&optional (n 1000000))
  (let* ((lock (mpcompat:make-lock)))
    (multiple-value-bind (mn sd)
        (ustime (n)
                (mpcompat:with-lock (lock)
                  (progn nil))
                (progn
                  (progn nil)))
      (format t "~%Elapsed time: ~A(~A) [us/op]" mn sd)
      (values mn sd))
    ))

#+:LISPWORKS
(defun tstcas (&optional (n 1000000))
  (let ((lst (list t)))
    (sys:with-other-threads-disabled
      (time 
       (loop repeat n do
             ;; (mpcompat:CAS (car lst) nil t)
             (progn nil)
             )))))