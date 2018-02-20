
(in-package :ac)

(defun tst (&optional (niter #N100_000))
  (time
   (dotimes (ix niter)
     (with-futures (&rest args)
         (nil nil nil nil nil
          nil nil nil nil nil)
       args))))

(defun tst2 (&optional (niter #N1_000_000))
  (time
   (dotimes (ix niter)
     (print (sin ix)))))

(defmacro nest (&rest r)
  (reduce (lambda (o i)
            `(,@o ,i))
          r
          :from-end t))

(nest
 (let ((x 15)))
 (let ((y 32)))
 (progn
   (doit)
   (didit 12)))

(trivia:match 15
  (x (print x)))

(lw:push-end-new 15 lst)

(defun doit (x)
  (print (+ x 3)))

(lw:defadvice (doit doit-1 :around)
    (x)
  (lw:call-next-advice (+ x 2)))

;; ----------------------------------------------

(sys:call-system-showing-output "sysctl -n hw.logicalcpu")
(with-open-stream (s (sys:open-pipe "sysctl -n hw.logicalcpu"))
  (parse-integer (read-line s nil nil)))

