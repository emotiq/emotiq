#|
The MIT License

Copyright (c) 2017-2018 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :um)

;; ------------------------------------------
;; A MONITOR is a region of code under control of a lock. Only one
;; thread at a time can be executing any one of the clauses of a
;; MONITOR.
;;
;; The clauses are written, just like a defun, with a name, and
;; arglist, and a body.
;;
;; Each clause will become a defun aimed at the shared critical
;; region.
;;
;; NOTE: rather than using (&rest clauses) we use (clauses) as the
;; argument in order to gain the formatting support of the editor.
;; DEFMONITOR should be written like an FLET with all clauses enclosed
;; in an outer list.

#+:LISPWORKS
(defmacro! critical-section (&body body)
  `(let ((,g!lock  (load-time-value (mp:make-lock))))
     (mp:with-lock (,g!lock)
       ,@body)))

#+:CLOZURE
(defmacro! critical-section (&body body)
  `(let ((,g!lock  (load-time-value (ccl:make-lock))))
     (ccl:with-lock-grabbed (,g!lock)
        ,@body)))

#+:ALLEGRO
(defmacro! critical-section (&body body)
  `(excl:critical-section (:non-smp :without-interrupts)
                          ,@body))

#+(or sbcl)
(defmacro! critical-section (&body body)
  `(let ((,g!lock (load-time-value (sb-thread:make-mutex :name "Global critical section mutex lock"))))
     (sb-thread:with-recursive-lock (,g!lock)
        ,@body)))

#+:LISPWORKS
(defmacro! defmonitor (clauses)
  `(let* ((,g!lock (mp:make-lock))
          (,g!lam  (lambda (&rest ,g!args)
                     (mp:with-lock (,g!lock)
                       (dcase ,g!args
                         ,@clauses)))
                   ))
     ,@(mapcar (lambda (clause)
                 (let ((fname (first clause)))
                   `(defun ,fname (&rest ,g!fargs)
                      (apply ,g!lam ',fname ,g!fargs))))
               clauses)
     ))

#+:LISPWORKS
(editor:setup-indent "DEFMONITOR" 1 nil nil 'flet)

#+:ALLEGRO
(defmacro! defmonitor (clauses)
  `(let ((,g!lam  (lambda (&rest ,g!args)
                    (excl:critical-section (:non-smp :without-interrupts)
                       (dcase ,g!args
                         ,@clauses)))
                  ))
     ,@(mapcar (lambda (clause)
                 (let ((fname (first clause)))
                   `(defun ,fname (&rest ,g!fargs)
                      (apply ,g!lam ',fname ,g!fargs))))
               clauses)
     ))

#+:CLOZURE
(defmacro! defmonitor (clauses)
  `(let* ((,g!lock (ccl:make-lock))
          (,g!lam  (lambda (&rest ,g!args)
                     (ccl:with-lock-grabbed (,g!lock)
                       (dcase ,g!args
                         ,@clauses)))
                   ))
     ,@(mapcar (lambda (clause)
                 (let ((fname (first clause)))
                   `(defun ,fname (&rest ,g!fargs)
                      (apply ,g!lam ',fname ,g!fargs))))
               clauses)
     ))

#+(or sbcl)
(defmacro! defmonitor (clauses)
  `(let* ((,g!lock (sb-thread:make-mutex))
          (,g!lam  (lambda (&rest ,g!args)
                     (sb-thread:with-recursive-lock (,g!lock)
                       (dcase ,g!args
                         ,@clauses)))
                   ))
     ,@(mapcar (lambda (clause)
                 (let ((fname (first clause)))
                   `(defun ,fname (&rest ,g!fargs)
                      (apply ,g!lam ',fname ,g!fargs))))
               clauses)
     ))

;; ----------------------------------------------------------
#+:LISPWORKS
(defun do-with-spinlock (cons fn)
  (unwind-protect
      (progn
        (do ()
            ((and (eq nil (car cons))
                  ;; Spin on fetch until we think we have a chance of
                  ;; succeeding with the CAS. This avoids excessive
                  ;; bus traffic.
                  (sys:compare-and-swap (car cons) nil mp:*current-process*))))
        (funcall fn))
    (sys:compare-and-swap (car cons) mp:*current-process* nil)))

#+:LISPWORKS
(defmacro! with-spinlock (cons &body body)
  `(flet ((,g!body ()
            ,@body))
     (declare (dynamic-extent #',g!body))
     (do-with-spinlock ,cons #',g!body)))

#+:LISPWORKS
(defmacro! defsponitor (clauses)
  `(let* ((,g!lock (list nil))
          (,g!lam  (lambda (&rest ,g!args)
                     (with-spinlock ,g!lock
                       (dcase ,g!args
                         ,@clauses)))
                   ))
     ,@(mapcar (lambda (clause)
                 (let ((fname (first clause)))
                   `(defun ,fname (&rest ,g!fargs)
                      (apply ,g!lam ',fname ,g!fargs))))
               clauses)
     ))

#+:LISPWORKS
(editor:setup-indent "DEFSPONITOR" 1 nil nil 'flet)

;; ----------------------------------------------------------

#|
(defsponitor ()
  ((ensure-access ()
     (doit))
   (diddle-access (arg)
     (doit2 arg))))
==>
(LET* ((#:LOCK46671 (MP:MAKE-LOCK))
       (#:LAM46670  (LAMBDA (&REST #:ARGS46669)
                      (MP:WITH-LOCK (#:LOCK46671)
                        (DCASE #:ARGS46669
                          (ENSURE-ACCESS NIL
                                         (DOIT))
                          (DIDDLE-ACCESS (ARG)
                                         (DOIT2 ARG))
                          )))
                    ))
  (DEFUN ENSURE-ACCESS (&REST #:FARGS46668)
    (APPLY #:LAM46670 'ENSURE-ACCESS #:FARGS46668))
  (DEFUN DIDDLE-ACCESS (&REST #:FARGS46668)
    (APPLY #:LAM46670 'DIDDLE-ACCESS #:FARGS46668)))
|#
