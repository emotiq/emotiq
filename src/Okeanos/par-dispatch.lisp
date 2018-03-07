#|
The MIT License

Copyright (c) 2008 Refined Audiometrics Laboratory, LLC

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

(defstruct workqs
  (lock (mp:make-lock))
  (cvar (mp:make-condition-variable
         :name "WorkQueue"))
  (parq (mp:make-mailbox))
  serqs)

(defstruct serq
  (q   (mp:make-mailbox))
  (rdy t))

(defvar *workqs* (make-workqs))

(defun make-serial-dispatch-queue ()
  (let ((serq (make-serq)))
    (mp:with-lock ((workqs-lock *workqs*))
      (push serq (workqs-serqs *workqs*)))
    serq))

;; -----------------------------------------------------------------
;; Worker Threads

(defun apply-safely (fn &rest args)
  #f
  (ignore-errors
    (apply fn args)))

(defun mb-read-nowait (mbox)
  #f
  (mp:mailbox-read mbox nil 0))

(defun look-for-work ()
  #f
  (um:nlet-tail iter ((serq nil))
    (when serq
      (um:if-let (w (mb-read-nowait (serq-q serq)))
          (progn
            (apply 'apply-safely (car w) (cdr w))
            (iter serq))
        (setf (serq-rdy serq) t)))
    (let* ((newq nil)
           (lock (workqs-lock *workqs*))
           (wrk  (loop
                  (mp:with-lock (lock)
                    (let ((w (or (mb-read-nowait (workqs-parq *workqs*))
                                 (find-if (lambda (serq)
                                            (um:when-let (w (and (serq-rdy serq)
                                                                 (mb-read-nowait (serq-q serq))))
                                              (setf newq serq
                                                    (serq-rdy serq) nil)
                                              w))
                                          (workqs-serqs *workqs*))
                                 )))
                      (if w
                          (return w)
                        (mp:condition-variable-wait (workqs-cvar *workqs*) lock))
                       ))) ))
      (apply 'apply-safely (car wrk) (cdr wrk))
      (iter newq) )))

(defun launch-stable-of-threads (&optional (n 16))
  (loop for ix from 1 to n do
        (mp:process-run-function (format nil "Dispatch-~A" ix) nil 'look-for-work)))

;; -------------------------------------------------------------
;; Asynchronous Dispatch

(defun dispatch-par (fn &rest args)
  #f
  (mp:mailbox-send (workqs-parq *workqs*) (cons fn args))
  (mp:condition-variable-signal (workqs-cvar *workqs*)))

(defun dispatch-ser (serq fn &rest args)
  #f
  (mp:mailbox-send (serq-q serq) (cons fn args))
  (mp:condition-variable-signal (workqs-cvar *workqs*)))

(defmacro with-parallel-dispatch ((&optional serq) &body body)
  `(do-with-parallel-dispatch ,serq (lambda () ,@body)))

(defun do-with-parallel-dispatch (serq fn &rest args)
  #f
  (if serq
      (apply 'dispatch-ser serq fn args)
    (apply 'dispatch-par fn args)))

(defmacro with-parallel-clauses (&rest clauses)
  `(progn
     ,@(mapcar (lambda (clause)
                 `(with-parallel-dispatch () ,clause))
               clauses)))

;; -------------------------------------------------

(defmethod discard-serial-dispatch-queue ((serq serq))
  (with-parallel-dispatch (serq)
    (mp:with-lock ((workqs-lock *workqs*))
      (um:deletef (workqs-serqs *workqs*) serq))))

;; -------------------------------------------------
;; Parallel dispatch with sync'able return values

(defstruct sync-err
  exn)

(defun make-protected-function (fn)
  #f
  (lambda (&rest args)
    (multiple-value-bind (ans exn)
        (ignore-errors
          (multiple-value-list (apply fn args)))
      (if exn
          (make-sync-err exn)
        ans))))

(defun do-with-sync-dispatch (serq fn &rest args)
  #f
  (let* ((mbox (mp:make-mailbox))
         (gfn  (lambda ()
                 (mp:mailbox-send
                  mbox
                  (apply (make-protected-function fn) args)) )))
    (do-with-parallel-dispatch serq gfn)
    mbox))

(defmacro with-sync-dispatch ((&optional serq) &body body)
  `(do-with-sync-dispatch ,serq (lambda () ,@body)))

(defun get-dispatch-result (mbox)
  #f
  (let ((ans (mp:mailbox-read mbox)))
    (if (sync-err-p ans)
        (error (sync-err-exn ans))
      (values-list ans))) )

(defmacro with-sync-dispatch-clauses (&rest clauses)
  `(list ,@(mapcar (lambda (clause)
                     `(with-sync-dispatch () ,clause))
                   clauses)))

(defun get-dispatch-results (mboxes)
  #f
  (mapcar 'get-dispatch-result mboxes))

;; ---------------------------------------------------------------
