;; lock-management.lisp -- Read / Write Locking
;; --------------------------------------------------------------------------------------
;;
;; DM/RAL  07/09-04/10
;; --------------------------------------------------------------------------------------
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
;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------


;; --------------------------------------------------------------------
;; The Lock-Manager

(defstruct ld-state
  (lock  (mpcompat:make-lock :sharing    t
                             :recursivep t)))

(defun get-db-lock ()
  (ld-state-lock (database-locks *current-okeanos-db*)))


(define-condition read-lock-held-too-long  (bfly-exn-timeout)
  ())

(define-condition write-lock-held-too-long (bfly-exn-timeout)
  ())

(defun schedule-self-timer-with-killer (duration killer-fn)
  (let ((tls (get-tls)))
    (if (tls-timer tls)
        (incf (tls-timer-refc tls))
      ;; else
      (let ((timer (mp:make-timer
                    'mp:process-interrupt (mpcompat:current-process) killer-fn) ))
        (setf (tls-timer tls)      timer
              (tls-timer-refc tls) 1)
        (mp:schedule-timer-relative timer duration)
        ))
    ))

(defun release-self-timer ()
  (let ((tls (get-tls)))
    (when (zerop (decf (tls-timer-refc tls)))
      (mp:unschedule-timer (tls-timer tls))
      (setf (tls-timer tls) nil)) ))

(defun do-with-self-timer (fn max-duration exn-type)
  (schedule-self-timer-with-killer max-duration
                                   (deferred
                                       (error (make-condition exn-type))))
  (hcl:unwind-protect-blocking-interrupts-in-cleanups
      (funcall fn)
    (release-self-timer)))

(defun adjusted-timeout (timeout)
  (and timeout
       (max timeout 0.001)))

(defun do-with-read-write-lock (fn timeout direction)
  (if (or (null *current-okeanos-db*)
          *shutting-down*)
      (funcall fn)
    ;; else
    (let ((lock  (get-db-lock))
          (tmout (adjusted-timeout timeout)))

      (ecase direction
        (:read
         (mp:with-sharing-lock (lock nil tmout)
           (do-with-self-timer fn (* 4 *timeout*) 'read-lock-held-too-long)))
      
        (:write
         (mp:with-exclusive-lock (lock nil tmout)
           (do-with-self-timer fn (* 2 *timeout*) 'write-lock-held-too-long)))
        ))))

(defun do-with-read-lock (fn timeout)
  (if (um:featurep :debugging)
      (funcall fn)
    (do-with-read-write-lock fn timeout :read)))
  
(defun do-with-write-lock (fn timeout)
  (if (um:featurep :debugging)
      (funcall fn)
    (do-with-read-write-lock fn timeout :write)))

(defmacro with-read-lock ((&key timeout) &body body)
  `(do-with-read-lock (deferred ,@body) ,timeout))

(defmacro with-write-lock ((&key timeout) &body body)
  `(do-with-write-lock (deferred ,@body) ,timeout))

#+:LISPWORKS
(editor:setup-indent "with-read-lock" 1)
#+:LISPWORKS
(editor:setup-indent "with-write-lock" 1)

;; ----------------------------------------------------------------------------
;; For remote connections
;; The server-side client proxy is a BFLY thread, responding to BFLY messages
;; It cannot perform a with-lock, but instead calls grab and release
;; Try to ensure safety by releasing the lock if anything goes wrong.

(defun schedule-proxy-self-timer (duration)
  (let* ((killer (deferred
                     (exit :ABNORMAL :write-lock-held-too-long))) )
    (schedule-self-timer-with-killer duration killer)))

(defun release-lock-on-process-kill (proc lock)
  (declare (ignore proc))
  (um:while (mp:lock-owned-by-current-process-p lock)
    (mp:process-exclusive-unlock lock)))

(defun grab-write-lock (&optional timeout)
    (let ((lock  (get-db-lock))
          (tmout (adjusted-timeout timeout)))
      (mp:process-exclusive-lock lock nil tmout)
      (mp:ensure-process-cleanup `(release-lock-on-process-kill ,lock))
      (schedule-proxy-self-timer (* 2 *timeout*))
      :ok))

(defun release-write-lock ()
  (when *current-okeanos-db*
    (let ((lock (get-db-lock)))
      (release-self-timer)
      (mp:process-exclusive-unlock lock)
      :ok)))
