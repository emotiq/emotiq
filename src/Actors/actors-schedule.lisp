;; Actors-machines.lisp -- Erlang RECV for Actors, using OPTIMA:MATCH syntax
;;
;; DM/RAL  12/17
;; -----------------------------------------------------------
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

(in-package #:actors)

;; ----------------------------------------------------------------

;; -------------------------------------------------------
;; SCHEDULE-TIMEOUT-ACTION -- a macro to hide the gory details...

(defun do-schedule-timeout-action (timeout fn)
  (spawn (lambda ()
           (recv
             :TIMEOUT    timeout
             :ON-TIMEOUT (funcall fn)))))

(defmacro schedule-timeout-action (timeout &body body)
  ;; a macro to schedule an action after timeout.
  ;;
  ;; Action should not bang on any local state of the Actor making
  ;; this call, because it executes in a different Actor, and that
  ;; would violate single-thread semantics.
  `(do-schedule-timeout-action ,timeout (lambda ()
                                          ,@body)))


(defun do-schedule-after (timeout fn)
  (=bind ()
      (schedule-timeout-action timeout
        (=values))
    (funcall fn)))

(defmacro schedule-after (timeout &body body)
  ;; a macro to schedule Actor body code to be performed after timeout.
  `(do-schedule-after ,timeout (lambda ()
                                 ,@body)))
