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

(defun find-kw-assoc (kw lst)
  (let ((test (curry #'eq kw)))
    (multiple-value-bind (pre post)
        (um:split-if test lst)
      (if post
          (values (cadr post) t
                  (nlet-tail flush ((pre  pre)
                                    (tl   (cddr post)))
                    (multiple-value-bind (hd new-tl) (um:split-if test tl)
                      (let ((new-pre (nconc pre hd)))
                        (if new-tl
                            (flush new-pre (cddr new-tl))
                          new-pre)
                        ))) )
        (values nil nil lst))
      )))

(defun parse-clauses (clauses)
  (multiple-value-bind (timeout timeout-present-p clauses)
      (find-kw-assoc :TIMEOUT clauses)
    (multiple-value-bind (on-timeout on-timeout-present-p clauses)
        (find-kw-assoc :ON-TIMEOUT clauses)
      (values clauses
              timeout    timeout-present-p
              on-timeout on-timeout-present-p)
      )))

(defun parse-pattern-clauses (clauses)
  `(lambda-match  ;; returns NIL on no matching message, as we need
     ,@(mapcar (lambda-ematch
                 ((list* pat 'when pred body)
                  `((guard ,pat ,pred)
                    (lambda () ,@body)))
                 ((list* pat 'unless pred body)
                  `((guard ,pat (not ,pred))
                    (lambda () ,@body)))
                 ((list* pat body)
                  `(,pat (lambda () ,@body)))
                 )
               clauses)) )

(defun parse-recv-clauses (clauses)
  (multiple-value-bind (new-clauses
                        timeout-expr      timeout-present-p
                        on-timeout-clause on-timeout-present-p)
      (parse-clauses clauses)
    (declare (ignore timeout-present-p))
    (let* ((conds-fn       (parse-pattern-clauses new-clauses))
           (timeout-fn     (when on-timeout-present-p
                             `(lambda ()
                                ,on-timeout-clause))))
      (values conds-fn timeout-fn timeout-expr))))

;; -----------------------------------------------------------
;; RECV -- selective retrieval of messages
;;
;; Any messages not matching one of the RECV clauses gets stashed in a
;; FIFO queue for later replay after the RECV clause either succeeds
;; or times out.
;;
;; Even though each RECV clause is entirely asynchronous, a series of
;; successive RECV clauses will apply successively. So syntax will
;; match semantics in this case. (Not generally true of asynchronous
;; clauses)

(defmacro recv (&rest clauses)
  ;;
  ;; a RECV uses Optima:MATCH style patterns and clauses.
  ;;
  ;; RECV receives and processes one qualifying message, or gets timed
  ;; out. Messages arriving at the Actor's mailbox which do not
  ;; qualify for any of the RECV clauses will be stashed during the
  ;; waiting period. RECV operates asynchronously and does not block
  ;; waiting.
  ;;
  ;; After RECV either times out or receives a qualifying message, the
  ;; body forms of the Actor that follow the RECV form will be in
  ;; effect, the stashed messages will be replayed, and the Actor will
  ;; be using its original behavior on those and all future messages.
  ;;
  ;; If there is a :TIMEOUT expression inside the RECV form, the Actor
  ;; will setup a timeout timer on that expression. If a qualifying
  ;; message does not arrive before the timer expires, a timeout will
  ;; occur. That timeout will execute the form listed after
  ;; :ON-TIMEOUT if given, or else a timeout error will be generated.
  ;;
  ;; In the macro, we parse the handler body to create a function
  ;; which takes a message and returns a fully deconstructed pattern
  ;; match closure, or nil. This allows us to cancel any pending
  ;; timeout if a message qualifies.
  ;;
  ;; An Actor containing a RECV form will not execute that form until
  ;; it receives some message that causes it to execute a branch of
  ;; code contained in the RECV form.
  ;;
  (multiple-value-bind (conds-fn timeout-fn timeout-expr)
      (parse-recv-clauses clauses)
    `(labels
         ((retry-recv ()
            (self-call
             :recv-setup-{204E1756-D84E-11E7-9D93-985AEBDA9C2A}
             ,conds-fn ,timeout-fn ,timeout-expr)))
       (retry-recv))
    ))

;; ----------------------------------------------------------------------------------

;; -------------------------------------------------------
;; SCHEDULE-TIMEOUT-ACTION -- a macro to hide the gory details...

(defun do-schedule-timeout-action (timeout fn)
  (spawn (lambda ()
           (recv
             :TIMEOUT    timeout
             :ON-TIMEOUT (funcall fn)))))

(defmacro schedule-timeout-action (timeout &body body)
  `(do-schedule-timeout-action ,timeout (lambda ()
                                          ,@body)))

