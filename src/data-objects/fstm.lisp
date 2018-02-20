;; fstm.lisp -- Dynamic STM - Software Transactional Memory
;;
;; Adapted from UCAM-CL-TR-579 U.Cambridge Tech Report 579,
;; "Practical lock-freedom" by Keir Fraser, Feb 2004
;;
;; See also paper: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.59.8787&rep=rep1&type=pdf
;;  "Software Transactional Memory for Dynamic-Sized Data Structures",
;;  Herlihy, Luchangco, Moir, Sherer
;;
;; DM/RAL  03/17
;; --------------------------------------------------------
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

#|
(defpackage #:fstm
  (:use #:common-lisp)
  (:import-from #:ref
   #:mref
   #:ref-value
   #:atomic-incf)
  (:export
   #:make-var
   #:atomic
   #:open-for-read
   #:open-for-write
   #:release-read
   #:abort-transaction
   #:retry
   #:clone
   ))
|#

(in-package #:fstm)

;; ----------------------------------------------------

(defvar *current-transaction* nil)

(defvar *tid-counter*  0)

(defun next-tid ()
  (sys:atomic-fixnum-incf (the fixnum *tid-counter*)))

(defvar *vid-counter*  0)

(defun next-vid ()
  (sys:atomic-fixnum-incf (the fixnum *vid-counter*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct transaction
    (id      (next-tid))
    (state   :undecided)
    ro-list
    (rw-map  (maps:empty))))

(defvar *ncomms*  0)  ;; cumm nbr successful commits
(defvar *nrolls*  0)  ;; cumm nbr retrys

;; ----------------------------------------------------

(define-condition retry-exn ()
  ())

(define-condition abort-exn ()
  ((arg  :reader abort-exn-retval :initarg :retval :initform nil)))

(defun retry ()
  (sys:atomic-fixnum-incf *nrolls*)
  (error (load-time-value
          (make-condition 'retry-exn)
          t)))

(defun abort-transaction (&optional retval)
  (if retval
      (error (make-condition 'abort-exn
                             :retval retval))
    ;; else
    (error (load-time-value
            (make-condition 'abort-exn)
            t))))

;; ----------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (var
              (:constructor make-var (&optional val)))
    (id  (next-vid))
    val))

;; ------------------------------
;; All objects subject to FSTM must define a FSTM:CLONE method
;;
;; CLONE is called when opening for write access.
;;
;; NOTE: These are all shallow (skeleton spine) copies Be careful not
;; to mutate any of the contained objects, but mutation of spine cells
;; is fine.  When operating on a copy, the use of single-thread
;; algorithms is fine. No other threads can see it.

(defmethod clone ((lst list))
  (copy-list lst))

(defmethod clone ((s structure-object))
  (copy-structure s))

(defmethod clone ((r mref))
  (mref (ref-value r)))

(defmethod clone ((v var))
  ;; Not a good idea to clone a var
  ;; Should always incorporate vars in some structured object
  ;; but never as the value of another var.
  (error "Attempt to clone a VAR"))

(defmethod clone ((s sets:tree))
  ;; immutable data object
  s)

(defmethod clone ((n number))
  n)

(defmethod clone ((c character))
  c)

(defmethod clone ((f function))
  f)

(defmethod clone ((s symbol))
  s)

(defmethod clone ((v vector))
  (copy-seq v))

(defmethod clone ((arr array))
  ;; potentially expensive
  ;; If arr is adjustable, the clone will be too.
  ;; If arr is displaced to another arr, clone will be a non-displaced copy
  ;; Only the structure is copied. If any elements are mutable objects
  ;; the cloned array will still point to those same mutable objects.
  ;; Be Careful!!
  (let* ((dims (array-dimensions arr))
         (adj  (adjustable-array-p arr))
         (eltp (array-element-type arr))
         (ans  (make-array dims
                           :adjustable adj
                           :element-type eltp))
         (src  (make-array (array-total-size arr)
                           :element-type eltp
                           :displaced-to arr))
         (dst  (make-array (array-total-size ans)
                           :element-type eltp
                           :displaced-to ans)))
    (replace dst src)
    ans))

;; ----------------------------------------------------

(declaim (inline trans<))

(defun trans< (trans1 trans2)
  (declare (type transaction trans1 trans2))
  (< (the fixnum (transaction-id trans1))
     (the fixnum (transaction-id trans2))))

;; -------------------------------------------------------------------

;; Here, it is known that other-trans is in the midst of a commit.  If
;; it is still in the reading / grabbing stage of commit, then we look
;; to see if we are also committing. If we are, we wouldn't be here
;; unless we had already grabbed all of our write cells. There is a
;; possibility that other-trans may also want some of those same write
;; cells, and the outcome of its commit is uncertain. To avoid an
;; infinite loop, if we are older than other-tran, then we abort other
;; tran.

;; Otherwise, whether we are committing or not, we help him commit, so
;; that we gain a definitive answer to his commit outcome. If he wants
;; some of our write cells, then

;; If it has proceeded past the point of grabbing all its write cells
;; and is now in :READ-PHASE, then we help it along.
;;
;; If trans (which is us) is also in a commit, and if we are older
;; than other-trans, then we attempt to abort other-trans. That will
;; either succeed, or else other-trans has already finished
;; committing. Either way it will have a status of :FAILED or
;; :SUCCEEDED.
;;
;; (NOTE: if we are in a commit, and in this routine, then we must
;; already be in :READ-PHASE)
;;
;; If we aren't in a commit, then we merely help other-trans finish
;; its commit.

(defun maybe-help (trans other-trans)
  (declare (transaction trans other-trans))
  (when (eq :read-phase (transaction-state other-trans))
    ;; other-trans is in the :read-phase of a commit
    (cond ((and (eq :read-phase (transaction-state trans)) ;; are we in :read-phase of commit too?
                (trans< trans other-trans))     ;; and are we older?
           ;; we are older - abort him
           (sys:compare-and-swap (transaction-state other-trans) :read-phase :failed))

          (t ;; either we are not in a commit, or we are younger
           (commit-transaction other-trans))
          )))

;; --------------------------------------

(defun obj-read (trans var)
  (declare (transaction trans)
           (var var))
  (let ((data (var-val var)))
    (when (transaction-p data)
      (let ((entry (maps:find (var-id var) (transaction-rw-map (the transaction data)))))
        (maybe-help trans data)
        (setf data (if (eq :successful (transaction-state data))
                       (ref-value (the mref (third entry)))  ;; new
                     (second entry))) ;; old
        ))
    data))

;; --------------------------------------

(defun commit-transaction (trans)
  (declare (transaction trans))
  (let ((rw-map  (transaction-rw-map trans)))

    (labels ((get-status (desired-state)
               (let ((state (transaction-state trans)))
                 (case state
                   (:failed     nil)
                   (:successful t)
                   (t
                    (sys:compare-and-swap (transaction-state trans) state desired-state)
                    (get-status desired-state))
                   )))
             
             (decide (desired-state)
               (let ((success (get-status desired-state)))
                 (labels ((restore (var old new-ref)
                            (sys:compare-and-swap (var-val var)
                                                  trans
                                                  (if success
                                                      (ref-value (the mref new-ref))
                                                    old))))
                   (maps:iter #'(lambda (k v)
                                  (declare (ignore k))
                                  (apply #'restore v))
                              rw-map)
                   (return-from commit-transaction success))))

             (acquire (var old new-ref)
               (unless (sys:compare-and-swap (var-val var) old trans)
                 (let ((data (var-val var)))
                   (cond ((eq data trans)
                          ;; break
                          )
                         
                         ((transaction-p data)
                          ;; someone else is committing, help them out
                          (commit-transaction data)
                          (acquire var old new-ref))
                         
                         (t
                          ;; didn't get it, can't get it
                          (decide :failed))
                         )))))

      (when (eq :undecided (transaction-state trans))
        (maps:iter #'(lambda (k v)
                       (declare (ignore k))
                       (apply #'acquire v))
                   rw-map)
        (sys:compare-and-swap (transaction-state trans) :undecided :read-phase))
      (when (eq :read-phase (transaction-state trans))
        (loop for (var val) in (the list (transaction-ro-list trans)) do
              (unless (eq val (obj-read trans var))
                (decide :failed))))
      (decide :successful)
      )))

;; -------------------------------------------------------------------------
;; operations on *CURRENT-TRANSACTION*

(defun open-for-read (var)
  (declare (var var))
  (let ((data (obj-read *current-transaction* var)))
    (push (list var data)
          (transaction-ro-list (the transaction *current-transaction*)))
    data))

(defun release-read (var)
  ;; releasing a var that was never opened is a benign error
  (um:deletef (transaction-ro-list (the transaction *current-transaction*))
              var
              :key  #'first
              :test #'eq))

(defun open-for-write (var)
  (declare (var var))
  ;; check if we already have it opened for writing
  (um:if-let (triple (maps:find (var-id var)
                                (transaction-rw-map (the transaction *current-transaction*))))
      (third triple) ;; mref of current new value
          
    ;; else - new open
    (let* ((old (um:if-let (pair (find var (the list (transaction-ro-list
                                                      (the transaction *current-transaction*)))
                                       :key  #'first
                                       :test #'eq))
                    (progn
                      (release-read var)
                      (second (the cons pair))) ;; current val
                  
                  ;; else - not in our read list
                  (obj-read *current-transaction* var)))

           (new-ref  (mref (clone old))))
      
      (setf (transaction-rw-map (the transaction *current-transaction*))
            (maps:add (var-id var) (list var old new-ref)
                      (transaction-rw-map (the transaction *current-transaction*))))
      new-ref)))

(defun commit ()
  (unless (commit-transaction *current-transaction*)
    (retry))
  (sys:atomic-fixnum-incf *ncomms*))

;; ------------------------------

(defun do-atomic (fn)
  (cond (*current-transaction*
         (funcall fn))
        
        (t
         (labels ((try-transaction ()
                    (return-from do-atomic
                      (multiple-value-prog1
                          (values (funcall fn) t)
                        (commit))))

                  (retry-loop ()
                    (let ((*current-transaction* (make-transaction)))
                      (handler-case
                          (try-transaction)
                        (retry-exn (exn)
                          (declare (ignore exn))
                          (retry-loop))
                        ))))

           (handler-case
               (retry-loop)
             (abort-exn (exn)
               (values (abort-exn-retval exn) nil))
             )))
        ))
  
(defmacro atomic (&body body)
  ;; return (values body t) if successful
  ;; else (values nil nil) if aborted
  `(do-atomic (lambda ()
                ,@body)))
       
;; -------------------------------------------------

;; ---------------------------------------------------
;; Test it out... hopefully lots of contention... yep!
#|
(progn
  (defun show-rolls (&optional (duration 1))
    (let ((pcnt (/ *nrolls* *ncomms* 0.01))
          (rate (/ *ncomms* duration)))
      (list :retrys *nrolls*
            :commits   *ncomms*
            :percent-retrys pcnt
            :commits-per-roll (if (zerop pcnt) :infinite (* 100 (/ pcnt)))
            :duration duration
            :commits-per-sec  rate)))
  
  (defun reset ()
    (setf *nrolls* 0)
    (setf *ncomms* 0))
  
  (defvar *a* (make-var 0))
  (defvar *b* (make-var 0))
  
  (defun check-invariant (&aux a b)
    (atomic
      (setf a (open-for-read *a*)
            b (open-for-read *b*)
            ))
    (if (= b (* 2 a))
        (format t "~%a = ~A, b = ~A  (~A)" a b mp:*current-process*)
      (bfly:log-info :SYSTEM-LOG "Invariant broken: A = ~A, B = ~A" a b)))
  
  (defun common-code (delta)
    (atomic
      (let* ((refa (open-for-write *a*))
             (refb (open-for-write *b*))
             (a    (+ delta (ref-value refa)))
             (b    (* 2 a)))
        (setf (ref-value refa) a
              (ref-value refb) b)
        )))

  (defvar *ct* 1000)
  
  (defun count-up ()
    (loop repeat *ct* do (common-code 1))
    (check-invariant)
    )
  
  (defun count-down ()
    (loop repeat *ct* do (common-code -1))
    (check-invariant)
    )
  
  (defun checker (&rest procs)
    (let ((start (usec:get-time-usec)))
      (loop while (some #'mp:process-alive-p procs)
            do (check-invariant))
      (let ((stop (usec:get-time-usec)))
        (bfly:log-info :SYSTEM-LOG (show-rolls (* 1e-6 (- stop start))))) ))
  
  (defun tst0 ()
    (bfly:log-info :SYSTEM-LOG "Start FSTM Test...")
    (setf *a* (make-var 0)
          *b* (make-var 0))
    (reset)
    (bfly:spawn #'checker
                :name :checker
                :args (mapcar #'bfly:pid-proc
                              (list (bfly:spawn #'count-down
                                                :name :up-counter)
                                    (bfly:spawn #'count-up
                                                :name :down-counter))))
    )
  
  (defun tst1 (&optional (ct 100000))
    ;; only one thread for no-contention timings
    (setf *ct* ct)
    (setf *a* (make-var 0)
          *b* (make-var 0))
    (reset)
    (let ((start (usec:get-time-usec)))
      (count-down)
      (let ((stop (usec:get-time-usec)))
        (show-rolls (* 1e-6 (- stop start))))
      ))
  
  (defun tst2 (&optional (ct 100000))
    (setf *ct* ct)
    (setf *a* (make-var 0)
          *b* (make-var 0))
    (reset)
    (let ((start (usec:get-time-usec))
          (ct 0)
          (down (bfly:spawn-link #'count-down
                                 :name :down-counter))
          (up   (bfly:spawn-link #'count-up
                                 :name :up-counter)))
      (loop until (= 2 ct)
            do
            (bfly:recv msg
              ((list :Exit-Message pid _ _)
               :when (or (eq pid down)
                         (eq pid up))
               (incf ct))
              ( _ )))
      
      (let ((stop (usec:get-time-usec)))
        (show-rolls (* 1e-6 (- stop start))))
      ))

  (defun tst3 (&optional (ct 100000))
    (setf *ct* ct)
    (setf *a* (make-var 0)
          *b* (make-var 0))
    (reset)
    (let ((start (usec:get-time-usec))
          (ct 0)
          (down (bfly:spawn-link #'count-down
                                 :name :down-counter))
          (up   (bfly:spawn-link #'count-up
                                 :name :up-counter))
          (down2 (bfly:spawn-link #'count-down
                                  :name :down-counter2)))
      (loop until (= 3 ct)
            do
            (bfly:recv msg
              ((list :Exit-Message pid _ _)
               :when (or (eq pid down)
                         (eq pid up)
                         (eq pid down2))
               (incf ct))
              ( _ )))
      
      (let ((stop (usec:get-time-usec)))
        (show-rolls (* 1e-6 (- stop start))))
      ))

  ;; -------------------------------------------

  (defun tst4 ()
    ;; only one thread for no-contention timings
    (setf *a* (make-var 0)
          *b* (make-var 0))
    (reset)
    (let ((start (usec:get-time-usec))
          (ct 0)
          (down (bfly:spawn-link #'count-down
                                 :name :down-counter)))
      (loop until (= 1 ct)
            do
            (bfly:recv msg
              ((list* :exit-message pid _)
               :when (eq pid down)
               (incf ct))
              ( _ )))
      
      (let ((stop (usec:get-time-usec)))
        (/ (- stop start) 5e6))))
  
  ) ;; progn
|#

#|
;; Speed Comparison DSTM/FSTM (median of 3 runs)
;;
;; Duration Measurements (1M Iters)
Test      DSTM       FSTM
----      ----       ----
TST1      2.23       4.57
TST2      4.50       9.59
TST3      6.72      14.82
|#


