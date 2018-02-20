;; MCAS.lisp -- Multiple CAS on CAR/CDR of ref-cells.
;;
;; Adapted from UCAM-CL-TR-579 U.Cambridge Tech Report 579,
;; "Practical lock-freedom" by Keir Fraser, Feb 2004
;;
;; DM/RAL  02/17
;; -------------------------------------------------------------
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

(defpackage #:mcas
  (:use #:common-lisp)
  (:import-from #:ref
   #:ref
   #:ref-value
   #:car-ref
   #:cdr-ref
   #:cas)
  (:export
   #:%mcas
   #:mcas
   #:mcas-read
   ))

(in-package #:mcas)
   
(declaim (optimize (speed 3) (safety 0) #+:LISPWORKS (float 0)))

;; ------------------
;; CCAS - Conditional CAS
;;
;; The job of CCAS is to conditionally acquire a reference on behalf
;; of an MCAS operation. The condition is that the MCAS operation must
;; still be in a state of :UNDECIDED.
;;
;; If this condition is met, we either acquire the ref cell with the
;; MCAS descriptor, or we fail because the ref cell did not contain
;; the expected old value
;;
;; If the CCAS acquires, but the condition is not met, it can only be
;; because another thread pushed us along to a :FAILED or :SUCCEEDED
;; resolution already. In other words we have already been through
;; here.  So even if we successfuly CAS again, we have to set the
;; value back to Old value.
;;

(defstruct mcas-desc
  triples
  (status   (list :undecided)))

(defmacro mstatus (mdesc)
  `(car (mcas-desc-status ,mdesc)))

(defstruct ccas-desc
  ref old mdesc)


(defun ccas (ref old mdesc)
  ;; CCAS -- conditional CAS
  ;; Condition is that mdesc must be :undecided.
  ;; Returns nothing useful.
  (declare (mcas-desc mdesc))
  
  (labels ((try-ccas (desc)
             (declare (ccas-desc desc))
             (cond ((cas ref old desc)
                    ;;
                    ;;               CAS succeeded
                    ;;                     |  
                    ;;              ref-val EQ old -> CCAS desc
                    ;;               |     |    |                   
                    ;;   MCAS :UNDECIDED   |   MCAS :FAILED
                    ;;                     |
                    ;;         MCAS :SUCCEEDED && old EQ new
                    ;;
                    ;; We got it! Either this is the first time
                    ;; through with MCAS :UNDECIDED or, since the
                    ;; old value was eq the expected old, the MCAS was
                    ;; pushed along by another thread and the state
                    ;; must now be :FAILED.
                    ;;
                    ;; (Or else, the planned new value was the same as
                    ;; the old value and MCAS :SUCCEEDED. Either way,
                    ;; it put back the old value.).
                    ;;
                    ;; In the first case, we can now replace our CCAS
                    ;; descriptor with the caller's MCAS descriptor.
                    ;;
                    ;; In the second case, we must put back the old value.
                    ;;
                    (ccas-help desc))
                     
                   (t
                    (let ((v (ref-value ref)))
                      (cond ((ccas-desc-p v)
                             (ccas-help v)
                             (try-ccas desc))
                            )))
                   )))
    
    (try-ccas (make-ccas-desc
               :ref      ref
               :old      old
               :mdesc    mdesc))
    ))

(defun ccas-help (desc)
  (declare (ccas-desc desc))
  (let* ((mdesc (ccas-desc-mdesc desc))
         (new   (if (eq :undecided (mstatus mdesc))
                    mdesc
                  (ccas-desc-old desc))))
    ;;
    ;; If the ref cell still contains our CCAS desc, then this CAS
    ;; will succeeed.
    ;;
    ;; If not, then it is because another thread has already been
    ;; through here pushing our CCAS desc and succeeded.
    ;;
    (cas (ccas-desc-ref desc) desc new)))

(defun ccas-read (ref)
  (let ((v (ref-value ref)))
    (cond ((ccas-desc-p v)
           (ccas-help v)
           (ccas-read ref))

          (t  v)
          )))

;; ------------------
;; MCAS - Multiple CAS

(defun %mcas (triples)
  ;; triples - a sequence of (ref old new) suitable for CAS
  (mcas-help (make-mcas-desc
              :triples triples)))

(defmacro mcas (&rest terms)
  `(%mcas (um:triples ,@terms)))

(defun mcas-help (desc)
  (declare (mcas-desc desc))
  (let ((triples       (mcas-desc-triples desc))
        (status-ref    (mcas-desc-status desc)))

    (symbol-macrolet ((status (car status-ref)))
      
      (labels ((decide (desired-state)
                 (sys:compare-and-swap status :undecided desired-state)
                 (let ((success (eq :successful status)))
                   (labels ((patch-up (ref old new)
                              (cas ref desc (if success new old))))
                     (map nil (lambda (triple)
                                (apply #'patch-up triple))
                          triples)
                     (return-from mcas-help success))))
               
               (acquire (ref old new)
                 (ccas ref old desc)
                 (let ((v (ccas-read ref)))
                   (cond ((eq v desc)
                          ;; we got it,
                          ;; break
                          )
                         
                         ((mcas-desc-p v)
                          ;; someone else is trying, help them out, then
                          ;; try again
                          (mcas-help v)
                          (acquire ref old new))
                       
                         (t ;; not a descriptor, and not eq old with
                            ;; :undecided, so we must have missed our
                            ;; chance, or else we already resolved to
                            ;; :failed or :successful, and this will
                            ;; have no effect.
                            (decide :failed))
                         ))
                 ))
        
        (when (eq :undecided status)
          (map nil (lambda (triple)
                     (apply #'acquire triple))
               triples))
        (decide :successful)
        ))))
  
(defun mcas-read (ref)
  (let ((v (ccas-read ref)))
    (cond ((mcas-desc-p v)
           (mcas-help v)
           (mcas-read ref))

          (t  v)
          )))


#|
(progn
  (defun tstx (&optional (n 1000000))
    (let ((a  (ref 1))
          (b  (ref 2))
          (ct 0))
      (rch:spawn (lambda ()
                   (loop repeat n do
                         (loop until (mcas a 1 3
                                           b 2 4))
                         (incf ct)
                         (mcas a 3 5
                               b 4 6))))
      (loop repeat n do
            (loop until (mcas a 5 7
                              b 6 8))
            (incf ct)
            (mcas a 7 1
                  b 8 2))
      ct))
) ;; progn
|#

  