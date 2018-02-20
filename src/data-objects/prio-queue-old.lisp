;; prio-queue.lisp -- Lightweight and Fast Shared / UnShared LIFO,
;; FIFO, Prio Queue, and Prio Mailbox
;;
;; DM/RAL 11/17
;; -----------------------------------------------------------------------
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

(in-package #:priq)

;; equiv to #F
(declaim  (OPTIMIZE (SPEED 3) (SAFETY 0) #+:LISPWORKS (FLOAT 0)))

;; All FIFO Queues provide methods ADDQ, POPQ, and EMPTYQ-P, as well
;; as individual make-xxx constructors for variants.
;;
;; POPQ always returns two values, the value popped and a T flag if
;; the value actually had been popped, and not just a nil value
;; resulting from a POPQ against an empty FIFO queue. It is not an
;; error to POPQ from an empty queue.
;;
;; --------------------------------------------------------------
;;
;; Take advantage of the fact that all our safe data structs are
;; single-slot objects. Hence the code to RMW each of them would be
;; the same. Structure inheritance to the (clean) rescue!

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct 1-slot-envelope
    (ref  nil)))

;; -----------------------------------------------------

(defun rmw (s modify-fn)
  ;;
  ;; s is some structure isomorphic to 1-slot-envelope, i.e., must
  ;; have the RMW pointer in the first slot
  ;;
  (declare (1-slot-envelope s)
           (function modify-fn))
  (loop for old = (1-slot-envelope-ref s)
        for new = (funcall modify-fn old)
        until 
	(CAS (1-slot-envelope-ref s) old new)))

#+:LISPWORKS
(defun exch (s val)
  (declare (1-slot-envelope s))
  (sys:atomic-exchange (1-slot-envelope-ref s) val))

#+:ALLEGRO
(defun exch (s val)
  (declare (1-slot-envelope s))
  (loop for old = (1-slot-envelope-ref s)
        until
	(CAS (1-slot-envelope-ref s) old val)
        finally
	(return old)))

;; --------------------------------------------------------------
;; UNSAFE-LIFO - A LIFO queue - unsafe for sharing

(defstruct (unsafe-lifo
            (:include 1-slot-envelope (:ref nil))))

(defmethod addq ((q unsafe-lifo) item &key &allow-other-keys)
  (push item (unsafe-lifo-ref q)))

(defmethod popq ((q unsafe-lifo) &key &allow-other-keys)
  (cond ((unsafe-lifo-ref q)
         (values (pop (unsafe-lifo-ref q)) t))
        
        (t
         (values nil nil))
        ))

(defmethod emptyq-p ((q unsafe-lifo))
  (null (unsafe-lifo-ref q)))

(defmethod contents ((q unsafe-lifo))
  (exch q nil))

(defmethod findq ((q unsafe-lifo) val &rest args)
  (apply 'find val (unsafe-lifo-ref q) args))

(defmethod lastq ((q unsafe-lifo))
  (car (unsafe-lifo-ref q)))

;; --------------------------------------------------------------
;; LIFO - A LIFO queue with safe sharing

(defstruct (lifo (:include unsafe-lifo)) )

(defmethod addq ((q lifo) item &key &allow-other-keys)
  (rmw q (um:curry #'cons item)))

(defmethod popq ((q lifo) &key &allow-other-keys)
  (let (ans
        found)
    (rmw q (lambda (lst)
             (cond (lst
                    (setf ans   (pop lst)
                          found t))
                   
                   (t
                    (setf ans   nil
                          found nil)) )
             lst))
    (values ans found)))

;; -------------------------------------------------------------
;; UNSAFE-FIFO - Very Fast FIFO Queue - unsafe for sharing
;;
;; Invariant: A non-empty queue always has a non-nil hd
#|
(defstruct (unsafe-fifo
            (:include 1-slot-envelope (:ref (cons nil nil)))))

(defun unsafe-normalize-fifo (hd tl)
  (unless hd
    (setf hd (nreverse tl) ;; note destructive reverse
          tl nil))
  (cons hd tl))

(defmethod addq ((q unsafe-fifo) item &key &allow-other-keys)
  (destructuring-bind (hd . tl) (unsafe-fifo-ref q)
    (push item tl)
    (setf (unsafe-fifo-ref q) (unsafe-normalize-fifo hd tl))))

(defmethod popq ((q unsafe-fifo) &key &allow-other-keys)
  (destructuring-bind (hd . tl) (unsafe-fifo-ref q)
    (let (ans found)
      (when hd
        (setf ans   (pop hd)
              found t)
        (setf (unsafe-fifo-ref q) (unsafe-normalize-fifo hd tl)))
      (values ans found))
    ))

(defmethod emptyq-p ((q unsafe-fifo))
  (null (car (unsafe-fifo-ref q))))

(defmethod contents ((q unsafe-fifo))
  (destructuring-bind (hd . tl) (exch q (cons nil nil))
    (nconc hd (nreverse tl))))

(defmethod findq ((q unsafe-fifo) val &rest args)
  (destructuring-bind (hd . tl) (unsafe-fifo-ref q)
    (or (apply 'find val hd args)
        (apply 'find val tl args))))
|#

;; invariant: hd always points to a cell with null car,
;;   and when (EQ hd tl) the queue is empty

(defstruct (unsafe-fifo
            (:constructor %make-unsafe-fifo))
  hd tl)

(defun make-unsafe-fifo ()
  (let ((empty (list nil)))
    (%make-unsafe-fifo
     :hd empty
     :tl empty)))

(defmethod addq ((q unsafe-fifo) item &key &allow-other-keys)
  (with-accessors ((tl  unsafe-fifo-tl)) q
    (declare (cons tl))
    (setf tl (setf (cdr tl) (list item)))
    ))

(defmethod popq ((q unsafe-fifo) &key &allow-other-keys)
  (with-accessors ((hd  unsafe-fifo-hd)
                   (tl  unsafe-fifo-tl)) q
    (declare (cons hd tl))
    (if (eq hd tl)
        (values nil nil)
      (values (shiftf (car (setf hd (cdr hd))) nil) ;; keep GC happy
              t))
    ))

(defmethod emptyq-p ((q unsafe-fifo))
  (with-accessors ((hd  unsafe-fifo-hd)
                   (tl  unsafe-fifo-tl)) q
    (eq hd tl)))

(defmethod contents ((q unsafe-fifo))
  (with-accessors ((hd  unsafe-fifo-hd)
                   (tl  unsafe-fifo-tl)) q
    (declare (cons hd tl))
    (shiftf (cdr (setf tl hd)) nil)))

(defmethod set-contents ((q unsafe-fifo) lst)
  (with-accessors ((hd  unsafe-fifo-hd)
                   (tl  unsafe-fifo-tl)) q
    (declare (cons hd tl))
    (setf (cdr hd) lst
          tl       (last hd)) ))

(defsetf contents set-contents)


(defmethod findq ((q unsafe-fifo) val &rest args)
  (with-accessors ((hd  unsafe-fifo-hd)) q
    (declare (cons hd))
    (apply 'find val (cdr hd) args)))

(defmethod lastq ((q unsafe-fifo))
  (with-accessors ((tl unsafe-fifo-tl)) q
    (car tl)))

;; ---------------------------------------------------------------
;; FIFO - Fast FIFO Queue - safe for sharing
;;
;; Invariant: A non-empty queue always has a non-nil hd

(defstruct (fifo
            (:include 1-slot-envelope (:ref (cons nil nil)))))

(defun normalize-fifo (hd tl)
  (unless hd
    (setf hd (reverse tl) ;; note: nondestructive reverse
          tl nil))
  (cons hd tl))

(defmethod addq ((q fifo) item &key &allow-other-keys)
  (rmw q (lambda (cell)
           (destructuring-bind (hd . tl) cell
             (push item tl)
             (normalize-fifo hd tl)))
       ))

(defmethod popq ((q fifo) &key &allow-other-keys)
  (let (ans
        found)
    (rmw q (lambda (cell)
             (destructuring-bind (hd . tl) cell
               (cond (hd
                      (setf ans   (pop hd)
                            found t)
                      (normalize-fifo hd tl))
                     
                     (t
                      (setf ans   nil
                            found nil)
                      cell)))) )
    (values ans found)))

(defmethod emptyq-p ((q fifo))
  (null (car (fifo-ref q))))

(defmethod contents ((q fifo))
  (destructuring-bind (hd . tl) (exch q (cons nil nil))
    (nconc hd (nreverse tl))))

(defmethod findq ((q fifo) val &rest args)
  (destructuring-bind (hd . tl) (fifo-ref q)
    (or (apply 'find val hd args)
        (apply 'find val tl args))))

;; ------------------------------------------------------------------
;; UNSAFE-PRIQ - Fast Priority FIFO Queue - unsafe for sharing
;;
;; Invariant: No priority level in the tree has an empty FIFO queue

(defstruct (unsafe-priq
            (:include 1-slot-envelope (:ref (maps:empty)))))

(defmethod addq ((q unsafe-priq) item &key prio)
  (let* ((tree (unsafe-priq-ref q))
         (fq   (maps:find prio tree)))
    (unless fq
      (setf fq (make-unsafe-fifo)
            (unsafe-priq-ref q) (maps:add prio fq tree)))
    (addq fq item)))

(defmethod popq ((q unsafe-priq) &key prio)
  (let ((tree  (unsafe-priq-ref q)))
    (labels ((no ()
               (values nil nil))

             (yes (prio fq)
               (let ((ans (popq fq)))
                 (when (emptyq-p fq)
                   (setf (unsafe-priq-ref q) (maps:remove prio tree)))
                 (values ans t))))
      
      (cond ((maps:is-empty tree) (no))
            
            (prio
             (um:if-let (fq (maps:find prio tree))
                 (yes prio fq)
               (no)))
          
            (t
             (let* ((node (sets:max-elt tree))
                    (prio (maps:map-cell-key node))
                    (fq   (maps:map-cell-val node)))
               (yes prio fq)))
            ))))

(defmethod emptyq-p ((q unsafe-priq))
  (maps:is-empty (unsafe-priq-ref q)))

(defmethod countq ((q unsafe-priq))
  (sets:cardinal (unsafe-priq-ref q)))
          
;; ------------------------------------------------------------------
;; PRIQ - Priority FIFO Queue - safe for sharing
;;
;; Invariant: No priority level in the tree has an empty FIFO queue

(defstruct (priq (:include unsafe-priq)))

(defmethod addq ((q priq) item &key (prio 0))
  (rmw q (lambda (tree)
           (let ((fq (maps:find prio tree)))
             (cond (fq
                    (setf fq (copy-fifo fq)))
                   
                   (t
                    ;; must use a safe FIFO even though our copy isn't
                    ;; shared yet, because UNSAFE-FIFO uses NREVERSE
                    ;; which isn't playing by purely functional rules.
                    ;;
                    (setf fq (make-fifo))) )
             (addq fq item)
             (maps:add prio fq tree))) ;; MAPS:ADD replaces any existing entry
       ))

(defmethod popq ((q priq) &key prio)
  (let (ans
        found)
    (rmw q (lambda (tree)
             ;;
             ;; The beautiful thing about this is that while inside
             ;; this function, the use of purely functional data types
             ;; ensures that our view of tree won't change, and
             ;; nothing we do to it (if we play by functional rules!)
             ;; can be seen by any other processes until we are
             ;; finished.
             ;;
             ;; We might be called to perform this body of code more
             ;; than once in case someone else changed the underlying
             ;; data structure before we could finish. But each time
             ;; through, our view of tree is entirely ours.
             ;;
             (labels ((no ()
                        (setf ans   nil
                              found nil)
                        tree)

                      (yes (prio fq)
                        (setf fq    (copy-fifo fq)
                              ans   (popq fq)
                              found t)
                        (if (emptyq-p fq)
                            (maps:remove prio tree)
                          (maps:add prio fq tree))))
               
               (cond ((maps:is-empty tree) (no))

                     (prio
                      (um:if-let (fq (maps:find prio tree))
                          (yes prio fq)
                        (no)))

                     (t 
                      (let* ((node (sets:max-elt tree))
                             (prio (maps:map-cell-key node))
                             (fq   (maps:map-cell-val node)))
                        (yes prio fq)))
                     )) ))
    (values ans found)))

;; ------------------------------------------------------
;; PRIO-MAILBOX -- Mailbox with priority delivery - safe for sharing

#+:LISPWORKS
(progn
  (defstruct (prio-mailbox (:include priq))
    (sem  (mp:make-semaphore :count 0) :read-only t))
  
  (defmethod mailbox-send ((mbox prio-mailbox) msg &key (prio 0))
    (with-accessors ((sem  prio-mailbox-sem)) mbox
      (addq mbox msg :prio prio)
      (mp:semaphore-release sem)))
  
  (defmethod mailbox-empty-p ((mbox prio-mailbox))
    (emptyq-p mbox))
  
  (defmethod mailbox-not-empty-p ((mbox prio-mailbox))
    (not (mailbox-empty-p mbox)))

  (defmethod mailbox-read ((mbox prio-mailbox) &optional wait-reason timeout)
    (with-accessors ((sem  prio-mailbox-sem)) mbox
      (and (mp:semaphore-acquire sem
                                 :wait-reason wait-reason
                                 :timeout     timeout)
           (popq mbox))
      )))

;; ------------------------------------------------------

#+:ALLEGRO
(progn
  (defstruct (prio-mailbox (:include priq))
    (sem  (mp:make-gate nil) :read-only t))
  
  (defmethod mailbox-send ((mbox prio-mailbox) msg &key (prio 0))
    (with-accessors ((sem  prio-mailbox-sem)) mbox
      (addq mbox msg :prio prio)
      (mp:put-semaphore sem)))
  
  (defmethod mailbox-empty-p ((mbox prio-mailbox))
    (emptyq-p mbox))
  
  (defmethod mailbox-not-empty-p ((mbox prio-mailbox))
    (not (mailbox-empty-p mbox)))
  
  (defmethod mailbox-read ((mbox prio-mailbox) &optional wait-reason timeout)
    (with-accessors ((sem  prio-mailbox-sem)) mbox
      (cond ((null timeout)
             (mp:get-semaphore sem)
             (popq mbox))
            
            ((not (plusp timeout))
	     ;; a zero timeout means grab mail quickly, if available,
	     ;; otherwise, return immediately
             (unless (emptyq-p mbox)
               (sys:with-timeout (0.1 (values nil nil))
                  (mp:get-semaphore sem)
                  (popq mbox))))
            
            ((plusp timeout)
             (sys:with-timeout ((max timeout 0.1)
                                (values nil nil))
                (mp:get-semaphore sem)
                (popq mbox)))
            ))))

;; ------------------------------------------------------
