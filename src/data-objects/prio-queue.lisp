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

(defclass envelope ()
  ((ref   :accessor  envelope-ref
          :initform  nil
          :initarg   :ref)))

(defclass safe-mixin ()
  ((lock  :reader    safe-mixin-lock
          :initform  (mpcompat:make-lock))))

;; -----------------------------------------------------

(defmethod rmw ((e envelope) modify-fn)
  (declare (function modify-fn))
  (setf (envelope-ref e) (funcall modify-fn (envelope-ref e))))

(defmethod exch ((e envelope) val)
  (shiftf (envelope-ref e) val))

;; ------------------------------------------

(defmethod rmw :around ((s safe-mixin) modify-fn)
  (mpcompat:with-lock ((safe-mixin-lock s))
    (call-next-method)))

(defmethod exch :around ((s safe-mixin) val)
  (mpcompat:with-lock ((safe-mixin-lock s))
    (call-next-method)))

(defmethod countq :around ((s safe-mixin))
  (mpcompat:with-lock ((safe-mixin-lock s))
    (call-next-method)))
          
;; --------------------------------------------------------------
;; UNSAFE-LIFO - A LIFO queue - unsafe for sharing

(defclass unsafe-lifo (envelope)
  ())

(defun make-unsafe-lifo ()
  (make-instance 'unsafe-lifo))

(defmethod addq ((q unsafe-lifo) item &key &allow-other-keys)
  (push item (envelope-ref q)))

(defmethod popq ((q unsafe-lifo) &key &allow-other-keys)
  (cond ((envelope-ref q)
         (values (pop (envelope-ref q))) t)
        
        (t
         (values nil nil))
        ))

(defmethod emptyq-p ((q unsafe-lifo))
  (null (envelope-ref q)))

(defmethod contents ((q unsafe-lifo))
  (shiftf (envelope-ref q) nil))

(defmethod findq ((q unsafe-lifo) val &rest args)
  (apply 'find val (envelope-ref q) args))

(defmethod lastq ((q unsafe-lifo))
  (car (envelope-ref q)))

;; --------------------------------------------------------------
;; LIFO - A LIFO queue with safe sharing

(defclass lifo  (safe-mixin unsafe-lifo)
  ())

(defun make-lifo ()
  (make-instance 'lifo))

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

(defclass unsafe-fifo ()
  ((hd  :accessor unsafe-fifo-hd
        :initarg  :hd)
   (tl  :accessor unsafe-fifo-tl
        :initarg  :tl)))

(defun make-unsafe-fifo ()
  (let ((empty (list nil)))
    (make-instance 'unsafe-fifo
                   :hd empty
                   :tl empty)))

(defun copy-unsafe-fifo (f)
  (make-instance 'unsafe-fifo
                 :hd (copy-list (unsafe-fifo-hd f))
                 :tl (copy-list (unsafe-fifo-tl f))))

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

(defmethod countq ((q unsafe-fifo))
  (with-accessors ((hd  unsafe-fifo-hd)) q
    (declare (cons hd tl))
    (length (cdr hd))))

;; ---------------------------------------------------------------
;; FIFO - Fast FIFO Queue - safe for sharing
;;
;; Invariant: A non-empty queue always has a non-nil hd

(defclass fifo (safe-mixin envelope)
  ((ref  :accessor fifo-ref
         :initform (cons nil nil))))

(defun make-fifo ()
  (make-instance 'fifo))

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
  (mpcompat:with-lock ((safe-mixin-lock q))
    (destructuring-bind (hd . tl) (fifo-ref q)
      (or (apply 'find val hd args)
          (apply 'find val tl args)))))

;; ------------------------------------------------------------------
;; UNSAFE-PRIQ - Fast Priority FIFO Queue - unsafe for sharing
;;
;; Invariant: No priority level in the tree has an empty FIFO queue

(defclass unsafe-priq (envelope)
  ((ref  :initform (maps:empty))))

(defun make-unsafe-priq ()
  (make-instance 'unsafe-priq))

(defmethod addq ((q unsafe-priq) item &key prio)
  (let* ((tree (envelope-ref q))
         (fq   (maps:find prio tree)))
    (unless fq
      (setf fq (make-unsafe-fifo)
            (envelope-ref q) (maps:add prio fq tree)))
    (addq fq item)))

(defmethod popq ((q unsafe-priq) &key prio)
  (let ((tree  (envelope-ref q)))
    (labels ((no ()
               (values nil nil))

             (yes (prio fq)
               (let ((ans (popq fq)))
                 (when (emptyq-p fq)
                   (setf (envelope-ref q) (maps:remove prio tree)))
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
  (maps:is-empty (envelope-ref q)))

(defmethod countq ((q unsafe-priq))
  (let ((ct 0))
    (maps:iter (lambda (k v)
                 (declare (ignore k))
                 (incf ct (countq v)))
               (envelope-ref q))
    ct))
          
;; ------------------------------------------------------------------
;; PRIQ - Priority FIFO Queue - safe for sharing
;;
;; Invariant: No priority level in the tree has an empty FIFO queue

(defclass priq (safe-mixin unsafe-priq)
  ())

(defun make-priq ()
  (make-instance 'priq))

(defmethod addq ((q priq) item &key (prio 0))
  (rmw q (lambda (tree)
           (let ((fq (maps:find prio tree)))
             (unless fq
               (setf fq (make-unsafe-fifo)))
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
                        (setf ans   (popq fq)
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
  (defclass prio-mailbox (priq)
    ((sem  :reader   prio-mailbox-sem
           :initarg  :sem
           :initform (mp:make-semaphore :count 0))))
  
  (defun make-prio-mailbox (&key name)
    (if name
        (make-instance 'prio-mailbox
                       :sem (mp:make-semaphore :count 0
                                               :name  name))
      ;; else
      (make-instance 'prio-mailbox)))
  
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

#+:CLOZURE
(progn
  (defclass prio-mailbox (priq)
    ((sem  :reader   prio-mailbox-sem
           :initform (mp:make-semaphore))))
  
  (defun make-prio-mailbox (&key name)
    (make-instance 'prio-mailbox))
  
  (defmethod mailbox-send ((mbox prio-mailbox) msg &key (prio 0))
    (with-accessors ((sem  prio-mailbox-sem)) mbox
      (addq mbox msg :prio prio)
      (mp:signal-semaphore sem)))
  
  (defmethod mailbox-empty-p ((mbox prio-mailbox))
    (emptyq-p mbox))
  
  (defmethod mailbox-not-empty-p ((mbox prio-mailbox))
    (not (mailbox-empty-p mbox)))
  
  (defmethod mailbox-read ((mbox prio-mailbox) &optional wait-reason timeout)
    (with-accessors ((sem prio-mailbox-sem)) mbox
      (ccl:with-interrupts-enabled
          (if timeout
              (and (mp:timed-wait-on-semaphore sem timeout)
                   (popq mbox))
              (progn
                (mp:wait-on-semaphore sem nil wait-reason)
                (popq mbox)))))))

#+:SBCL
(progn
  (defclass prio-mailbox (priq)
    ((sem  :reader   prio-mailbox-sem
           :initform (sb-thread:make-semaphore))))
  
  (defun make-prio-mailbox (&key name)
    (make-instance 'prio-mailbox))
  
  (defmethod mailbox-send ((mbox prio-mailbox) msg &key (prio 0))
    (with-accessors ((sem  prio-mailbox-sem)) mbox
      (addq mbox msg :prio prio)
      (sb-thread:signal-semaphore sem)))
  
  (defmethod mailbox-empty-p ((mbox prio-mailbox))
    (emptyq-p mbox))
  
  (defmethod mailbox-not-empty-p ((mbox prio-mailbox))
    (not (mailbox-empty-p mbox)))
  
  (defmethod mailbox-read ((mbox prio-mailbox) &optional wait-reason timeout)
    (declare (ignore wait-reason))
    (with-accessors ((sem prio-mailbox-sem)) mbox
      (if timeout
          (and (sb-thread:wait-on-semaphore sem :timeout timeout)
               (popq mbox))
          (progn
            (sb-thread:wait-on-semaphore sem)
            (popq mbox))))))


;; ------------------------------------------------------

#+:ALLEGRO
(progn
  (defclass prio-mailbox (priq)
    ((sem  :reader   prio-mailbox-sem
           :initform (mp:make-gate nil))))

  (defun make-prio-mailbox (&key name)
    (make-instance 'prio-mailbox))
  
  (defmethod mailbox-send ((mbox prio-mailbox) msg &key (prio 0))
    (with-accessors ((sem  prio-mailbox-sem)) mbox
      (addq mbox msg :prio prio)
      (mp:put-semaphore sem)))
  
  (defmethod mailbox-empty-p ((mbox prio-mailbox))
    (emptyq-p mbox))
  
  (defmethod mailbox-not-empty-p ((mbox prio-mailbox))
    (not (mailbox-empty-p mbox)))
  
  (defmethod mailbox-read ((mbox prio-mailbox) &optional wait-reason timeout)
    (with-accessors ((sem   prio-mailbox-sem)) mbox
      (cond ((null timeout)
             (mp:get-semaphore sem)
             (popq mbox))
            
            ((plusp timeout)
             (sys:with-timeout ((max timeout 0.1)
                                (values nil nil))
                (mp:get-semaphore sem)
                (popq mbox)))
            ))))

;; ------------------------------------------------------
