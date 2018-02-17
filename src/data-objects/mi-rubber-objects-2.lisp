;; --------------------------------------------------------------------
;; rubber-objects.lisp -- Create a stupid-simple flexible object
;; system with inheritance. "Self" without optimizations.
;;
;; DM/RAL 07/16, 04/17 -- Rube Goldberg has nothing on us...
;; added multiple inheritance, observers for reactive programming,
;; computed slots, observed slots
;; -- Version 2: remove complexity of observed slots since we need to
;; scan for object changes anyway.
;; --------------------------------------------------------------------
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

(defpackage :rubber-objects
  (:use #:common-lisp)
  (:nicknames #:ro)
  (:import-from #:useful-macros
   #:defmacro!
   #:if-let
   #:when-let
   #:nlet-tail
   #:group
   #:symb
   #:rcurry
   #:curry
   )
  (:export
   #:rubber-object
   #:rubber-object-p
   #:direct-parents
   #:parents
   #:parent
   #:props
   #:=top=
   #:prop
   #:call
   #:call-next
   #:next-prop
   #:set-prop ;; (set-prop obj key val), also (setf (prop obj key) val)
   #:make-prop-accessor
   #:make-prop-accessors
   #:direct-prop-keys
   #:prop-keys
   #:has-direct-prop
   #:has-prop
   #:remove-direct-prop
   #:is-a
   #:copy-of
   #:instance-of

   #:child-p
   #:parent-p
   #:descendent-p
   #:ancestor-p

   #:observe
   #:make-observer
   #:remove-observer
   #:with-delayed-observers
   #:defslotfn
   #:pvl
   #:pv
   ))

(in-package :rubber-objects)

;; equiv to #F
(proclaim '(optimize (speed  3)
                     (safety 0)
                     (float  0)))

(defclass rubber-object ()
  ((=props=     :accessor props     :initarg :=props=     :initform nil)
   (lock        :reader   ro-lock   :initform (mp:make-lock
                                               :sharing t))
   ))

(defmethod rubber-object-p (obj)
  nil)

(defmethod rubber-object-p ((obj rubber-object))
  t)

(defvar =top= (make-instance 'rubber-object))

;; ---------------------------------------------------------------
;; A normal slot is just a plist value

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; need to define computed-slot for sake of SYS:COMPARE-AND-SWAP
  (defstruct computed-slot
    cached
    busy
    fn))

;; --------------------------------------------
;; macro for creating a computed slot

(defmacro! computed ((&key initial) &rest body)
  `(make-computed-slot
    :cached ,initial
    :fn     (lambda (,a!self)
              ,@body)))

(editor:setup-indent "computed" 1)

;; ----------------------------------------------------------
;; Fast circular queue implementation. No need for thread safety here
;; since all uses here are thread-local.

(defstruct queue
  items)

(defun qempty? (q)
  (null (queue-items q)))

(defun qpop (q)
  (when-let (tl (queue-items q))
    (let* ((hd  (cdr tl)))
      (if (eq hd tl)
          (setf (queue-items q) nil)
        (setf (cdr tl) (cdr hd)))
      (car hd))))

(defun nqappend (q items)
  (when items
    (let* ((ilast (last items))
           (tl    (queue-items q))
           (hd    (if tl (cdr tl) items)))
      (setf (cdr (or tl ilast)) items
            (cdr ilast) hd
            (queue-items q) ilast)
      )))

(defun qappend (q items)
  (when items
    (nqappend q (copy-list items))))

(defun qadd (q item)
  (nqappend q (list item)))

(defun qcontents (q)
  (when-let (tl (shiftf (queue-items q) nil))
    (shiftf (cdr tl) nil)))

;; ----------------------------------------------------------
;; BFS = Breadth First Search. This is not the C3 Algorithm, but
;; unlike C3 this is easily comprehended...

(defun do-with-bfs (obj fn)
  (let ((q (make-queue)))
    (qadd q obj)
    (nlet-tail iter ()
      (if (qempty? q)
          (funcall fn =top=)
        ;; else
        (let ((hd (qpop q)))
          (unless (eq hd =top=)
            (nqappend q (direct-parents hd))
            (funcall fn hd))
          (iter))
        ))
    ))

(defmacro with-bfs ((obj root) &body body)
  `(do-with-bfs ,root (lambda (,obj) ,@body)))

(editor:setup-indent "with-bfs" 1)
          
;; ----------------------------------------------------------

(defmethod direct-parents ((obj rubber-object))
  ;; we have to copy this list anyway, for the most common
  ;; use case - looking up the inheritance chain.
  ;; so just play it thread-safe for everyone.
  (mp:with-sharing-lock ((ro-lock obj))
    (copy-list (getf (props obj) :is-a))))

(defmethod parent ((obj rubber-object))
  ;; only defined for single-inheritance objects
  (let ((parents (direct-parents obj)))
    (assert (null (cdr parents)))
    (car parents)))

(defmethod parents ((obj rubber-object))
  (let ((qans (make-queue)))
    (with-bfs (hd obj)
      (qadd qans hd))
    (cdr (delete-duplicates (qcontents qans)
                            :from-end t))))

(defmethod is-a ((obj rubber-object) (archetype rubber-object))
  (with-bfs (hd obj)
    (when (eq hd archetype)
      (return-from is-a t))))

(defmethod child-p ((obj rubber-object) (par rubber-object))
  (member par (direct-parents obj)))

(defmethod descendent-p ((obj rubber-object) (par rubber-object))
  (is-a obj par))

(defmethod parent-p ((par rubber-object) (obj rubber-object))
  (child-p obj par))

(defmethod ancestor-p ((par rubber-object) (obj rubber-object))
  (is-a obj par))

;; ----------------------------------------------------------
;; modified p-list get that returns a success flag

(defvar +unique+ (load-time-value (gensym) t))

(defmethod getp ((lst list) key &optional default)
  (let ((ans (getf lst key +unique+)))
    (if (eq ans +unique+)
        default
      ;; else
      (values ans t))
    ))

(defmethod getp ((obj rubber-object) key &optional default)
  (mp:with-sharing-lock ((ro-lock obj))
    (getp (props obj) key default)))

;; ---------------------------------------

(define-condition retry-exclusive (error)
  ())

(defvar +retry-exclusive+
  (load-time-value (make-condition 'retry-exclusive) t))

;; ----------------
;; observed slots

(defun notify-observers (obj key val)
  (when (intersection `(,key :=ANY=) (prop obj :update-slots))
    (with-delayed-observers
      (dolist (fn (prop obj :update-demons))
        (add-work ()
          (funcall fn obj key val)))
      )))

;; ---------------------------------------
;; normal slot values

(defmethod prop-val (val obj key)
  val)

;; ---------------------------------------
;; computed-slot values

(defvar *this* nil)

(defun eval-computed-slot (slot obj)
  (with-accessors ((fn     computed-slot-fn)
                   (busy   computed-slot-busy)
                   (cached computed-slot-cached)) slot
    (if (sys:compare-and-swap busy nil mp:*current-process*)
        ;; prevent any other caller, including us recursively, from
        ;; executing the body function.
        (unwind-protect
            (setf cached (funcall fn obj))
          (sys:compare-and-swap busy mp:*current-process* nil))
      ;; else
      cached
      )))

(defun do-with-possible-notification (slot obj key fn)
  (let ((cached (computed-slot-cached slot))
        (val    (funcall fn)))
    (unless (eql val cached)
      (notify-observers obj key val))
    val))

(defmacro with-possible-notification ((slot obj key) &body body)
  `(do-with-possible-notification ,slot ,obj ,key
                                  (lambda () ,@body)))

(editor:setup-indent "with-possible-notification" 1)

(defmethod prop-val ((slot computed-slot) obj key)
  (if (eq *this* obj)
      slot
    ;; else -- need to install in obj because of cache update (COW semantics)
    (error +retry-exclusive+)))
    
(defmethod prop-val-ex ((slot computed-slot) obj key)
  ;; in this function we have exclusive access to the object
  ;; so we can mutate at will...
  (if (eq *this* obj)
      slot
    ;; else -- need to install in obj because of cache update (COW semantics)
    (let ((new-slot (copy-computed-slot slot)))
      (setf (getf (props obj) key) new-slot)
      new-slot)))
    
;; ---------------------------------------

(defun find-prop (obj key start default skip-this)
 ;; Inner-find-prop returns the slot value without further
  ;; interpretation while under lock safety. If a slot contains a
  ;; computed slot value, then we ensure that it resides in the
  ;; current object.
  (labels ((try-find (pval-fn)
             (with-bfs (hd start)
               (unless (and skip-this
                            (eq hd start))
                 (multiple-value-bind (ans found)
                     (getp hd key)
                 (when found
                   (let ((val (funcall pval-fn ans obj key)))
                     (return-from try-find (values val hd))))
                 )))
             default)

           (locked-find-prop ()
             (handler-case
                 (mp:with-sharing-lock ((ro-lock obj))
                   (try-find #'prop-val))
               
               (retry-exclusive ()
                 ;; this should happen only rarely...  It happens when the slot
                 ;; key matches a computed slot that has been added to a parent
                 ;; object sometime after instantiations of that parent object,
                 ;; and so the computed slot has not been copied down into the
                 ;; instance object. We make that copy here...
                 (mp:with-exclusive-lock ((ro-lock obj))
                   (try-find #'prop-val-ex)))
               )))
    
    ;; we split this out because we want to run computed-slots under no
    ;; locks... they might try to mutate the object, needing an
    ;; exclusive-lock for themselves
    (multiple-value-bind (val hd)
        (locked-find-prop)
      (cond ((computed-slot-p val)
             (with-possible-notification (val obj key)
               (values (let ((*this* hd))
                         (eval-computed-slot val obj))
                       hd)))
            
            (t  (values val hd))
            ))))

(defmethod prop ((obj rubber-object) key &optional default)
  ;; return the direct or ancestor property value
  ;; as a secondary value we return the object in which the prop was found
  (find-prop obj key obj default nil))

(defmethod next-prop ((obj rubber-object) key &optional default)
  (find-prop obj key *this* default t))

;; ---------------------------------------

(defun msg-dispatch (propfn obj key args)
  (multiple-value-bind (fn this) (funcall propfn obj key)
    (if (functionp fn)
        (let ((*this* this))
          (apply fn args))
      (unless (and (null fn)
                   (eq propfn #'next-prop))
        ;; okay for there to be no next to call
        (error "Not a function")))))

(defmethod call ((obj rubber-object) key &rest args)
  (msg-dispatch #'prop obj key args))

(defmethod call-next ((obj rubber-object) key &rest args)
  (msg-dispatch #'next-prop obj key args))
      
;; ---------------------------------------

(defmacro pv (obj &rest slots)
  (nlet-tail iter ((slots (rest slots))
                      (exp   `(prop ,obj ,(car slots))))
    (if slots
        (iter (rest slots) `(prop ,exp ,(car slots)))
      exp)))

(defmacro! pvl (&rest slots)
  ;; shorthand for (pv self <slots>...)
  `(pv ,a!self ,@slots))

(defmacro make-prop-accessor (key &optional accessor-name)
  (let ((obj    (gensym (string :obj-)))
        (val    (gensym (string :val-)))
        (reader (symb (or accessor-name key)))
        (writer (symb (gensym (string :set-)))))
    `(progn
       ;; by creating methods here, we allow for the possibility
       ;; that the user could define the same accessors on other types.
       (defmethod ,reader ((,obj rubber-object))
         #F
         (prop ,obj ,key))
       (defmethod ,writer ((,obj rubber-object) ,val)
         #F
         (setf (prop ,obj ,key) ,val))
       (defsetf ,reader ,writer))
    ))

(defmacro make-prop-accessors (&rest keys)
  `(progn
     ,@(mapcar (lambda (key)
                 (cond ((consp key)
                        `(make-prop-accessor ,(car key) ,(cadr key)))
                       (t
                        `(make-prop-accessor ,key))
                       ))
               keys)))

;; ---------------------------------------

(defmethod direct-prop-keys ((obj rubber-object))
  (mp:with-sharing-lock ((ro-lock obj))
    (nlet-tail iter ((lst (props obj))
                        (ans nil))
      (if lst
          (iter (cddr lst) (cons (car lst) ans))
        (nreverse ans)))))

(defmethod prop-keys ((obj rubber-object))
  (let (props)
    (with-bfs (hd obj)
      (setf props (nconc props (direct-prop-keys hd))))
    (delete-duplicates props :from-end t)))

(defmethod has-direct-prop ((obj rubber-object) key)
  (second (multiple-value-list (getp obj key))))

(defmethod has-prop ((obj rubber-object) key)
  (second (multiple-value-list (prop obj key))))

(defmethod remove-direct-prop ((obj rubber-object) key)
  (let (changed)
    (mp:with-exclusive-lock ((ro-lock obj))
      (setf changed (remf (props obj) key)))
    (when changed
      (notify-observers obj key :=REMOVED=))
    changed))

;; ----------------------------------------------------------

(defvar *print-recursive* nil)

(defmethod print-object ((obj rubber-object) out-stream)
  (if *print-recursive*
      (call-next-method)
    (let ((*print-recursive* t))
      (if-let (fn (prop obj :print-object-fn))
          (funcall fn obj out-stream)
        ;; else
        (call-next-method)))))

;; -----------------------------------------------------------

(defun merge-new-old-slots (new old &optional acc)
  ;; New and old are now a-lists of (slot-name, slot-value).
  ;;
  ;; If a slot appears in both new and old, then take the new slot
  ;; value unless the old was a computed slot and the new is merely a
  ;; value. In that case, the new value is used to initialize the
  ;; cached value of the inherited computed slot from old.
  ;;
  (if new
      (let ((this (car new)))
        (if-let (a (assoc (car this) old))
            (merge-new-old-slots (cdr new) (remove a old)
                                 (cons
                                  (cond ((computed-slot-p (cadr this)) this)
                                        ((computed-slot-p (cadr a))
                                         ;; a is already a copy of the original computed slot
                                         ;; so it is okay to mutate here...
                                         (setf (computed-slot-cached (cadr a)) (cadr this))
                                         a)
                                        (t this))
                                  acc))
          ;; else -- not in old list, take the new
          (merge-new-old-slots (cdr new) old (cons this acc))))
    
    ;; else -- finished the list of new slots
    ;; form up the new p-list without duplicates in the tail
    (mapcan #'identity
            (delete-duplicates (nreconc acc old)
                               :key  #'car
                               :test #'eq
                               :from-end t))))
                    
(defun merge-props (new-props old-props)
  ;; removes duplicates, keeping first
  ;; old-props have already been paired off on entry
  (let* ((new   (group new-props 2))
         (props (merge-new-old-slots new old-props)))
    (labels ((elide-key (key)
               (unless (eq +unique+ (getf props key +unique+))
                 (remf props key)
                 (warn "Property ~S ignored" key))))
      (dolist (key '(:=props=))
        (elide-key key))
      props)))

;; ------------------------------

(defmethod copy-slot-val (val)
  val)

(defmethod copy-slot-val ((val computed-slot))
  (copy-computed-slot val))

;; ------------------------------

(defun copy-slot (slot)
  (destructuring-bind (key val) slot
    `(,key ,(copy-slot-val val))))
    
(defun copy-slots (slots)
  (mapcar #'copy-slot (group slots 2)))

;; ------------------------------

(defun paired-slots-of-object (obj)
  (mp:with-sharing-lock ((ro-lock obj))
    (copy-slots (props obj))))

(defmethod paired-inherited-computed-slots ((obj rubber-object))
  (delete-if (complement #'computed-slot-p) (paired-slots-of-object obj)
             :key #'cadr))

(defmethod paired-inherited-computed-slots ((parents list))
  (mapcan #'paired-inherited-computed-slots parents))

(defun validate-parents (lst)
  (assert (every (rcurry #'typep 'rubber-object) lst)))

;; --------------------------------------------------------------------------

(defmethod copy-of ((obj rubber-object) &rest new-props)
  ;; make a copy of an object, possibly with new or modified properties
  ;; result has same parents as source object
  (make-instance 'rubber-object
                 :=props=   (merge-props new-props
                                         (paired-slots-of-object obj))
                 ))

;; --------------------------------------------------------------------------

#|
(defmethod instance-of ((obj rubber-object) &rest new-props)
  ;; make a child object with obj as its parents, possibly with new or
  ;; modified properties.
  ;;
  ;; We need to copy any computed slots from parent object, since
  ;; these contain cached values that get updated whenever the slot is
  ;; evaluated. So we short-circuit the COW protocol on them.
  ;;
  (let ((self (make-instance 'rubber-object
                             :=props=   (merge-props (list* :is-a (list obj) new-props)
                                                     (paired-inherited-computed-slots obj))
                             )))
    (init-obj self)
    self))
|#

(defmethod instance-of ((obj null) &rest new-props)
  (apply #'instance-of (list =top=) new-props))

(defmethod instance-of ((obj rubber-object) &rest new-props)
  (apply #'instance-of (list obj) new-props))

(defmethod instance-of ((parents cons) &rest new-props)
  ;; We need to copy any computed slots from parent object, since
  ;; these contain cache values that get updated whenever the slot is
  ;; evaluated. So we short-circuit the COW protocol for them.
  ;;
  (validate-parents parents)
  (let ((self (make-instance 'rubber-object
                             :=props=   (merge-props (list* :is-a parents new-props)
                                                     (paired-inherited-computed-slots parents))
                             )))
    (init-obj self)
    self))

;; -----------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; need to define work-queue for sake of SYS:COMPARE-AND-SWAP
  (defstruct work-queue
    (hold    0) ;; used to hold off activity
    (qlock   nil) ;; spinlock used to safely mutate the map
    (map     (maps:empty))))
  
(defvar *work-queue*  (make-work-queue))

(defun do-with-spinlocked-priq (priq fn)
  (with-accessors ((qlock work-queue-qlock)) priq
    (loop until (sys:compare-and-swap qlock nil mp:*current-process*))
    (prog1
        (funcall fn)
      ;; we could just nil out the qlock, but that might have cache
      ;; delayed write-back implications. CAS should ensure that all
      ;; multicore caches synchronize.
      (sys:compare-and-swap qlock mp:*current-process* nil))
    ))
    
(defmacro with-spinlocked-priq (priq &body body)
  `(do-with-spinlocked-priq ,priq (lambda () ,@body)))

(editor:setup-indent "with-spinlocked-priq" 1)

(defun qpri-add (item level priq)
  (with-accessors ((map   work-queue-map)) priq
    (with-spinlocked-priq priq
      (let ((q (maps:find level map)))
        (unless (member item q)
          (setf q (nconc q (list item))
                map (maps:add level q map)))
        ))))

(defun qpri-next (priq)
  (with-accessors ((map   work-queue-map)) priq
    (with-spinlocked-priq priq
      (unless (maps:is-empty map)
        (let* ((entry (sets:max-elt map))
               (key   (maps:map-cell-key entry))
               (q     (maps:map-cell-val entry))
               (nxt   (pop q)))
          (setf map
                (if q
                    (maps:add key q map)
                  (maps:remove key map)))
          nxt))
      )))

;; ------------------------------------------------
;;
;; Note: The *busy* flag with dynamic bindings is ineffective against
;; multiple-threads trying to enter the RUN-WORK function. But that's
;; okay because the priority queue is thread-safe for multiple access.
;; All we are tring to do with the *busy* flag is prevent
;; self-recursion within one thread.
;;
;; Conversely, the HOLD count is effective across multiple threads,
;; forcing any thread executing, or trying to execute, RUN-WORK to
;; exit that function. The last thread to decrement the HOLD count
;; back to zero will then run the priority queue workload.
;;
;; In a single-thread environment, if the thread increments the HOLD
;; count as a result of running a thunk, it will also decrement HOLD
;; before resuming, making the HOLD incident unnoticed by the thread.
;;
;; No need for an Actor thread. Each participant thread shares in the
;; workload.

(defvar *busy*  nil) ;; recursion prevention

(defun run-work ()
  (with-accessors ((hold  work-queue-hold)) *work-queue*
    (unless *busy*
      (let ((*busy* t))
        (nlet-tail iter ()
          ;; exit if either HOLD > 0, or work queue exhausted
          (when (zerop hold)
            (when-let (thunk (qpri-next *work-queue*))
              (funcall thunk)
              (iter))))
        ))))

(defun do-add-work (level fn)
  (qpri-add fn level *work-queue*)
  (run-work))

(defmacro add-work ((&key (level 0)) &body body)
  `(do-add-work ,level (lambda () ,@body)))

(editor:setup-indent "add-work" 1)

(defun do-with-delayed-observers (fn)
  (with-accessors ((hold work-queue-hold)) *work-queue*
    (sys:atomic-fixnum-incf hold)
    (unwind-protect
        (funcall fn)
      (when (zerop (sys:atomic-fixnum-decf hold))
        (run-work)))))

(defmacro with-delayed-observers (&body body)
  `(do-with-delayed-observers (lambda () ,@body)))

(defun init-obj (obj)
  (when-let (fn (prop obj :INITIALIZE))
    (add-work ()
      (funcall fn obj))))

#| -----------------------------------------------------------------------------------------------
  ;; compare speed of access between property lists and hashtables
  ;; Speed of hashtable is relatively constant for any number of entries in the table (as expected)
  ;; Speed of property list is head:head with hashtable for fewer than 100 elements,
  ;; about half as fast at 500 elements.
(let* ((nel  500)
       (niter 1000000)
       (keys (loop repeat nel collect (lw:mt-random (* 5 nel))))
       (vals (loop repeat nel collect (lw:mt-random 1000)))
       (ht   (make-hash-table))
       (lst  (mapcan 'list keys vals))
       (queries (loop repeat niter collect (lw:mt-random (* 5 nel)))))
  (loop for key in keys
        for val in vals
        do
        (setf (gethash key ht) val))
  (print "Timing HT")
  (time (dolist (query queries)
          (gethash query ht)))
  (print "Timing Lst")
  (time (dolist (query queries)
          (getf lst query)))
  )
|#

#|
(let* ((o  (instance-of () :name :o))
       (a  (instance-of o :name :a))
       (b  (instance-of o :name :b))
       (c  (instance-of o :name :c))
       (d  (instance-of o :name :d))
       (e  (instance-of o :name :e))
       (k1 (instance-of (a b c) :name :k1))
       (k2 (instance-of (d b e) :name :k2))
       (k3 (instance-of (d a)   :name :k3))
       (z  (instance-of (k1 k2 k3) :name :z)))
  ;; example from C3 Algorithm on Wikipedia
  (labels ((names (lst)
             (mapcar (rcurry #'prop :name) lst)))
    (print (names (parents z)))
    ;; example from AMOP
    (let* ((a (instance-of () :name :a))
           (b (instance-of () :name :b))
           (c (instance-of () :name :c))
           (s (instance-of (a b) :name :s))
           (r (instance-of (a c) :name :r))
           (q (instance-of (s r) :name :q)))
      (print (names (parents q)))
      )))
|#

;; -----------------------------------------------------------

(defun find-slot (obj key)
  (with-bfs (hd obj)
    (multiple-value-bind (ans found)
        (getp hd key)
      (when found
        (return-from find-slot (values ans hd)))
      )))

;; ----------------
;; normal slots

(defmethod set-prop-for-slot (slot val obj key)
  ;; overwriting scalar value
  (setf (getf (props obj) key) val))

;; ----------------
;; computed slots

(defmethod set-prop-for-slot ((slot computed-slot) val obj key)
  ;; scalar value into computed-slot cached value
  (setf (computed-slot-cached slot) val
        (getf (props obj) key)      slot))
  
(defmethod set-prop-for-slot ((slot computed-slot) (val computed-slot) obj key)
  ;; computed-slot overwriting computed-slot
  (setf (getf (props obj) key) val))

;; ---------------------

(defmethod cached-val (val)
  val)

(defmethod cached-val ((slot computed-slot))
  (computed-slot-cached slot))

;; ---------------------

(defmethod basic-prop ((obj rubber-object) key &optional default)
  (prop obj key default))

(defmethod basic-set-prop ((obj rubber-object) key val)
  ;; copy-on-write semantics. Any changes to properties
  ;; occur in the direct object, not in any of the inheritaned ancestors
  ;; return nil if no changes made
  ;;
  ;; basic-set-prop operates under exclusive locking, and offers no
  ;; notifications of changes made.
  ;;
  (mp:with-exclusive-lock ((ro-lock obj))
    (multiple-value-bind (sv sv-obj)
        (find-slot obj key)
      (if (eql val (cached-val sv))
          val
        (progn
          (cond ((eq obj sv-obj)
                 (set-prop-for-slot sv val obj key))
                
                (sv-obj
                 (let ((new-slot (copy-slot-val sv)))
                   (set-prop-for-slot new-slot val obj key)))
                
                (t
               (setf (getf (props obj) key) val))
                )
          (values val t))
        ))))

(defmethod set-prop ((obj rubber-object) key val)
  ;; we want to run notifications without being under lock in case
  ;; the observer functions need to grab a lock for themselves.
  (multiple-value-bind (valx changed)
      (basic-set-prop obj key val)
    (declare (ignore valx))
    (when changed
      (notify-observers obj key val))
    val))

(defmethod set-prop ((obj rubber-object) key (cslot computed-slot))
  ;; copy-on-write semantics. Any changes to properties
  ;; occur in the direct object, not in any of the inheritaned ancestors
  ;;
  ;; Computed slot values always override and must always reside in
  ;; current object
  (let (changed)
    (mp:with-exclusive-lock ((ro-lock obj))
      (let ((sv  (getf (props obj) key)))
        (unless (eql sv cslot)
          (setf (getf (props obj) key) cslot
                changed t))
        ))
    (when changed
      (notify-observers obj key cslot))
    cslot))

(defsetf prop       set-prop)
(defsetf basic-prop basic-set-prop)

;; -----------------------------------------------------------

(defmethod observe ((obj rubber-object) (key null) fn)
  (symbol-macrolet ((demons (basic-prop obj :update-demons)))
    (let (new)
      (mp:with-exclusive-lock ((ro-lock obj))
        (setf new (pushnew fn demons)))
      (notify-observers obj :update-demons new)
      new
      )))

(defmethod observe ((obj rubber-object) (keys cons) fn)
  (symbol-macrolet ((demons (basic-prop obj :update-demons))
                    (slots  (basic-prop obj :update-slots)))
    (let (new)
      (mp:with-exclusive-lock ((ro-lock obj))
        (setf new   (pushnew fn demons)
              slots (remove-duplicates (append keys slots))))
      (notify-observers obj :update-demons new)
      new
      )))

(defmethod observe ((obj rubber-object) (key symbol) fn)
  (observe obj (list key) fn))

(defmethod remove-observer ((obj rubber-object) fn)
  (symbol-macrolet ((demons (basic-prop obj :update-demons)))
    (let (changed
          new)
      (mp:with-exclusive-lock ((ro-lock obj))
        (let ((lst demons))
          (setf new (remove fn lst))
          (unless (eq lst new)
            (setf demons new
                  changed t))))
      (when changed
        (notify-observers obj :update-demons new))
      new)))
  
(defmacro make-observer (obj slot (obj-arg key-arg val-arg) &body body)
  `(observe ,obj ',slot (lambda (,obj-arg ,key-arg ,val-arg) ,@body)))

(editor:setup-indent "make-observer" 3)

#|
(setf x (instance-of nil
                  :watchme 15
                  :compute-me (computed (:initial 32)
                                (+ (pvl self :compute-me)
                                   (pvl self :watchme)))))

(make-observer x :watchme (val)
  (format t "You set :WATCHME to ~A~%" val)
  (format t "ComputeMe is now ~A~%" (pvl x :compute-me)))

(setf (prop x :watchme) 32)
(setf (prop x :watchme) 15)
|#

(defmacro! defslotfn (key obj (&rest args) &body body)
  ;; slot functions can refer to inherited slot functions for the same
  ;; slot key by calling CALL-SUPER
  `(let ((,g!obj ,obj)
         (,g!key ,key))
     (setf (prop ,g!obj ,g!key)
           (lambda (&rest ,g!org-args)
             (flet ((call-super (&rest ,g!args)
                      (apply #'call-next ,g!obj ,g!key (if ,g!args ,g!args ,g!org-args))))
               (destructuring-bind ,args ,g!org-args
                 ,@body))))))

(editor:setup-indent "defslotfn" 3)

(defslotfn :print-object-fn =top= (obj stream)
  (print-unreadable-object (obj stream :identity t)
    (format stream "~:(~S~)"
            (class-name (class-of obj))))
  (terpri stream)
  (princ "Properties:" stream)
  (pprint (mapcar (lambda (slot)
                    (list slot (getp (props obj) slot)))
                  (direct-prop-keys obj))
          stream)
  obj)
