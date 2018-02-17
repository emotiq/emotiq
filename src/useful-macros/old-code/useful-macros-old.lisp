;; useful_macros.lisp -- A collection of really handy macros
;;
;; DM/HMSC  11/97
;; -----------------------------------------------------------

(in-package "USEFUL-MACROS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbol-gensym (s)
    (gensym (with-output-to-string (str)
              (format str "~A-" (symbol-name s)))
            ))
  
  (defmacro with-gensyms (syms &body body)
    `(let ,(mapcar #'(lambda (s)
		       `(,s (symbol-gensym ',s)))
		   syms)
       ,@body))
  
  ;; -------------------------------------------------------------------
  ;; WITH-TAIL-PURE-CODE -- Compiled code using this macro can run recursively
  ;; all day long without ever blowing the stack.
  ;; Tail calls are effectively made into direct jumps.
  ;;
  ;; N.B. DM/RAL 02/07 -- tests with LWM 5.01 indicate that this may be
  ;; unnecessary. Code must be compiled for tail optimization. Interpreted code
  ;; has problems no matter what.
  (defmacro with-tail-pure-code (&body body)
    `(locally
       (declare (optimize (debug 1) (safety 1)))
       ,@body)))
  

;; ----------------------------------------------------------------------
(declaim (inline last1 single append1 conc1 mklist))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj)
      obj
    (list obj)))

(defun single (arg)
  (and (consp arg)
       (null (cdr (the cons arg)))))

(defun last1 (lst)
  (car (the cons (last lst))))

;; ----------------------------------------------------
;; Special macro versions of functional composition operators
;; All of these expect a parenthesized list of named dummy args
;; immediately following the EXPANDED-xxx macro name, and prior to
;; the actual arguments
;;
(defmacro expanded-combine ((&rest args) op f1 f2)
  `(lambda ,args
     (funcall ,op (funcall ,f1 ,@args) (funcall ,f2 ,@args))))

(defmacro expanded-compose ((&rest args) &rest fns)
  (cond ((null fns) `#'identity)
        ((single fns) (car fns))
        ((single (rest fns))
         `(lambda ,args
            (funcall ,(first fns) (funcall ,(second fns) ,@args))
            ))
        (t  (let ((fn1 (last1 fns))
                  (fns (butlast fns)))
              `(lambda ,args
                 (foldr #'funcall (list ,@fns) (funcall ,fn1 ,@args)))
              ))
        ))

(defmacro expanded-curry ((&rest suf-args) f &rest pref-args)
  `(lambda ,suf-args
     (funcall ,f ,@pref-args ,@suf-args)))

(defmacro expanded-rcurry ((&rest pref-args) f &rest suf-args)
  `(lambda ,pref-args
     (funcall ,f ,@pref-args ,@suf-args)))

(defmacro curried-lambda ((&rest args) &body body)
  (if (rest args)
      `(lambda (,(first args))
         (curried-lambda ,(rest args)
                         ,@body))
    `(lambda ,args
       ,@body)))

#|
;; try them out...
(expanded-compose (tree) #'first #'second #'fifth)
(expanded-compose (tree) #'first #'second)
(expanded-combine (x) #'+ #'second #'third)
(expanded-rcurry (seq) #'subseq start end)
(expanded-curry  (val) #'* 3)

(curried-lambda (a b c) (list a b c))
|#
;; -------------------------------------------------------------

(defmacro allf (val &rest places)
  (with-gensyms (gval)
     `(let ((,gval ,val))
	(setf ,@(mapcan (rcurry #'list #|place|# gval) places))
        )))

(defmacro nilf (&rest places)
  `(allf nil ,@places))

(defmacro tf (&rest places)
  `(allf t ,@places))


#+:LISPWORKS5.0
(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))

#+(OR :LISPWORKS5.1 :ALLEGRO :CLOZURE :SBCL)
(define-modify-macro conc1f (&rest obj)
  nconc)

;; ------------------------------------
(define-modify-macro addf (&rest args)
  +)

(define-modify-macro subf (&rest args)
  -)

(define-modify-macro mulf (&rest args)
  *)

(define-modify-macro divf (&rest args)
  /)

;; ------------------------------------

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro if-let ((var val) t-clause &optional f-clause)
  `(let ((,var ,val))
     (if ,var
         ,t-clause
       ,f-clause)))

(defmacro when-let ((var val) &body body)
  `(let ((,var ,val))
     (when ,var
       ,@body)))

#+:LISPWORKS
(editor:setup-indent "if-let" 2 2 4)

#|
;; -------------------------------------------------------
;; Define our own collector objects that
;; perform rapid nconc list accumulation
;;
;; DM/RAL 02/07 -- added a Lock for MP safety

(defclass <collector> ()
  ((hd   :accessor collector-hd)
   (tl   :accessor collector-tl)
   (lock :accessor collector-lock)))

(defmacro with-locked-collector ((c &rest lock-args) &body body)
  `(mp:with-lock ((collector-lock ,c) ,@lock-args)
     ,@body))

(defun collector-discard-contents (c)
  (with-locked-collector (c)
    (let ((v (list nil)))
      (setf (collector-hd c) v
            (collector-tl c) v)
      )))

(defmethod initialize-instance ((c <collector>) &key &allow-other-keys)
  (setf (collector-lock c) (mp:make-lock :name "Collector Lock"))
  (collector-discard-contents c))

(defun collector-contents (c &key (discard t))
  ;; make readout destructive to the collector object
  ;; so that we can't accidentally hand off a list that
  ;; is still undergoing active mutation
  ;;
  ;; If user doesn't want to discard contents,
  ;; then hand him a copy of the contents as they exist at this moment.
  (with-locked-collector (c)
    (let ((lst (cdr (the cons (collector-hd c)))))
      (if discard
          (progn
            (collector-discard-contents c)
            lst)
        (copy-seq lst))
      )))
    
(defun collector-ncontents (c)
  (with-locked-collector (c)
    (length (cdr (the cons (collector-hd c))))
    ))

(defun collector-empty-p (c)
  (zerop (collector-ncontents c)))

(defun collector-append-item (c item)
  (with-locked-collector (c)
    (setf (collector-tl c)
          (cdr (the cons (rplacd (the cons (collector-tl c)) (list item))))
          )))

(defun collector-push-item (c item)
  (with-locked-collector (c)
    (setf (collector-hd c)
          (cons nil (the cons (rplaca (the cons (collector-hd c)) item)))
          )))

(defun collector-pop (c)
  (with-locked-collector (c)
    (let* ((lst (collector-contents c))
           (v   (car lst)))
      (unless (endp lst)
        (setf (collector-hd c) lst))
      v)))

(defun make-collector ()
  (make-instance '<collector>))
|#

;; ---------------------------------------------------------------------
#+:lispworks
(defun constituent (c)
  (and (graphic-char-p c)
       (not (lispworks:whitespace-char-p c))))

#-:lispworks
(progn
  (defvar *whitespace-chars*
      (list #\space #\tab #\newline #\return #\backspace #\Page))
  
  (defun whitespace-char-p (c)
    (member c *whitespace-chars*))
  
  (defun constituent (c)
    (and (graphic-char-p c)
         (not (whitespace-char-p c)))))

#|
;; show whitespace chars
(dolist (item
         (loop for ix from 0 below 256
               for c = (code-char ix)
               when (lw:whitespace-char-p c)
               collect (list ix c)))
  (format t "~%0x~2,'0x ~S" (first item) (second item)))

;; show graphics chars
(dolist (item
         (loop for ix from 0 below 256
               for c = (code-char ix)
               when (graphic-char-p c)
               collect (list ix c)))
  (format t "~%0x~2,'0x ~S" (first item) (second item)))
|#

(defun tokens (str &key (test 'constituent) test-not (start 0) end key)
  (let ((test (if test-not
                  (complement test-not)
                test)))
    (loop for p1 = (position-if test str :start start :end end :key key)
          while p1
          do (setf start (position-if-not test str :start p1 :end end :key key))
          collect (subseq str p1 (or start end))
          while start
          )))

(defun tokens-if (test str &rest args)
  (apply 'tokens str :test test args))

(defun tokens-if-not (test-not str &rest args)
  (apply 'tokens str :test-not test-not args))

(defun split-string (str &key delims (start 0) end key)
  (if delims
      (tokens-if-not (rcurry #'find #|c|# delims) str
                     :start start :end end :key key)
    (tokens str :start start :end end :key key)
    ))


(defun paste-strings (delim &rest args)
  (with-output-to-string (s)
    (when args
      (princ (car args) s)
      (dolist (arg (cdr args))
        (princ delim s)
        (princ arg s)
        ))
    ))
      
;; --------------------------------------------
;; PERFORM = Named LET

(defmacro perform (name bindings &body body)
  (let ((args (mapcar #'first bindings))
        (vals (mapcar #'second bindings)))
    `(labels ((,name ,args ,@body))
       (,name ,@vals))
    ))
  
;; ------------------------------------------------------------------
(defmethod longer ((x list) (y list))
  (do ((lx x (cdr lx))
       (ly y (cdr ly)))
      ((or (null lx)
           (null ly)) lx)))

(defmethod longer (x y)
  (> (length x) (length y)))

(defun filter (fn seq)
  (remove-if (complement fn) seq))

(defun group (seq n)
  (when (zerop n) 
    (error "zero length"))
  (if seq
      (let ((c (make-collector)))
        (perform rec ((seq seq))
          (cond ((zerop (length seq))
                 (collector-contents c))
                
                ((>= (length seq) n)
                 (collector-append-item c (subseq seq 0 n))
                 (rec (subseq seq n)))
                
                (t 
                 (collector-append-item c seq)
                 (collector-contents c)))
          ))
    ))

(defun flatten (x)
  (perform rec ((x   x)
                (acc nil))
    (cond ((null x) acc)
          ((atom x) (cons x acc))
          (t (rec (car x) (rec (cdr x) acc))))
    ))

(defun prune (test tree)
  (perform rec ((tree tree)
                (c    (make-collector)))
    (cond ((null tree) (collector-contents c))
          ((consp (car tree))
           (collector-append-item c
                                  (rec (car tree) (make-collector)))
           (rec (cdr tree) c))
          (t
           (unless (funcall test (car tree))
             (collector-append-item c (car tree)))
           (rec (cdr tree) c))
          )))

(defun find2 (fn lst)
  (do* ((l lst (cdr l))
        (val (funcall fn (car l))
             (funcall fn (car l))))
       (val (values (car l) val))))

(defun before (x y lst &key (test 'eql))
  (with-tail-pure-code
    (and lst
         (let ((first (car lst)))
           (cond ((funcall test y first) nil)
                 ((funcall test x first) lst)
                 (t (before x y (cdr lst) :test test))
                 )))))

(defun after (x y lst &key (test 'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test 'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn seq)
  (let ((pos (if-let (pos (position-if fn seq))
                     pos
                     (length seq))))
    (values (subseq seq 0 pos)
            (subseq seq pos))))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
    (let* ((wins (car lst))
           (max  (funcall fn wins)))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (when (> score max)
            (setf wins obj
                  max  score))))
      (values wins max))))

(defun best (fn lst)
  (and lst
       (let ((wins (car lst)))
         (dolist (obj (cdr lst))
           (if (funcall fn obj wins)
               (setf wins obj)))
         wins)))

(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
    (let ((c   (make-collector))
          (max (funcall fn (car lst))))
      (collector-append-item c (car lst))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (cond ((> score max)
                 (setf max score)
                 (collector-discard-contents c)
                 (collector-append-item c obj))

                ((= score max)
                 (collector-append-item c obj)))
          ))
      (values (collector-contents c) max))))

(defun drop (n seq)
  (subseq seq n))

(defun take (n seq)
  (subseq seq 0 n))

(defun split (n seq)
  (let ((hd (take n seq))
        (tl (drop n seq)))
    (values hd tl)))

;; -----------------------------------------------------
;; Mapping

#|
(defun mapa-b (fn a b &optional (step 1))
  (let ((c (make-collector)))
    (do ((i a (+ i step)))
        ((> i b) (collector-contents c))
      (collector-append-item c (funcall fn i))
      )))
|#
(defun mapa-b (fn a b &optional (step 1))
  (loop for i from a below b by step
        collect (funcall fn i)))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun map-> (fn start until-fn succ-fn)
  (let ((c (make-collector)))
  (do ((i start (funcall succ-fn i)))
      ((funcall until-fn i) (collector-contents c))
    (collector-append-item c (funcall fn i))
    )))

(defun mappend (fn &rest lsts)
  (apply 'append (apply 'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((c (make-collector)))
    (dolist (lst lsts)
      (dolist (obj lst)
        (collector-append-item c (funcall fn obj))))
    (collector-contents c)))

(defun rmapcar (fn &rest args)
  (if (some 'atom args)
      (apply fn args)
    (apply 'mapcar
           #'(lambda (&rest args)
               (apply 'rmapcar fn args))
           args)))

;; -------------------------------------------------------
;;
(defun readlist (&rest args)
  (values (read-from-string
           (mkstr "(" (apply 'read-line args) ")"))))

(defun prompt (&rest args)
  (apply 'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
   (let ((in (apply 'prompt args)))
     (if (funcall quit in)
         (return)
       (format *query-io* "~A~%" (funcall fn in))))))

(defun raw-mkstr (&rest args)
  (with-output-to-string (s)
      (dolist (a args) 
        (princ a s))
      ))

(defun mkstr (&rest args)
  (with-standard-io-syntax
    (apply #'raw-mkstr args)))


(defun correct-for-symbol-character-case (str)
  ;; a portable way to make symbol strings
  ;; Modern Mode vs ANSI
  (if (eql #\a (char (string :a) 0))
      (string-downcase (string str))
    (string-upcase (string str))))

(defun intern-symbol (str &optional package)
  (if package
      (intern (correct-for-symbol-character-case str) package)
    (intern (correct-for-symbol-character-case str))))

(defun symb (&rest args)
  (values (intern-symbol (apply 'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply 'mkstr args))))

(defun explode (sym)
  (map 'list (compose #'intern-symbol #'string) (symbol-name sym)))

;; ------------------------------------------------------------------
#+:lispworks
(defun pickfile (message &rest rest)
  (apply 'capi:prompt-for-file message rest))

(defun get-time-string (&optional short-form)
  (multiple-value-bind (secs mins hrs day mon year dow dst-p tz)
      (get-decoded-time)
    (let ((wkday (aref #("Monday" "Tuesday" "Wednesday"
                                  "Thursday" "Friday" "Saturday"
                                  "Sunday")
                       dow))
          (mth (aref #("January" "February" "March"
                                 "April" "May" "June" "July"
                                 "August" "September" "October"
                                 "November" "December")
                     (1- mon)))
          (zone (if (zerop tz)
						"GMT"
						(aref (if dst-p
								#("EDT" "CDT" "MDT" "PDT")
	                        #("EST" "CST" "MST" "PST"))
    	                  (- tz 5)))))
      (format nil "~A ~A ~A ~A  ~2,'0D:~2,'0D:~2,'0D ~A"
              (if short-form
                  (subseq wkday 0 3)
                wkday)
              (if short-form
                  (subseq mth 0 3)
                mth)
              day year hrs mins secs zone))
    ))

;; ----------------------------------------------------
#|
(defun map-from-below (fn from below &optional (by 1))
  (let ((c (make-collector)))
    (do ((ix from (+ ix by)))
        ((>= ix below) (collector-contents c))
      (collector-append-item c (funcall fn ix))
      )))
|#

(defun map-from-below (fn from below &optional (by 1))
  (loop for ix from from below below by by
        collect (funcall fn ix)))

(defun map-from-to (fn from to &optional (by 1))
  (map-from-below fn from (1+ to) by))

(defun map-from-for (fn from len &optional (by 1))
  (map-from-below fn from (+ from len) by))

;; ---------------------------------------------------------------------
(defun collect-where (lst sels &key (test #'identity) (key #'identity))
  (let ((c (make-collector)))
    (do ((l lst  (cdr l))
         (s sels (cdr s)))
        ((or (endp l)
             (endp s)) (collector-contents c))
      (if (funcall test (funcall key (car s)))
          (collector-append-item c (car l)))
      )))

(defun collect-where-not (lst sels &key (test #'identity) (key #'identity))
  (collect-where lst sels :test (complement test) :key key))

;; ---------------------------------------------------------------------
(defmethod where (predicate (proseq null) &key key)
  (declare (ignore predicate key))
  nil)

(defmethod where (predicate (proseq cons) &key (key #'identity))
  (let ((c (make-collector)))
    (do ((l proseq (cdr l))
         (ix 0     (1+ ix)))
        ((endp l) (collector-contents c))
      (if (funcall predicate (funcall key (car l)))
          (collector-append-item c ix))
      )))

(defmethod where (predicate (proseq vector) &key (key 'identity))
  (let ((c (make-collector)))
    (dotimes (ix (length proseq) (collector-contents c))
      (if (funcall predicate (funcall key (aref proseq ix)))
          (collector-append-item c ix))
      )))

(defmethod where (predicate (proseq array) &key (key 'identity))
  (where predicate (make-array (array-total-size proseq)
                               :displaced-to proseq
                               :element-type (array-element-type proseq))
         :key key))


(defun where-not (predicate &rest rest)
  (apply 'where (complement predicate) rest))


(defmethod subselect (proseq (where null))
  (declare (ignore proseq))
  nil)

(defmethod subselect (proseq (where cons))
  (subselect proseq (coerce where 'vector)))

(defmethod subselect ((proseq list) (where vector))
  (subselect (coerce proseq 'vector) where))

(defmethod subselect ((proseq vector) (where vector))
  (let* ((len  (length where))
         (rslt (if (plusp len)
                   (make-array len
                               :element-type (array-element-type proseq))
                 nil)))
    (when rslt
      (dotimes (ix len rslt)
        (setf (aref rslt ix) (aref proseq (aref where ix)))))
    ))

(defmethod subselect ((proseq array) (where vector))
  (subselect (make-array (array-total-size proseq)
                         :displaced-to proseq
                         :element-type (array-element-type proseq))
             where))


(defun indgen (n)
  (let ((rslt (make-array n :element-type 'integer)))
    (dotimes (ix n rslt)
      (setf (aref rslt ix) ix))))


(defun collect-if (predicate proseq &rest rest)
  (apply 'remove-if-not predicate proseq rest))

(defun collect-if-not (predicate proseq &rest rest)
  (apply 'remove-if predicate proseq rest))


(defun keep-if (predicate proseq &rest rest)
  (apply 'delete-if-not predicate proseq rest))

(defun keep-if-not (predicate proseq &rest rest)
  (apply 'delete-if predicate proseq rest))

;; ---------------------------------------------------------------------
;; Macros to take the pain out of passing strings to DLL's
;;
#+:lispworks
(progn
  (defconstant *null-string*
    (fli:make-pointer :address 0))
  
  (defun _with-cstring (str fn)
    (if str
	(fli:with-foreign-string (cstr nel nb) (mkstr str)
				 (declare (ignore nel nb))
				 (funcall fn cstr))
	(fli:with-coerced-pointer (p :type 'ct:uchar) *null-string*
				  (funcall fn p))))
  
  (defmacro with-cstring (binding &body body)
    `(_with-cstring ,(second binding)
		    #'(lambda (,(first binding))
			,@body)))
  
  (defmacro with-cstrings (bindings &body body)
    (if (null bindings)
	`(progn ,@body)
	`(with-cstring ,(first bindings)
	   (with-cstrings ,(cdr bindings) ,@body))))
  
  (defun actual-args (arglist)
    ;; Some args are default :constant args without a name
    ;; These should not be specified when calling the FLI function.
    ;; Just return the list of actual arguments.
    (remove-if #'(lambda (arg)
		   (and (consp arg)
			(eql :constant (first arg))))
	       arglist))
  
  (defun arg-name (arg)
    ;; some args are simple identifiers that default to type :int
    ;; and others are lists that follow the name with a C-type.
    ;; Just return the name of the arg.
    (if (consp arg)
	(first arg)
	arg))
  
  (defun arg-names (arglist)
    ;; Return the list of actual argument names.
    (mapcar #'arg-name arglist))
  
  
  (defmacro ez-define-foreign-function-receiving-c-string (name entry-name args
							   &rest rest)
    (with-gensyms (rslt dll-name)
      (let ((dll-argnames (arg-names (actual-args args))))
	`(progn
	   
	   (fli:define-foreign-function (,dll-name ,entry-name)
	       ,args
	     ,@rest)
	   
	   (defun ,name ,dll-argnames
	     (let ((,rslt (,dll-name ,@dll-argnames)))
	       (unless (fli:null-pointer-p ,rslt)
		 (fli:convert-from-foreign-string ,rslt)))))
	)))
  
  (defmacro ez-define-foreign-function-sending-c-strings (name entry-name args
							  &rest rest)
    (labels
	((is-cstring-arg (arg)
	   (and (consp arg)
		(equal (rest arg) '(ct:out-cstring)))))
      
      (let* ((actuals          (actual-args args))
	     (proto-list       (mapcar #'(lambda (arg)
					   (if (is-cstring-arg arg)
					       (list (gensym)
						     (arg-name arg))
					       (arg-name arg)))
				       actuals))
	     (actual-argnames  (arg-names actuals))
	     (cstring-bindings (remove-if-not #'consp proto-list))
	     (call-argnames    (arg-names proto-list))
	     (dll-name         (gensym)))
	
	`(progn
	   
	   (fli:define-foreign-function (,dll-name ,entry-name)
	       ,args ,@rest)
	   
	   (defun ,name ,actual-argnames
	     (with-cstrings ,cstring-bindings
	       ,(if (eql 'ct:in-cstring (getf rest :result-type))
		    (let ((rslt (gensym)))
		      `(let ((,rslt (,dll-name ,@call-argnames)))
			 (unless (fli:null-pointer-p ,rslt)
			   (fli:convert-from-foreign-string ,rslt))))
		    `(,dll-name ,@call-argnames))
	       )))
	)))
  
  (defmacro ez-define-foreign-function ((name entry-name) args &rest rest)
    (cond ((find '(ct:out-cstring) args :key 'cdr :test 'equal)
	   `(ez-define-foreign-function-sending-c-strings ,name ,entry-name
							  ,args
							  ,@rest))
	  ((eql 'ct:in-cstring (getf rest :result-type))
	   `(ez-define-foreign-function-receiving-c-string ,name ,entry-name
							   ,args 
							   ,@rest))
	  (t
	   `(fli:define-foreign-function (,name ,entry-name)
		,args
	      ,@rest))))
  )

;; ------------------------------------------------------------------
;;

(defmacro def-enum (&rest enums)
  "Generate a list of defconstant's beginning with zero.
If one if the elements of the list is a pair
then generate that and all following constants
beginning with the value of the second element.
This is C++ style enumerations."
  (let ((cur-ix -1)
        (base   0))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(mapcar
          #'(lambda (enum)
              (if (consp enum)
                  `(defconstant ,(first enum)
                     ,(progn
		        (setf cur-ix 0)
		        (setf base (second enum))))
                `(defconstant ,enum
                   (+ ,base ,(incf cur-ix)))
                ))
          enums))
    ))

;; ------------------------------------------------------------------
;; !!! Extra !!!

#|
(defmacro compose (&rest fns)
  (case (length fns)
    (0 #'identity)
    (1 (car fns))
    (otherwise
     (let ((args (gensym))
           (revfns (reverse fns)))
       (labels ((revlist (a b)
                         `(funcall ,b ,a)))
         `#'(lambda (&rest ,args)
              ,(reduce #'revlist revfns
                       :start 1
                       :initial-value
                       `(apply ,(first revfns) ,args)))
         )))
    ))
|#
#|
(defmacro compose (&rest fns)
  (cond ((null fns) `#'identity)
        ((endp (cdr fns)) (car fns))
        (t (let ((args (gensym)))
             `#'(lambda (&rest ,args)
                  ,(second
                    (foldr (lambda (f ans)
                             `(funcall (,(first ans) ,f ,(second ans))))
                           fns `(apply ,args))))
             ))))
|#

#|
(defmacro compose (&rest fns)
  (cond ((null fns) `#'identity)
        ((single fns) (car fns))
        (t (let ((args (gensym)))
             `#'(lambda (&rest ,args)
                  ,(foldr (lambda (f ans)
                            `(funcall ,f ,ans))
                          (butlast fns) `(apply ,(last1 fns) ,args)))
             ))
        ))
|#

(defun foldl (fn init seq)
  ;; fn should be a function of (accum item)
  (reduce fn seq
          :initial-value init))

(defun foldr (fn seq init)
  ;; fn should be a function of (item accum)
  (reduce fn seq :from-end t :initial-value init))

(defun compose (&rest fns)
  (cond ((null fns)   #'identity)
        ((single fns) (car fns))
        ((single (rest fns))
         (destructuring-bind (fn1 fn2) fns
           (lambda (&rest args)
             (funcall fn1 (apply fn2 args)))
           ))
        (t (let ((fn1 (last1 fns))
                 (fns (butlast fns)))
             #'(lambda (&rest args)
                 (foldr #'funcall fns (apply fn1 args)))
             ))
        ))

;; in ML these are referred to as sections
;; these actually correspond to the Dylan operators
;; secr ::= rcurry, secl ::= curry
(defun rcurry (f &rest suf-args)
  #'(lambda (&rest pref-args)
      (apply f (append pref-args suf-args))))

(defun curry (f &rest pref-args)
  #'(lambda (&rest suf-args)
      (apply f (append pref-args suf-args))))


#|
;; ML versions
(defun secr (f &rest suf-args)
  #'(lambda (&rest pref-args)
      (apply f (append pref-args suf-args))))

(defun secl (f &rest pref-args)
  #'(lambda (&rest suf-args)
      (apply f (append pref-args suf-args))))

;; in ML currying and uncurrying happens with functions of 2 args
(defun curry (f)
  (lambda (a)
    (lambda (b)
      (funcall f a b))))

(defun uncurry (f)
  (lambda (a b)
    (funcall (funcall f a) b)))
|#
#|
(defmacro rcurry (f &rest args)
  (let ((x (gensym)))
    `#'(lambda (,x)
	 (funcall ,f ,x ,@args))
    ))

(defmacro curry (f &rest args)
  (let ((x (gensym)))
    `#'(lambda (,x)
	 (funcall ,f ,@args ,x))
    ))
|#

(defun combine (op f1 f2)
  ;; operationally combine two functions f1 and f2
  ;; under binary operation op
  (lambda (x)
    (funcall op (funcall f1 x) (funcall f2 x))))


;; ---------------------------------------------------
;;
(defun eqlcond-clause (var val &rest body)
  (cond ((consp val)
         (if (eql 'QUOTE (first val))
             `((eql ,var ,val) ,@body)
           `((or ,@(mapcar #'(lambda (v)
                               `(eql ,var ,v))
                           val))
             ,@body)
           ))
        
        ((eq val :otherwise) `(t ,@body) )
        
        (t  `((eql ,var ,val) ,@body) )))

(defmacro eqlcond (var &rest clauses)
  (let ((gvar (gensym)))
    `(let ((,gvar ,var))
       (cond ,@(mapcar
                #'(lambda (clause)
                    (apply #'eqlcond-clause gvar clause))
                clauses)))
    ))

;; -------------------------------------------------------
;; Safe FLI interfaces with coercion of caller args to
;; types required by FLI interface
;;
#+:LISPWORKS
(defun coerce-fli-arg (arg)
  (destructuring-bind (name type &rest rest) arg
    (declare (ignore rest))
    (unless (eq name :constant)
      (case type
        ((:short :long :int) 
         `(,name (coerce ,name 'fixnum)))
        ((:float :single-float)
         `(,name (coerce ,name 'single-float)))
        ((:double :double-float)     
         `(,name (coerce ,name 'double-float)))
        (otherwise
         `(,name ,name))
        ))))

#+:LISPWORKS
(defmacro def-safe-fli-function ((name &rest args)
                                 user-args &rest other-args)
  (let ((cname       (intern-symbol (format nil "_~A" name)))
        (caller-args (delete :constant 
                             (mapcar #'first user-args)))
        (coercions   (delete nil 
                             (mapcar #'um:coerce-fli-arg user-args))))
    `(progn
       (fli:define-foreign-function (,cname ,@args)
           ,user-args
         ,@other-args)
       (defun ,name ,caller-args
         (let ,coercions
           (,cname ,@caller-args))))
    ))

#|
;; Example:
(def-safe-fli-function (diddly)
                       ((a :float)
                        (b :int))
                       :result-type :long)

   ==> (by macro-expansion)

(PROGN
  (FLI:DEFINE-FOREIGN-FUNCTION (_DIDDLY) ((A :FLOAT) (B :INT)) :RESULT-TYPE :LONG)
  (DEFUN DIDDLY (A B)
    (LET ((A (COERCE A 'SINGLE-FLOAT)) (B (COERCE B 'FIXNUM))) (_DIDDLY A B))))
|#

;; -----------------------------------------
;; Lazy evaluation and once-functions
;;

(defstruct <lazy-fn>
  fn)

(defun make-lazy-onetime-fn (fn)
  (let (ifn)
    (setf ifn #'(lambda ()
                  (let ((ans (funcall fn)))
                    (setf ifn (constantly ans))
                    ans)))
    ifn))

(defun make-lazy-once-thereafter-fn (fn-first fn-thereafter)
  (let (ifn)
    (setf ifn #'(lambda ()
                  (setf ifn fn-thereafter)
                  (funcall fn-first)))
    ifn))

(defmacro lazy-once (&body body)
  `(make-<lazy-fn> :fn (make-lazy-onetime-fn #'(lambda ()
                                                 ,@body))))

(defmacro lazy-once-thereafter ((&body body-first)
                                (&body body-thereafter))
  `(make-<lazyfn> :fn (make-lazy-once-thereafter-fn
                       #'(lambda ()
                           ,@body-first)
                       #'(lambda ()
                           ,@body-thereafter))))

#|
(defmacro lazy (&body body)
  `(make-<lazyfn> :fn #'(lambda ()
                          ,@body)))

(defmethod force (x)
  x)

(defmethod force ((x <lazy-fn>))
  (funcall (<lazy-fn>-fn x)))
|#

#|
(print (force (lazy (+ 2 3 4))))

(let ((x (lazy-once-thereafter ((print 1))
                               ((print 2)))))
  (loop repeat 3 do (force x)))

|#


#|
(defmacro defun-lazy (name args &body body)
  (let ((gargs (mapcar #'(lambda (name)
                           (declare (ignore name))
                           (gensym))
                       args)))
    `(defun ,name ,args
       ;; we have to avoid symbol-macrolet's like (a (force a))
       ;; because LW goes into infinite recursion on this
       ;; so first bind them to dummy names, and then symbol-macrolet from
       ;; forcing those dummy names back to their original names...
       (let ,(mapcar #'(lambda (name gname)
                         `(,gname ,name))
                     args gargs)
         (symbol-macrolet ,(mapcar #'(lambda (name gname)
                                       `(,name (force ,gname)))
                                   args gargs)
           (lazy ,@body))))))

(defun-lazy doit (a b c)
  (+ a b c)))

|#

;; -----------------------------------------------------
;; FirstTime, Thereafter
;; (firsttime &body)
;;
#|
(defmacro once-thereafter (body1 body2)
  (let ((fn (gensym)))
    `(let ((,fn))
       (setf ,fn
             #'(lambda ()
                 (setf ,fn #'(lambda ()
                               ,body2))
                 ,body1))
       #'(lambda ()
           (funcall ,fn)))
    ))

#| Example:

(let ((xx (once-thereafter (print 1) (print 2))))
  (loop repeat 3 do (funcall xx)))

|#

(defmacro with-once-thereafter-function (name body1 body2 &body body)
  (let ((fn (gensym)))
    `(let ((,fn (once-thereafter ,body1 ,body2)))
       (labels ((,name ()
                  (funcall ,fn)))
         ,@body))
    ))

#|
(with-once-thereafter-function onefn (print 1) (print 2)
  (loop repeat 3 do (onefn)))

|#


(defvar *once-functions*
  (make-hash-table))

(defmacro once (name &body body)
  (let ((fn  (or name (gensym)))
        (ans (gensym)))
    `(let ((,ans (gethash ',fn *once-functions* :not-present)))
       (if (eq ,ans :not-present)
           (setf (gethash ',fn *once-functions*)
                 (funcall #'(lambda ()
                              ,@body)))
         ,ans))
    ))
|#
#|
;; def-once-only creates a function that will only ever eval
;; its body forms just once. Period.
(defmacro def-once-only (name args &body body)
  (let ((ans  (gensym))
        (ansx (gensym)))
    `(let ((,ans nil))
       (defun ,name ,args
         (if ,ans
             (funcall ,ans)
           (let ((,ansx  (progn
                           ,@body)))
             (setf ,ans #'(lambda ()
                            ,ansx))
             ,ansx))
         ))
    ))

;; once-function creates a new lambda closure that will eval
;; its body forms only once afer it has been created. But every
;; time you execute the once-function form it creates a fresh 
;; once-function. So this is only locally useful, not globally.
;; For global once-function behavior see def-once-only above.
(defmacro once-function (args &body body)
  (let ((ans  (gensym))
        (ansx (gensym)))
    `(let ((,ans nil))
       #'(lambda ,args
           (if ,ans
               (funcall ,ans)
             (let ((,ansx  (progn
                             ,@body)))
               (setf ,ans #'(lambda ()
                              ,ansx))
               ,ansx))
           ))
    ))

;; once more without an internal let 
;; -- the inner lambda serves that purpose
(defmacro once-function (args &body body)
  (let ((ans (gensym)))
    `(let ((,ans nil))
       #'(lambda ,args
           (if ,ans
               (funcall ,ans)
             (funcall 
              (setf ,ans (funcall #'(lambda (,ans)
                                      #'(lambda ()
                                          ,ans))
                                  (progn
                                    ,@body))
                    ))
             )))
    ))

;; this is getting really twisted... but here's one that has no
;; branching logic inside the main function...
(defmacro once-function (args &body body)
  (let ((ans (gensym)))
    `(let ((,ans nil))
       #'(lambda ,args
           (funcall (or ,ans
                        (setf ,ans
                              (funcall #'(lambda (,ans)
                                           #'(lambda ()
                                               ,ans))
                                       (progn
                                         ,@body))
                              ))
                    )))
    ))

;; So...
;; We can define LET through the use of a lambda function:
;;
;;  (LET ((x <expr>)) <body>)
;;
;; same as
;;
;;  (funcall #'(lambda (x)
;;                 <body>)
;;     <expr>)
;;
;; ----------------------------------
;; Idiom:  (if x x y)  => (or x y)  ;; good one to call (either x y)
;;         (when x y)  => (and x y)
;;         (unless x y) => (and (not x) y)

(defmacro either (a b)
  `(or ,a ,b))

(defmacro both (a b)
  `(and ,a ,b))

(defmacro any (&rest args)
  `(or ,@args))

(defmacro all (&rest args)
  `(and ,@args))
|#

(defun true (&rest args)
  (declare (ignore args))
  t)

(defun false (&rest args)
  (declare (ignore args))
  nil)

;; -------------------------------------------------------
;; finite lists of values spanning a range
;;
(defun make-range (from to &key (by (if (>= to from) 1 -1)))
  (labels
      ((iota (tst)
         (loop for ix from 0
               for v = (+ from (* ix by))
               until (funcall tst v)
               collect v)))

    (cond ((zerop by)
           (and (= from to) (list from)))
          ((and (>= to from) (plusp by))
           (iota (rcurry #'> #|ix|# to)))
          ((and (< to from) (minusp by))
           (iota (rcurry #'< #|ix|# to)))
          (t nil))))

(defun range (a b &optional c)
  (if c
      (make-range a c :by (- b a))
    (make-range a b)))

;; ------------------------------------------------------------
;; once-thereafter objects

(defstruct once-thereafter
  first-time
  thereafter)

(defmethod get-value ((v once-thereafter))
  (shiftf (once-thereafter-first-time v) (once-thereafter-thereafter v)))


;; ------------------------------------------------------------
#|
;; Possibly useful idiom...
;;
;;  To make a binary function into a function that can be mapped against
;;  multiple sequences, possibly more than 2,
;;  as in (defun a+b (a b) (+ a b)) in (map 'vector #'a+b '(1 2 3) '(4 5 6) '(7 8 9))
;;  which should have the effect of (vector (reduce #'a+b '(1 4 7))
;;                                          (reduce #'a+b '(2 5 8))
;;                                          (reduce #'a+b '(3 6 9))) 
;;
;;   (um:compose (um:curry #'reduce #'<your-binary-function-here>) #'list)
|#

(defun largest-abs-value (a b)
  ;; return the argument having the largest absolute value
  (if (> (abs a) (abs b))
      a
    b))

(defun make-list-reducer (fn)
  (compose (curry #'reduce fn) #'list))

;; -----------------------------------------------------
;; Post incr and decr
 
(defmacro post-incf (place &optional (incr 1))
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((oldval (gensym)))
      `(let* (,@(mapcar #'list vars forms)
              (,oldval ,access)
              (,(car var) (+ ,oldval ,incr)))
         ,set
         ,oldval)
      )))
  
(defmacro post-decf (place &optional (decr 1))
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((oldval (gensym)))
      `(let* (,@(mapcar #'list vars forms)
              (,oldval ,access)
              (,(car var) (- ,oldval ,decr)))
         ,set
         ,oldval)
      )))


(defmacro with-slot-values (slots instance &body body)
  (let ((ginst (gensym)))
    `(let ((,ginst ,instance))
       (let (mapcar (lambda (pair)
                      `(,(first pair) (,(second pair) ,ginst)))
                    ,slots)
         ,@body))))

;; ----------------------------------------------------------------

(defmacro fn (args &body body)
  `(lambda ,args ,@body))

(defmacro if* (test tclause &rest fclauses)
  `(if ,test ,tclause
       ,(if (< (length fclauses) 2)
            (first fclauses)
          `(if* ,(first fclauses)
                ,(second fclauses)
                ,@(cddr fclauses))
          )))

(defmacro aif (test tclause &optional fclause)
  `(let ((it ,test))
     (if it ,tclause ,fclause)))

(defmacro aif* (test tclause &rest fclauses)
  `(let ((it ,test))
     (if it ,tclause
       ,(if (< (length fclauses) 2)
            (first fclauses)
          `(aif* ,(first fclauses)
                 ,(second fclauses)
                 ,@(cddr fclauses))
          ))))

(defun firsts-of (lst)
  (mapcar #'first lst))

(defun slice (seq start &optional (nel 1))
  (let* ((len (length seq))
         (actual-start (mod (if (minusp start)
                                (+ len start)
                              start)
                            len))
         (slen (min nel (- len actual-start))))
    (if (< slen nel)
	(concatenate (cond
		       ((listp   seq) 'list)
		       ((stringp seq) 'string)
		       ((vectorp seq) 'vector))
		     (subseq seq actual-start (+ actual-start slen))
			 (slice seq 0 (- nel slen)))
	(subseq seq actual-start (+ actual-start nel))
	)))

(defun left-part (seq nel)
  (slice seq 0 nel))

(defun right-part (seq nel)
  (slice seq (- nel) nel))

;; ---------------------------------------------------------------
;; Array mover

(defun move (src src-from dst dst-from nel)
  (replace dst src
           :start1 dst-from
           :start2 src-from
           :end1   (+ dst-from nel)))

;; ----------------------------------------------------------------

#-:LISPWORKS
(defun ceiling-pwr2 (n)
  (declare (fixnum n))
  (labels ((iter (n nsh)
             (declare (fixnum n nsh))
             (logior n (ash n nsh))))
    (declare (inline iter))
    (1+ (iter 
         (iter
          (iter
           (iter
            (iter (1- n) -1)
            -2)
           -4)
          -8)
         -16))))

#+:LISPWORKS
(defun ceiling-pwr2 (n)
  (declare (fixnum n))
  (declare (optimize (float 0)))
  (labels ((iter (n nsh)
             (declare (sys:int32 n))
             (declare (fixnum nsh))
             (sys:int32-logior n (sys:int32>> n nsh))))
    (declare (inline iter))
    (the fixnum
         (sys:int32-to-integer
          (sys:int32-1+
           (iter 
            (iter
             (iter
              (iter
               (iter (sys:int32-1- n) 1)
               2)
              4)
             8)
            16)))
         )))

(defun ceiling-log2 (n)
  (declare (fixnum n))
  (logcount (1- (ceiling-pwr2 n))))

(defun floor-pwr2 (n)
  (declare (fixnum n))
  (let ((c (ceiling-pwr2 n)))
    (if (= n c)
        n
      (ash c -1))))

(defun floor-log2 (n)
  (declare (fixnum n))
  (logcount (1- (floor-pwr2 n))))

(defun align-pwr2 (val pwr2)
  (declare (type fixnum pwr2)
           (type integer val))
  (let ((pwr2m1 (1- pwr2)))
    (logandc2 (+ val pwr2m1) pwr2m1)))

;; -----------------------------------------------------------------------

(defun format-error (err)
  "Routine to get the readable error message from a condition object."
  (with-output-to-string (s)
    (let ((*print-escape* nil))
      (print-object err s))))

;; ----------------------------------------------
;; convenience macro

(defmacro with-slot-accessors ((varname struct-name) slot-names &body body)
  `(with-accessors ,(mapcar (lambda (slot-name)
                              `(,(if (consp slot-name)
                                     (first slot-name)
                                   slot-name)
                                ,(intern-symbol (um:mkstr struct-name #\-
                                                          (if (consp slot-name)
                                                              (second slot-name)
                                                            slot-name)))
                                ))
                            slot-names)
       ,varname
     ,@body))


#+:LISPWORKS
(editor:setup-indent "with-slot-accessors" 2 2 4)

;; --------------------------------------------
;; BIND*

(defmacro bind* (bindings &body body)
  (perform iter ((bindings bindings))
    
    (labels ((invalid-syntax (bindings)
               (error "Invalid BIND* syntax: ~S" bindings))
             
             (more (bindings)
               (let ((decls (loop while (and (cdr bindings)
                                             (consp (cadr bindings))
                                             (member (caadr bindings) '(:DECLARE DECLARE)))
                                  do (pop bindings)
                                  collect
                                  `(declare ,@(cdr (first bindings)))
                                  )))
                 `(,@decls
                   ,@(if (cdr bindings)
                         `(,(iter (rest bindings)))
                       body))
                 )))

      (cond ((endp bindings) `(progn ,@body))
          
            ((symbolp (first bindings))
             `(let (,(first bindings))
                ,@(more bindings)))
          
            ((consp (first bindings))
             (let ((binding (first bindings)))
               
               (cond ((symbolp (first binding))

                      (cond ((bind*-handler (first binding))
                             ;; something we handle
                             (funcall (bind*-handler (first binding))
                                      binding
                                      bindings
                                      #'more))


                            ;; regular LET binding
                            (t (destructuring-bind (name val) (first bindings)
                                 `(let ((,name ,val))
                                    ,@(more bindings))))
                            ))
                   
                     ((consp (first binding))
                      ;; destructuring binding
                      (destructuring-bind (lst val) (first bindings)
                        `(destructuring-bind ,lst ,val
                           ,@(more bindings))))
                   
                     (t
                      (invalid-syntax bindings))
                     )))
          
            (t (invalid-syntax bindings))
            ))))

(defun bind*-handler (symbol)
  (get symbol 'bind*-handler))

(defmacro define-bind*-handler (symbol (binding-name more-bindings-name) &body body)
  (let ((gbindings (gensym))
        (gmore     (gensym)))
    `(setf (get ,symbol 'bind*-handler)
           #'(lambda (,binding-name ,gbindings ,gmore)
               (symbol-macrolet ((,more-bindings-name (funcall ,gmore ,gbindings)))
                 ,@body)))
    ))

#+:LISPWORKS
(editor:setup-indent "define-bind*-handler" 2 2)

(define-bind*-handler :ACCESSORS (binding more-bindings)
  (destructuring-bind (a-bindings obj) (rest binding)
    `(with-accessors ,(mapcar #'(lambda (a-binding)
                                  (if (consp a-binding)
                                      a-binding
                                    (list a-binding a-binding)))
                              a-bindings)
         ,obj
       ,@more-bindings)))

(define-bind*-handler :STRUCT-ACCESSORS (binding more-bindings)
  (destructuring-bind (struct-type slot-bindings obj) (rest binding)
    `(with-slot-accessors (,obj ,struct-type) ,slot-bindings
       ,@more-bindings)))

(define-bind*-handler :SLOTS (binding more-bindings)
  (destructuring-bind (slot-bindings obj) (rest binding)
    `(with-slots ,slot-bindings ,obj
       ,@more-bindings)))
                      
(define-bind*-handler :SYMBOL-MACRO (binding more-bindings)
  (destructuring-bind (name form) (rest binding)
    `(symbol-macrolet ((,name ,form))
       ,@more-bindings)))

(define-bind*-handler :VALUES (binding more-bindings)
  (destructuring-bind (names form) (rest binding)
    `(multiple-value-bind ,names ,form
       ,@more-bindings)))
                        
#|
(bind* ((a 1)
        (b 2)
        (:values (x y z) doit)
        ((a c &key (d 5) &rest xs) doit2))
  body)
(bind* ((a 1)
        (b 2)
        (:values (x y z) (values 15 22 34 55))
        ((d e &rest xs &key (g 99) &allow-other-keys) '(101 102 :g 13 :h 88)))
  (list a b x y z d e xs g))
|#

;; --------------------------------------------------------------

(defun binsearch (low-index hi-index compare-fn)
  (declare (type fixnum low-index hi-index))
  ;; General utility binary search routine.
  ;; low-index = starting index of table, high-index is 1 beyond table's last index.
  ;; compare-fn is a user provided comparison routine of one argument, the index,
  ;; and it should return <0, =0, >0 for each index.
  ;; returns: found, ixu
  ;;
  ;; When found is true, ixu is its index location
  ;; When found is false, ixu is where it would have to be inserted for key < key[ixu]
  ;; each index. Routine stops when comparison yields 0, or when the table is exhausted.
  ;; Comparison values of <0 indicate that the index is too high, >0 indicates it is too low.
  ;;
  (um:perform search ((ixl (1- low-index))
                      (ixu hi-index))
    (declare (type fixnum ixl ixu))
    (cond ((> (- ixu ixl) 1)
           (let* ((ixm (truncate (+ ixu ixl) 2))
                  (c (funcall compare-fn ixm)))
             (declare (type fixnum ixm c))
             (cond ((= c 0) (values t ixm)) ;; found it!
                   
                   ((< c 0) (search ixl ixm))
                   
                   (t       (search ixm ixu))
                   )))

          (t (values nil ixu))
          )))

;; --------------------------------------------------------------

;; --------------------------------------

;; -- end of usefull_macros.lisp -- ;;
