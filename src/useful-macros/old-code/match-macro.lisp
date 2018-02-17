
(in-package "USEFUL-MACROS")

;; -----------------------------------------------------------------
;; row-type encodes a #T(TypeName :SLOT-NAME Pat :SLOT-NAME2 Pat2 .. ) template
;; This permits matching against selected slots, listed explicitly in the template
;; against structure slots of type TypeName, igoring those slots of the value struct
;; that haven't been mentioned in the template.
;;
;; NOTE: we can also match against #S(Typename ...), but that expects to match
;; against every known slot of the struct type. Those not explicitly mentioned in the
;; #S() pattern receive their default values. So this isn't quite as convenient as the #T()
;; template types. Every slot of a #S() pattern should be listed. Only those slots of interest
;; need to be listed in a #T() template pattern.
;;

#+(OR :LISPWORKS :CLOZURE :ALLEGRO)
(defstruct row-type
  type slots pats)

#+(OR :LISPWORKS :CLOZURE :ALLEGRO)
(set-dispatch-macro-character #\# #\T
   #'(lambda (s c n)
       ;; read in the template and pre-process to select out struct type,
       ;; slot-names, and slot patterns
       (declare (ignore c n))
       (let* ((lst   (read s t nil t))
              (type  (car lst))
              (plist (cdr lst))
              (pairs (um:group plist 2))
              (keys  (mapcar #'first pairs))
              (pats  (mapcar #'second pairs))
              (class (find-class type))
              (all-names #+:LISPWORKS (structure:structure-class-slot-names class)
                         #+:CLOZURE   (mapcar #'ccl:slot-definition-name (ccl:class-slots class))
                         #+:ALLEGRO   (mapcar #'clos:slot-definition-name (clos:class-slots class))
                         )
              (slots (mapcar #'(lambda (key)
                                 (or (find key all-names
                                           :test #'string=)
                                     (error "Structure type ~A has no slot named ~A"
                                            type key)))
                             keys)))
         (make-row-type
          :type  type
          :slots slots
          :pats  pats) )))

#+(OR :CLOZURE :ALLEGRO)
(defmethod make-load-form ((obj row-type) &optional environment)
  (make-load-form-saving-slots obj :environment environment))
  

;; --------------------------------------------------------
;; Pattern Matching

#| ;; unused
(defun needs-symbol (sym)
  (unless (symbolp sym)
    (error "Symbol Required: ~A" sym)))
|#

(defun is-match-any-and-ignore-symbol? (sym)
  (and (symbolp sym)
       (string= "_" sym)))

(defun augment-args (args sym)
  (cond ((or (is-match-any-and-ignore-symbol? sym)
             (stringp sym)
             (eql 't sym)  ;; stands for anything but nil
             (eql 'nil sym)  ;; can this happen?
             (keywordp sym)
             (numberp sym)
             (characterp sym))
         args)

        ((symbolp sym)
         (adjoin sym args))

        (t (error "??? in pattern: ~S" sym))))

(defun lambda-list-of-pattern (pattern)
  (um:perform iter ((state :not-in-tail)
                    (lst   pattern)
                    (args  nil))
    (cond
     ((null lst)  args)
     
     ((consp lst)
      (um:bind*
          (((hd &rest tl) lst))
        
        (case hd
          (QUOTE  args)
          
          ((&OPTIONAL &AUX &KEY &ALLOW-OTHER-KEYS)
           (iter :in-tail tl args))
          
          (&REST
           (um:bind*
               (((sym &rest tl) tl))
             
             (iter :in-tail tl (augment-args args sym))
             ))
          
          (:AS
           (um:bind*
               (((pat1 pat2) tl))
             
             (iter state pat2
                   (iter state pat1 args))
             ))
          
          (t
           (case state
             (:in-tail  (augment-args args hd))
             
             (t         (iter state tl
                              (iter state hd args)))
             )))))
     
     ((vectorp lst)
      (um:perform iterv ((ix   0)
                         (args args))
        (if (>= ix (length lst))
            args
          (iterv (1+ ix) (iter state (aref lst ix) args)))
        ))

     #+(OR :LISPWORKS :CLOZURE :ALLEGRO)
     ((row-type-p lst)
      (iter state (row-type-pats lst) args))
     
     #+(OR :LISPWORKS :CLOZURE :ALLEGRO)
     ((typep lst 'structure-object)
      (um:perform iters ((slots #+:LISPWORKS (structure:structure-class-slot-names
                                              (class-of lst))
                                #+:CLOZURE   (ccl:class-slots (class-of lst)))
                         (args  args))
        (if (endp slots)
            args
          (iters (cdr slots) (iter state (slot-value lst (car slots)) args))
          )))

     (t  (augment-args args lst))
     )))

#|
(um:match '(1 2 3 4) ((a b &rest c) (list a b c)))
(lambda-list-of-pattern '(a b &rest c))
|#

(defun compute-match-clause-form (args body)
  (if (and (consp body)
           (eq (first body) :WHEN))
      `(LIST
        (LAMBDA ,args
          (DECLARE (IGNORABLE ,@args))
          ,(second body))
        (LAMBDA ,args
          (DECLARE (IGNORABLE ,@args))
          ,@(cddr body)))
    `(LAMBDA ,args
       (DECLARE (IGNORABLE ,@args))
       ,@body)
    ))

(defun patterns-and-lambdas (bindings)
  (um:bind*
      ((patterns (mapcar #'first bindings))
       (arglsts  (mapcar #'lambda-list-of-pattern patterns))
       (bodies   (mapcar #'rest  bindings))
       (clauses  (mapcar #'compute-match-clause-form arglsts bodies)))
    
    (values patterns clauses)))

;; -----------------------------------------
;; Runtime portion of MATCH

(defun eql-tree (v1 v2)
  (cond ((consp v1)
         (and (consp v2)
              (eql-tree (first v1) (first v2))
              (eql-tree (rest v1) (rest v2))))

        ((vectorp v1)
         (and (vectorp v2)
              (= (length v1) (length v2))
              (eql (array-element-type v1) (array-element-type v2))
              (every #'eql-tree v1 v2)))

        ((arrayp v1)
         (and (arrayp v2)
              (equal (array-dimensions v1) (array-dimensions v2))
              (let ((typ1 (array-element-type v1))
                    (typ2 (array-element-type v2)))
                (and (eql typ1 typ2)
                     (let ((v1v (make-array (array-total-size v1)
                                            :element-type typ1
                                            :displaced-to v1))
                           (v2v (make-array (array-total-size v2)
                                            :element-type typ2
                                            :displaced-to v2)))
                       (every #'eql-tree v1v v2v))
                     ))))
        
        (t (eql v1 v2))
        ))
                        
(defun match-1 (val pattern)
  (um:perform iter ((val     val)
                    (pattern pattern)
                    (accum   nil))
    (cond

     ;; put row-type pattern matches first, since we seem to be using them
     ;; a lot...
     #+(OR :LISPWORKS :CLOZURE :ALLEGRO)
     ((row-type-p pattern)
      ;; #T() template patterns
      (and (typep val (row-type-type pattern))
           (um:perform itert ((slots (row-type-slots pattern))
                              (pats  (row-type-pats pattern))
                              (accum accum))
               (if (endp slots)
                   (values accum t)
                 (um:bind*
                     ((slot (car slots))
                      (pat  (car pats))
                      (:values (accum ok)
                       (iter (slot-value val slot) pat accum)))
                   (if ok
                       (itert (cdr slots) (cdr pats) accum))
                   )))
           ))
                       
     ;; constant patterns (sort of...)
     ((null pattern)
      (and (null val)
           (values accum t)))
     
     ((eql 't pattern)
      (and val
           (values accum t)))
     
     ((is-match-any-and-ignore-symbol? pattern)
      (values accum t))
     
     ((keywordp pattern)
      (and (eq val pattern)
           (values accum t)))
     
     ((numberp pattern)
      (and (numberp val)
           (= val pattern)
           (values accum t)))
     
     ((stringp pattern)
      (and (stringp val)
           (string= val pattern)
           (values accum t)))
     
     ((characterp pattern)
      (and (characterp val)
           (char= val pattern)
           (values accum t)))
     
     ;; binding patterns
     ((symbolp pattern)
      (um:bind*
          ((pair (assoc pattern accum)))
        
        (if pair
            (and (eql-tree val (rest pair))
                 (values accum t))
          (values (acons pattern val accum) t))
        ))
     
     ((vectorp pattern)
      (and (vectorp val)
           (= (length val) (length pattern))
           (um:perform iterv ((ix    0)
                              (accum accum))
             (if (>= ix (length pattern))
                 (values accum t)
               
               (um:bind*
                   ((:values (accum ok) (iter (aref val ix) (aref pattern ix) accum)))
                 
                 (if ok
                     (iterv (1+ ix) accum))
                 )))))
     
     ;; recursive patterns
     ((consp pattern)
      (um:bind*
          (((pat-hd &rest pat-tl) pattern))
        
        ;; AS patterns
        (cond
         ((eq :AS pat-hd)
          (um:bind*
              (((pat1 pat2) pat-tl)
               (:values (accum ok) (iter val pat1 accum)))

            (and ok
                 (iter val pat2 accum))
            ))
         
         ((eql 'QUOTE pat-hd)
          (and (eql val (second pattern))
               (values accum t)))

         ((eql '&REST pat-hd)
          (and (consp val)
               (iter val (first pat-tl) accum)))
         
         (t
          ;; list patterns
          (and (consp val)
               (um:bind*
                   (((val-hd &rest val-tl) val)
                    (:values (accum ok) (iter val-hd pat-hd accum)))
                 
                 (and ok
                      (iter val-tl pat-tl accum))
                 )))
         )))

     #+(OR :LISPWORKS :CLOZURE :ALLEGRO)
     ((typep pattern 'structure-object)
      ;; #S() structure patterns
      (and (typep val (type-of pattern))
           (um:perform iters ((slots #+:LISPWORKS (structure:structure-class-slot-names
                                                   (class-of pattern))
                                     #+:CLOZURE   (ccl:class-slots (class-of pattern)))
                              (accum accum))
             (if (endp slots)
                 (values accum t)
               
               (um:bind*
                   ((slot (car slots))
                    (:values (accum ok) (iter (slot-value val slot)
                                              (slot-value pattern slot)
                                              accum)))
                 (if ok
                     (iters (cdr slots) accum))
                 )))))
    
     ;; huh??
     (t nil)
     
     )))

(define-condition match-failure (error)
  ()
  (:report "Match Failure"))

(defvar $match-failure-exn
  (make-condition 'match-failure))

(defun find-match (val patterns clauses)
  ;; for each pattern there is a corresponding clause
  ;; If the pattern included a :WHEN guard, then the corresponding
  ;; clause is a pair of lambdas. The first one is the guard clause
  ;; and the second is the normal clause.
  (um:perform iter ((patterns patterns)
                    (clauses  clauses))
    (if patterns  ;; until patterns exhausted
        (um:bind*
            (((pattern &rest patterns-tail) patterns)
             ((clause  &rest clauses-tail) clauses)
             (:symbol-macro next (iter patterns-tail clauses-tail))
             (:values (bindings ok) (match-1 val pattern)))  ;; try pattern match
          
          (if ok
              (um:bind*
                  ((args (mapcar #'rest bindings)))
                
                (if (consp clause) ;; handle WHEN guards
                    (um:bind*
                        (((when-guard clause) clause))
                      
                      (if (apply when-guard args)
                          (values args clause)
                        
                        ;; else continue searching for match
                        next))
                  
                  ;; else - no guard, we matched, do clause
                  (values args clause)))
            
            ;; else - continue searching for a match
            next))
      
      ;; else - no more patterns - generate match-fail error
      nil)))


(defun match-fail ()
  (error $match-failure-exn))

(defun do-match (val patterns clauses)
  (um:bind*
      ((:values (args clause) (find-match val patterns clauses)))
    
    (if clause
        (lambda () (apply clause args))
      #'match-fail)
    ))

(defun do-match-macro-expansion (arg bindings &optional fn)
  (um:bind*
      ((:values (patterns clauses) (patterns-and-lambdas bindings)))
    
    (if fn
        ;; what the heck!?
        `(funcall (DO-MATCH ,(funcall fn arg) ',patterns
                            ,(funcall fn `(LIST ,@clauses))
                            NIL
                            #'match-fail))
      `(funcall (DO-MATCH ,arg ',patterns (LIST ,@clauses)))
      )))

(defmacro match (arg &rest bindings)
  (do-match-macro-expansion arg bindings))


#+:LISPWORKS
(editor:setup-indent "match" 1 2)

#|

(match 15
  (:x :not)
  (x  (1+ x)))



|#
