;; queries.lisp -- Object ID Sets and OID files
;; --------------------------------------------------------------------------------------
;; OkeanosMM -- memory mapped database system
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  03/09
;; --------------------------------------------------------------------------------------
#|
Complement of A = (NOT A)  [in set-theoretic terms, called A^c]
(NOT A) = U - A  [in set-theoretic terms, called U \ A; in Lisp term = set-difference]

DeMorgan's Laws:
(NOT (OR A B)) = (AND (NOT A) (NOT B))
(NOT (AND A B)) = (OR (NOT A) (NOT B))

Identities:
(OR A (NOT A)) = U
(AND A (NOT A)) = NIL
(NOT NIL) = U
(NOT U) = NIL
If (MEMBER A B) then (MEMBER (NOT B) (NOT A))
(NOT (NOT A)) = A

(- A B) = (AND A (NOT B))
(NOT (- A B)) = (OR (NOT A) B)


|#
;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

;; -----------------------------------------------
;; Operations: LET, AND, OR, NOT, :SET-, :SYM-SET-, :XOR
;; Factors: (= slot-name value)
;;          (:RANGE slot-name from-value)
;;          (:RANGE slot-name from-value to-value)
;; ------------------------------------------------

(defvar *query-leaf-table*)
(defvar *query-local-bindings*)

(defun enter-leaf (e)
  ;; add a Sexpr to the list of known sexprs. Give it a name
  (if-let (pair (find e *query-leaf-table* :test 'eql-tree :key 'car))
      ;; the use of eql-tree is needed here because it retains case-sensitive
      ;; string and character comparisons, while also descending tree structure like equalp
      (cdr pair)
    (let ((name (gensym)))
      (push (cons e name) *query-leaf-table*)
      name)))

(defun gensymp (sym)
  (and (symbolp sym)
       (null (symbol-package sym))))

(defun term-compare< (a b)
  (cond ((and (symbolp a)
              (symbolp b))
         (string< a b))

        ((symbolp a) t)
        ((symbolp b) nil)
        (t t)))

(defun reorder-terms (expr)
  ;; split the terms into positive and negative groups
  ;; sort each group by term to give a constant ordering
  (let ((terms (foldl #'(lambda (acc e)
                        (match e
                          (('_USET- _ ex) (push ex (cadr acc)))
                          (_              (push e (car  acc))))
                        acc)
                      (list nil nil) expr)))
    (values (sort (car terms)  'term-compare<)
            (sort (cadr terms) 'term-compare<))))

(defun trim-expr (es)
  (reorder-terms
   (mapcar 'identify-leaves es)))

(defun identify-leaves (expr)
  ;; find the leaf terms of the query expression, and rewrite
  ;; the overall expression to simpler terms.

  ;; In particular, we bubble up any negatives to top level
  
  (match expr
    (NIL NIL)
    
    (('OR e1 . es)
     (multiple-value-bind (pos neg) (trim-expr (cons e1 es))
       (if neg
           (let ((n-and (identify-leaves `(_AND ,@neg))))
             (if pos
                 (let ((p-or (identify-leaves `(_OR ,@pos))))
                   (identify-leaves `(NOT (_SET- ,n-and ,p-or))))
               (identify-leaves `(NOT ,n-and))))
         (identify-leaves `(_OR ,@pos)))))

    (('AND e1 . es)
     (multiple-value-bind (pos neg) (trim-expr (cons e1 es))
       (if neg
           (let ((n-or (identify-leaves `(_OR ,@neg))))
             (if pos
                 (let ((p-and (identify-leaves `(_AND ,@pos))))
                   (identify-leaves `(_SET- ,p-and ,n-or)))
               (identify-leaves `(NOT ,n-or))))
         (identify-leaves `(_AND ,@pos)))))

    ((:XOR e1 . es)
     (multiple-value-bind (pos neg) (trim-expr (cons e1 es))
       (let ((xor-set (identify-leaves `(_XOR ,@pos ,@neg))))
         (if (evenp (length neg))
             xor-set
           (identify-leaves `(NOT ,xor-set))))))

    ((:SET- e1 . es)
     (let ((e1x (identify-leaves e1)))
       (match e1x
         (('_USET- . _)
          (identify-leaves `(NOT (OR ,e1 ,@es))))
         (_
          (multiple-value-bind (pos neg) (trim-expr es)
            (let ((p-set- (identify-leaves `(_SET- ,e1x ,@pos))))
              (if neg ;; we already know we have at least one positive (the first)
                  (let ((n-and (identify-leaves `(_AND ,@neg))))
                    (identify-leaves `(_AND ,p-set- ,n-and)))
                p-set-)))) )))
         
    (('NOT ('NOT e))      (identify-leaves e))
    (('NOT e)             (identify-leaves `(_USET- ,(enter-leaf '_uset) ,(identify-leaves e))))

    ((:OR . es)           (identify-leaves `(OR ,@es)))
    ((:AND . es)          (identify-leaves `(AND ,@es)))
    ((:NOT . es)          (identify-leaves `(NOT ,@es)))
    ((:NOR . es)          (identify-leaves `(NOT (OR ,@es))))
    ((:NAND . es)         (identify-leaves `(NOT (AND ,@es))))
    ((:NXOR . es)         (identify-leaves `(NOT (:XOR ,@es))))
    ((:NSET- . es)        (identify-leaves `(NOT (:SET- ,@es))))

    ((:all)                (enter-leaf '_USET))
    ((:as e (':RANGE . _)) (enter-leaf e))
    ((:as e ('= . _))      (enter-leaf e))
    ((':MEMBER slot-name member-list)
     (identify-leaves `(_OR ,@(mapcar #'(lambda (item)
                                        `(= ,slot-name ,item))
                                      member-list))))

    (('_USET- e e)             nil)
    (('_USET- _ ('_USET- _ e)) (identify-leaves e))
    (('_USET- USET)            USET)
    (('_USET- . _)             expr)

    ;; ------------------------------------------------------------
    ;; by the time we reach the following clauses, all the terms are positive
    ;; and all have been scanned for leaf terms
    ;; ------------------------------------------------------------

    (('_OR)               nil)
    (('_OR e1)            e1)
    (('_OR (_SET- e1 e2) (_SET- e2 e1) . es)
     (identify-leaves `(_OR (_XOR ,e1 ,e2) ,@es)))
    (('_OR e1 e2 . es)    (identify-leaves `(_OR (_OR ,e1 ,e2) ,@es)))
    (('_OR _ _)           expr)

    (('_AND)              nil)
    (('_AND e1)           e1)
    (('_AND e1 e2 . es)   (identify-leaves `(_AND (_AND ,e1 ,e2) ,@es)))
    (('_AND _ _)          expr)

    (('_XOR e1)           e1)
    (('_XOR)              nil)
    (('_XOR e e . es)     (identify-leaves `(_XOR ,@es)))
    (('_XOR e1 e2 . es)   (identify-leaves `(_XOR (_XOR ,e1 ,e2) ,@es)))
    (('_XOR _ _)          expr)

    (('_SET-)             nil)
    (('_SET- e1)          e1)
    (('_SET- e e . _)     NIL)
    (('_SET- ('_OR e1 e2) ('_AND e1 e2) . es)
     (identify-leaves `(_SET- (_XOR ,e1 ,e2) ,@es)))
    (('_SET- e1 e2 . es)  (identify-leaves `(_SET- (_SET- ,e1 ,e2) ,@es)))
    (('_SET- _ _)         expr)
    
    (('LET bindings . body)
     (let ((*query-local-bindings* *query-local-bindings*))
       `(LET ,(mapcar #'(lambda (binding)
                        (destructuring-bind (name val) binding
                          (push name *query-local-bindings*)
                          (list name (identify-leaves val))))
                      bindings)
          ,(when-let (clause (car (last body)))
              (identify-leaves clause)) )))

    (sym :when (member sym *query-local-bindings*) sym)

    (sym :when (gensymp sym) sym)
    
    (_ (error "Ill-formed query: ~A" expr))
    ))

(defun tree-count (item tree &key (test 'eql))
  (nlet iter ((tree  tree)
              (count 0))
    (if tree
        (cond ((consp tree)
               (let ((ct-car (iter (car tree) count)))
                 (iter (cdr tree) ct-car)))
              (t (if (funcall test item tree)
                     (1+ count)
                   count)))
      count)))

(defun compile-query (expr)
  (let* ((*query-leaf-table*     nil)
         (*query-local-bindings* nil)
         (new-expr (identify-leaves expr)))
    (nlet iter ((table    *query-leaf-table*)
                (bindings nil)
                (expr     new-expr))
      (if table
          (destructuring-bind (e . l) (car table)
            (if (> (tree-count l expr) 1)
                (iter (cdr table)
                      (cons (list l e) bindings)
                      expr)
              (iter (cdr table)
                    bindings
                    (subst e l expr))))
        (if bindings
            `(let ,bindings
               ,expr)
          expr)))))

#|
(compile-query '(and (not (= name "Dave"))
                 (not (= location "Tucson"))
                 (not (= name "dave"))))
(compile-query '(= name "Dave"))
(compile-query '(and (= name "Dave")
                 (not (= location "Boston"))))

(compile-query '(or (and (= name "Dave")
                     (= location "Boston"))
                (and (not (= location "Boston"))
                     (not (= name "Dave")))))
(compile-query '(or (and (= name "Dave")
                     (not (= location "Boston")))
                (and (not (= name "Dave"))
                     (= location "Boston"))))

(compile-query '(and (or (= name "Dave")
                     (= location "Boston"))
                 (or (not (= location "Boston"))
                     (not (= name "Dave")))))
(compile-query '(:member name ("Dave" "Panos")))
(compile-query '(and (= name "Dave")
                     (not (= name "Dave"))))
(compile-query '(:xor (= name "Dave")
                     (= name "Dave")))
(compile-query '(:set- (= name "Dave")
                     (= name "Dave")))
(compile-query nil)
|#

;; -----------------------------------------------------
;; the executive - takes user query spec and reduces to
;; compact efficient form, then executes the resulting script.

(defun query-engine (cquery test universe-fn select-eq-fn select-range-fn)
  (let ((*query-local-bindings* nil))
    (nlet parse ((e cquery))
      ;; (print e)
      (match e
        
        (('_OR e1 e2)
         (let ((v1 (parse e1))
               (V2 (parse e2)))
           (union v1 v2
                  :test test)))
        
        (('_AND e1 e2)
         (let ((v1 (parse e1))
               (v2 (parse e2)))
           (intersection v1 v2
                         :test test)))

         (('_XOR e1 e2)
          (let ((v1 (parse e1))
                (v2 (parse e2)))
            (set-exclusive-or v1 v2
                              :test test)))
         
         (('_SET- e1 e2)
          (let ((v1 (parse e1))
                (v2 (parse e2)))
            (set-difference v1 v2
                            :test test)))
         
         ;; ---------
         
        (('_USET- e1 e2)
         ;; complement = NOT, e1 is U
         (let ((v1 (parse e1))
               (v2 (parse e2)))
           (set-difference v1 v2
                           :test test)))
        
         ;; ---------
         
         (('= slot-name value)
          (funcall select-eq-fn slot-name value))
         
         ((':range slot-name from)
          (funcall select-range-fn slot-name from nil))
         
         ((':range slot-name from limit)
          (funcall select-range-fn slot-name from limit))

         ('_USET
          (funcall universe-fn))
         
         ;; ---------
         
         (('LET bindings . body)
          (let ((*query-local-bindings* *query-local-bindings*))
            (loop for binding in bindings do
                  (let ((name (car binding))
                        (val  (funcall (parse (cadr binding)))))
                    (push (list name val) *query-local-bindings*)))
            (parse (car (last body))) )) ;; only the last one has any effect

         ;; ----------
         
         (sym
          (cadr (find sym *query-local-bindings* :key 'car)))
         
         ))))

  
;; ---------------------------------------------------------------------
;; Persistent-Class Instances...

(defmethod run-query ((class-name symbol) cquery)
  (query-engine cquery
                'oid=
                (deferred (find-instances* class-name))
                (curry 'fetch-instances-for-slot class-name)
                (curry 'find-instances-for-slot class-name)))

(defmethod query ((class-name symbol) expr-list)
  (when-let (cquery (compile-query expr-list))
    (perform 'run-query class-name cquery)))

;; ---------------------------------------------------------------------
;; File-Tables...

(defmethod run-query ((table file-table) cquery)
  (query-engine cquery
                (get-key-column-test (table-schema table))
                (deferred (fetch-all-rows table))
                (curry 'fetch-rows-for-column table)
                (curry 'find-rows-for-column table)))

(defmethod query ((table file-table) expr-list)
  (when-let (cquery (compile-query expr-list))
    (run-query table cquery)))

;; ---------------------------------------------------------------------
;; File-BTrees...

(defmethod run-query ((btree file-btree) cquery)
  (let ((keyfn (btree:key-fn btree)))
    (query-engine cquery
                  (btree:compare-fn btree)
                  (deferred
                    (um:accum acc
                      (btree:map-tree btree
                                      (compose #'acc keyfn))))
                  (curry 'btree:find-item btree)
                  #'(lambda (slot-ignored from to)
                      (declare (ignore slot-ignored))
                      (um:accum acc
                        (btree:map-tree btree (compose #'acc keyfn)
                                        :from from :to to)))
                  )))

(defmethod query ((btree file-btree) expr-list)
  (when-let (cquery (compile-query expr-list))
    (run-query btree cquery)))

;; -----------------------------
;; List of system queries
#|

;; list of users that have read the database
(dolist (user-id (query (user-mappings) '(:all)))
  (print (fetch-row `(:user-id ,user-id) (user-mappings))))

;; list of transctions against the database
(dolist (tid (query (transaction-directory) '(:all)))
  (print (fetch-row `(:tid ,tid) (transaction-directory))))

;; the oid mappings table
(dolist (oid (query (oid-mappings) '(:all)))
  (print (fetch-row `(:oid ,oid) (oid-mappings))))

;; the string pool contents
(dolist (str (query (string-pool-btree
                     (string-pool-pointer
                      (database-mapper *current-okeanos-db*)))
                    '(:all)))
  (print str))

;; the list of persistent classes registered with the database
(dolist (class-estr (query (get-class-table) '(:all)))
  (print (fetch-row `(:key ,(get-encoded-string-value class-estr)) (get-class-table))))

;; the list of OK-Tables registered in the database
(dolist (tbl (query 'ok-table '(:all)))
  (print (ok-table-name (deref tbl))))

;; the list of OK-Maps registered in the database
(dolist (map (query 'ok-map '(:all)))
  (print (ok-map-name (deref map))))

;; the list of OK-Sets registered in the database
(dolist (set (query 'ok-set '(:all)))
  (print (ok-set-name (deref set))))

;; the list of OK-Schema registered in the database
(dolist (schema (query 'ok-schema '(:all)))
  (print (ok-schema-name (deref schema))))
|#

;; ---------------------------------------------------------------------

#| ;; Example

(setf *timeout* 1000)

(get-persistent-class 'address-entry)
(query 'address-entry '(and (= okeanos::name "Dave")
                            (= okeanos::location "Tucson")))
(mapcar 'deref
        (query 'address-entry '(and (= okeanos::name "Dave")
                                    (= okeanos::location "Tucson"))))
(mapcar 'deref
        (query 'address-entry '(= okno::name "Dave")))

(mapcar 'deref
        (query 'address-entry '(and (= okeanos::location "Tucson"))))

(setf x (deref (car (query 'address-entry '(and (= okno::name "Dave")
                                                (= okno::location "Tucson"))))))
(setf (slot-value x 'okno::location) "Hollywood")
(commit)
(setf x (deref (car (query 'address-entry '(= okno::name "Dave")))))
(setf (slot-value x 'okno::location) "Tucson")
(commit)





(match x
  ((:A (:B xx)) d1)
  ((:A xx) d2))

(compile-query '(and (= e1 x) (not (= e2 y))))
(compile-query '(and (not (= e1 x)) (= e2 y)))

(query 'address-entry '(and (= okeanos::name "Dave")
                            (NOT (= okeanos::location "Tucson"))))
                     
|#
