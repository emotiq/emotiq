;; queries.lisp -- Object ID Sets and OID files
;; --------------------------------------------------------------------------------------
;; OkeanosMM -- memory mapped database system
;;
;; DM/RAL  03/09
;; --------------------------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2008 Refined Audiometrics Laboratory, LLC

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
(NOT A) = U - A
(NOT (NOT A)) = A
If (MEMBER A B) then (MEMBER (NOT B) (NOT A))

(SET- A B) = (AND A (NOT B))
(SYM-SET- A B) = (OR (AND A (NOT B)) (AND (NOT A) B))
               = (AND (OR A B) (NOT (AND A B)))
               = (SET- (OR A B) (AND A B))

(NOT (SET- A B)) = (OR (NOT A) B)
(NOT (SYM-SET- A B)) = U - A - B + A*B

... the moment we perform a NOT, we involve a search through the
entire database. Seems like we should strive for reductions, where
possible, that elide the outermost NOT.  And since NOT(x) = U - x we
are looking to avoid an outermost subtraction from U.

Conjuctive Normal Form (CNF) = (AND (OR A B) (OR C D) ...)
Disjunctive Normal Form (DNF) = (OR (AND A B) (AND C D) ...)

We desire the fewest NOT terms, and seek transformations which reduce
the count of NOT operations, even if the resulting formula appears
more complex.

OTOH, it may well be that any term A involves searching the entire
database looking for items which satisfy A. In that case, nothing is
gained by eliding NOT.

We need a cost heuristic. (NOT A) may be larger than A, in which case
eliding NOT could spare memory cost. But the driving factor will be
I/O transactions.

If we have a formula with a bunch of factors (sets) A, B, C, ... etc,
then the server could locally cache these sets, obtaining them in
parallel with one scan through the database (huge I/O cost). Then the
server would reduce the formula over these cached sets to produce the
result set that will be transmitted (I/O again) to the client.

These local sets are the leafs referred to below. They form the
elemental accesses to the database. If there is only one product in
the query, the initial read could peform the entire conjunction at
once during its disk I/O traversal - obviating the need to cache
individual sets.

It would seem, therefore, that DNF is to be preferred, hoping for only
one conjunctive term, but caching sets if there are more conjunctions
to follow.

... but that reasoning is flawed. If only one outer term exists then
it won't matter to the single DB scan whether we have a conjuctive
acceptance criteria, or a disjunctive one.

And, in any event, if we have the server perform all the selection
logic, then it doesn't matter whether or not we reduce the formula to
simpler terms. The server can build up a decider function to be used
on every record in its scan and call that on every record to decide on
inclusion in the result set.  The only thing saved by formula
reduction at the client end is a little bit of netword trafic involved
in sending across the query to be performed at the server.

The only thing gained by formula reduction is being able to spot
nullary inquiries and skipping the query altogether to return a NULL
result.

<query> := <src>
        |  (:SELECT <src> <expr>)
        |  (LET* ((<name> <selection>)*) <expr>)

<src >  := {:ALL | (:ALL)}
        |  (= <slot-name> <value>)
        |  (:FROM  <slot-name> <from-value>)
        |  (:TO    <slot-name> <to-value>)
        |  (:BELOW <slot-name> <below-value>)
        |  (:ABOVE <slot-name> <above-value>)
        |  (:RANGE <slot-name> <from-value> [<to-value>])
        |  (:MEMBER <slot-name> <member-values-list>)

<selection> := {<src> | <name>}
            |  (:SELECT {<src>|<name>} <expr>)

<expr>  := (= <slot-name> <value>)
        |  (:RANGE <slot-name> <from-value> [<to-value>])
        |  (:MEMBER <slot-name> <member-values-list>)
        |  ({AND | :AND | *} <expr>*)
        |  ({OR | :OR | +} <expr>*)
        |  (:XOR <expr>*)
        |  ({:SET- | -} <expr>*)
        |  (:SYM-SET- <expr*>)
        |  ({NOT | :NOT | -} <expr>)
        |  (LET* ((<name> <selection>)*) <expr>)
        |  <selection>
        |  epsilon -- i.e., nothing at all

The :SELECT operator establishes a Universe set (:ALL) for the
subsequent expressions. This is used by the (:NOT <expr>) expression
to become (:SET- :ALL <expr>).

|#
;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

;; -----------------------------------------------
;; Operations: LET, AND, OR, NOT, :SET-, :SYM-SET-, :XOR
;; Factors: :ALL or (:ALL)                - the Universe set
;;          (= slot-name value)           - specific items
;;          (:RANGE slot-name from-value) - collections of items
;;          (:RANGE slot-name from-value to-value)
;;          (:MEMBER slot-name member-list)
;;
;; Factors represent sets of data, operators perform
;; operations on these sets.
;; ------------------------------------------------
#|
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
                          (optima:match e
                            ((list '_USET- _ ex) (push ex (cadr acc)))
                            (_                   (push e (car  acc))))
                          acc)
                      (list nil nil) expr)))
    (values (sort (car terms)  'term-compare<)
            (sort (cadr terms) 'term-compare<))))

(defun trim-expr (es)
  (reorder-terms
   (mapcar 'identify-leaves es)))

(defun valid-variable (e)
  (or (member e *query-local-bindings*)
      (gensymp e)))

(defun scan-set (expr &optional (tree (rb-tree:empty)))
  (flet ((find-or-add (item tree)
           (multiple-value-bind (found elt) (rb-tree:mem item tree)
             (if found
                 (values elt tree)
               (values item (rb-tree:add item tree)))
             ))
         (idempotent (op)
           (and (atom op)
                (member op '(and or union intersection + *))))
         (commutes (op)
           (and (atom op)
                (member op '(and or xor union intersection + *)))))
    
    (cond ((atom expr)
           (find-or-add expr tree))
          
          ((consp expr)
           (um:nlet-tail iter ((lst   expr)
                               (tree  tree)
                               (accum nil))
             (if (endp lst)
                 (let* ((ans (nreverse accum))
                        (op  (car expr)))
                   (when (idempotent op)
                     (setf (cdr ans) (delete-duplicates (cdr ans))))
                   (when (commutes op)
                     (setf ans (cons (car ans)
                                     (sort (cdr ans) (lambda (a b)
                                                       (minusp (ord:compare a b))))
                                     )))
                   (find-or-add ans tree))
               ;; else
               (let* ((hd (car lst))
                      (tl (cdr lst)))
                 (multiple-value-bind (hd tree) (scan-set hd tree)
                   (iter tl tree (cons hd accum)))))
             ))
          
          (t (error "What? : ~S" expr))
          )))

(defun identify-leaves (expr)
  ;; find the leaf terms of the query expression, and rewrite
  ;; the overall expression to simpler terms.

  ;; In particular, we bubble up any negatives to top level

  (optima:match (scan-set expr)
    
    (NIL NIL)  ;; we are finished...

    ;; ------------------------------------------------
    ;; Handle the factors
    
    ;; allow use of parenthesized or un-parenthesized :ALL
    ((or :ALL
         (list :ALL))          (enter-leaf '_USET))
    ((list '= . _)             (enter-leaf expr))
    ((list :MEMBER slot-name member-list)
     (identify-leaves `(_OR ,@(mapcar #'(lambda (item)
                                        `(= ,slot-name ,item))
                                      member-list))))
    ((list :RANGE . _)         (enter-leaf expr))

    ;; --------------------------------------------------
    ;; Handle the operators
    
    ((list 'OR . es)
     (multiple-value-bind (pos neg) (trim-expr es)
       (if neg
           (let ((n-and (identify-leaves `(_AND ,@neg))))
             (if pos
                 (let ((p-or (identify-leaves `(_OR ,@pos))))
                   (identify-leaves `(NOT (_SET- ,n-and ,p-or))))
               (identify-leaves `(NOT ,n-and))))
         (identify-leaves `(_OR ,@pos)))))

    ((list 'AND . es)
     (multiple-value-bind (pos neg) (trim-expr es)
       (if neg
           (let ((n-or (identify-leaves `(_OR ,@neg))))
             (if pos
                 (let ((p-and (identify-leaves `(_AND ,@pos))))
                   (identify-leaves `(_SET- ,p-and ,n-or)))
               (identify-leaves `(NOT ,n-or))))
         (identify-leaves `(_AND ,@pos)))))

    ((list :XOR . es)
     (multiple-value-bind (pos neg) (trim-expr es)
       (let ((xor-set (identify-leaves `(_XOR ,@pos ,@neg))))
         (if (evenp (length neg))
             xor-set
           (identify-leaves `(NOT ,xor-set))))))

    ((list :SET- e1 . es)
     (let ((e1x (identify-leaves e1)))
       (optima:match e1x
         ((list '_USET- . _)
          (identify-leaves `(NOT (OR ,e1 ,@es))))
         (_
          (multiple-value-bind (pos neg) (trim-expr es)
            (let ((p-set- (identify-leaves `(_SET- ,e1x ,@pos))))
              (if neg ;; we already know we have at least one positive (the first)
                  (let ((n-and (identify-leaves `(_AND ,@neg))))
                    (identify-leaves `(_AND ,p-set- ,n-and)))
                p-set-)))) )))
         
    ((list 'NOT (list 'NOT e))      (identify-leaves e))
    ((list 'NOT e)                  (identify-leaves `(_USET- ,(enter-leaf '_uset) ,(identify-leaves e))))

    ((list :OR . es)           (identify-leaves `(OR ,@es)))
    ((list :AND . es)          (identify-leaves `(AND ,@es)))
    ((list :NOT . es)          (identify-leaves `(NOT ,@es)))
    ((list :NOR . es)          (identify-leaves `(NOT (OR ,@es))))
    ((list :NAND . es)         (identify-leaves `(NOT (AND ,@es))))
    ((list :NXOR . es)         (identify-leaves `(NOT (:XOR ,@es))))
    ((list :NSET- . es)        (identify-leaves `(NOT (:SET- ,@es))))

    ((list '_USET- e (eql e) . _)  nil)
    ((list '_USET- e1 e2 (eql e2) . es)  (identify-leaves `(_USET- ,e1 ,e2 ,@es)))
    ((list '_USET- _ (list '_USET- _ e)) (identify-leaves e))
    ((list '_USET- USET)            USET)
    ((list '_USET- . _)             expr)

    ;; ------------------------------------------------------------
    ;; by the time we reach the following clauses, all the terms are positive
    ;; and all have been scanned for leaf terms
    ;; ------------------------------------------------------------

    ((list '_OR)               nil)
    ((list '_OR e1)            e1)
    ((list '_OR (list '_SET- e1 e2) (list '_SET- (eql e2) (eql e1)) . es)
       (identify-leaves `(_OR (_XOR ,e1 ,e2) ,@es)))
    ((list '_OR e1 e2 . es)    (identify-leaves `(_OR (_OR ,e1 ,e2) ,@es)))
    ((list '_OR _ _)           expr)

    ((list '_AND)              nil)
    ((list '_AND e1)           e1)
    ((list '_AND e1 e2 . es)   (identify-leaves `(_AND (_AND ,e1 ,e2) ,@es)))
    ((list '_AND _ _)          expr)

    ((list '_XOR e1)           e1)
    ((list '_XOR)              nil)
    ((list '_XOR e (eql e) . es)     (identify-leaves `(_XOR ,@es)))
    ((list '_XOR e1 e2 . es)   (identify-leaves `(_XOR (_XOR ,e1 ,e2) ,@es)))
    ((list '_XOR _ _)          expr)

    ((list '_SET-)             nil)
    ((list '_SET- e1)          e1)
    ((list '_SET- e (eql e) . _)      nil)
    ((list '_SET- e1 e2 (eql e2) . es) (identify-leaves `(_SET- ,e1 ,e2 ,@es)))
    ((list '_SET- (list '_OR e1 e2) (list '_AND (eql e1) (eql e2)) . es)
     (identify-leaves `(_SET- (_XOR ,e1 ,e2) ,@es)))
    ((list '_SET- e1 e2 . es)  (identify-leaves `(_SET- (_SET- ,e1 ,e2) ,@es)))
    ((list '_SET- _ _)         expr)

    ((list 'LET bindings . body)
     (let ((*query-local-bindings* *query-local-bindings*))
       `(LET ,(mapcar #'(lambda (binding)
                        (destructuring-bind (name val) binding
                          (push name *query-local-bindings*)
                          (list name (identify-leaves val))))
                      bindings)
          ,(when-let (clause (car (last body)))
              (identify-leaves clause)) )))

    ((satisfies valid-variable) expr)
    
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
    (nlet-tail iter ((table    *query-leaf-table*)
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
(compile-query '(let ((daves (= name "Dave")))
                  (:SET- daves (OR (= location "Tucson")
                                   (= location "Boston")))))
(compile-query '(and
                 (not (= name "Dave"))
                 (not (= location "Tucson"))
                 (not (= name "Dave"))
                 (not (= name "dave"))))
(compile-query '(and
                 (not (= location "Tucson"))
                 (not (= name "Dave"))
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
      (optima:match e
        
        ((list '_OR e1 e2)
         (let ((v1 (parse e1))
               (V2 (parse e2)))
           (union v1 v2
                  :test test)))
        
        ((list '_AND e1 e2)
         (let ((v1 (parse e1))
               (v2 (parse e2)))
           (intersection v1 v2
                         :test test)))

         ((list '_XOR e1 e2)
          (let ((v1 (parse e1))
                (v2 (parse e2)))
            (set-exclusive-or v1 v2
                              :test test)))
         
         ((list '_SET- e1 e2)
          (let ((v1 (parse e1))
                (v2 (parse e2)))
            (set-difference v1 v2
                            :test test)))
         
         ;; ---------
         
        ((list '_USET- e1 e2)
         ;; complement = NOT, e1 is U
         (let ((v1 (parse e1))
               (v2 (parse e2)))
           (set-difference v1 v2
                           :test test)))
        
         ;; ---------
         
         ((list '= slot-name value)
          (funcall select-eq-fn slot-name value))
         
         ((list ':range slot-name from)
          (funcall select-range-fn slot-name from nil))
         
         ((list ':range slot-name from limit)
          (funcall select-range-fn slot-name from limit))

         ('_USET
          (funcall universe-fn))
         
         ;; ---------
         
         ((list 'LET bindings . body)
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
|#

;; ---------------------------------------------------------------------
(defclass <query-binder> ()
  ((all-fn        :accessor query-all-fn        :initarg :all-fn)
   (slot-eq-fn    :accessor query-slot-eq-fn    :initarg :slot-eq-fn)
   (slot-range-fn :accessor query-slot-range-fn :initarg :slot-range-fn)
   ))

(defun query-engine (selection filter-fn fn-bindings)
  (with-accessors ((all-fn        query-all-fn)
                   (slot-eq-fn    query-slot-eq-fn)
                   (slot-range-fn query-slot-range-fn)) fn-bindings
    (let ((cfilter-fn (and filter-fn
                           (compile nil filter-fn)))
          (universe
           (optima:ematch selection
             ((or :all
                  (list :all))
              (funcall all-fn))

             ((list slot-name '= val)
              (funcall slot-eq-fn slot-name val))

             ((list slot-name :from from-val)
              (funcall slot-range-fn slot-name from-val nil))

             ((list slot-name :to to-val)
              (funcall slot-range-fn slot-name nil to-val))

             ((list slot-name :from from-val :to to-val)
              (funcall slot-range-fn slot-name from-val to-val))
             )))
      (if cfilter-fn
          (um:filter cfilter-fn universe)
        universe))))

;; ---------------------------------------------------------------------
;; Persistent-Class Instances...

(defmethod run-query ((class-name symbol) selection filter-fn)
  (query-engine selection filter-fn
                (make-instance '<query-binder>
                               :all-fn        (deferred (find-instances* class-name))
                               :slot-eq-fn    (curry #'fetch-instances-for-slot class-name)
                               :slot-range-fn (curry #'find-instances-for-slot class-name))
                ))

(defmethod query ((class-name symbol) selection &optional filter-fn)
  (perform 'run-query class-name selection filter-fn))

;; ---------------------------------------------------------------------
;; File-Tables...

(defmethod run-query ((table file-table) selection filter-fn)
  (query-engine selection filter-fn
                (make-instance '<query-binder>
                               :all-fn        (deferred (fetch-all-rows table))
                               :slot-eq-fn    (curry 'fetch-rows-for-column table)
                               :slot-range-fn (curry 'find-rows-for-column table))
                ))

(defmethod query ((table file-table) selection &optional filter-fn)
  (run-query table selection filter-fn))

;; ---------------------------------------------------------------------
;; File-BTrees...

(defmethod run-query ((btree file-btree) selection filter-fn)
  (let ((keyfn (btree:key-fn btree)))
    (query-engine selection filter-fn
                  (make-instance '<query-binder>
                                 :all-fn         (deferred
                                                     (um:accum acc
                                                       (btree:map-tree btree
                                                                       (compose #'acc keyfn))))
                                 :slot-eq-fn     (curry 'btree:find-item btree)
                                 :slot-range-fn  #'(lambda (slot-ignored from to)
                                                     (declare (ignore slot-ignored))
                                                     (um:accum acc
                                                       (btree:map-tree btree (compose #'acc keyfn)
                                                                       :from from :to to)))
                                 ))))

(defmethod query ((btree file-btree) selection &optional filter-fn)
  (run-query btree selection filter-fn))

;; -----------------------------
;; List of system queries
#|

;; list of users that have read the database
(dolist (user-id (query (user-mappings) :all))
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





(optima:match x
  ((list :A (list :B xx)) d1)
  ((list :A xx) d2))

(compile-query '(and (= e1 x) (not (= e2 y))))
(compile-query '(and (not (= e1 x)) (= e2 y)))

(query 'address-entry '(and (= okeanos::name "Dave")
                            (NOT (= okeanos::location "Tucson"))))
                     
|#
