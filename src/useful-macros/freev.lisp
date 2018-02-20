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


(in-package :freev)

;; NOTE:
;; (hcl:variable-information '*read-eval*) => (values :special nil nil)
;; (sys:declared-special-p '*read-eval*) => T

(defun prognify (form)
  ;; form is always a (PROGN ...) or a (LOCALLY ...)
  (optima:match form
    ((list (or 'PROGN 'LOCALLY) clause)  clause)
    (_                                   form)
    ))

(defstruct binding
  sym gsym class lambda-level)

(defvar *dbg* nil)

(defun rewrite-with-annotation (form env)
  (let ((lambda-level 0))
    (declare (special lambda-level))
    (labels
        ((get-binding-sym (binding)
           (if (consp binding)
               (car binding)
             binding))

         (get-binding-val (binding)
           (when (consp binding)
             (cadr binding)))
         
         (find-free-vars (form env)
           (when *dbg*
             (with-standard-io-syntax
               (pprint `(<- ,form))))
           (labels
               ((lookup (sym)
                  (find sym env :key #'binding-sym))

                
                (create-binding (sym class)
                  ;;
                  ;; Binding Classes:
                  ;;   :GLOBAL  (non-constant, global, and always special)
                  ;;   :CONST   (constant special global, but used like a lexical reference)
                  ;;   :LEXICAL (lambda args referenced in the body
                  ;;             not including nested lambda's, and local bindings)
                  ;;   :SPECIAL (local special)
                  ;;   :LAMBDA-GLOBAL (lambda arg shadowing a global special)
                  ;;   :FREE    (refs to unknown symbols)
                  ;;   :ALPHALEX (alpha conversion required on every unshadowed use)
                  ;;
                  ;; Class :LAMBDA-BINDING is a probing key during
                  ;; arglist absorption, not a stored symbol binding
                  ;; class.
                  ;;
                  (let* ((is-global (and (not (constantp sym))
                                         (sys:declared-special-p sym)))
                         (new-class (cond ((eq :LAMBDA-BINDING class)
                                           (if is-global
                                               :LAMBDA-GLOBAL
                                             :LEXICAL))
                                          ((eq :FLET class) :LEXICAL) ;; overrides :GLOBAL
                                          (is-global       :GLOBAL)
                                          ((constantp sym) :CONST)
                                          (t               class) ;; :LEXICAL, :SPECIAL, :FREE, :ALPHACONV
                                          )))
                    (make-binding
                     :sym  sym
                     :gsym (if (eq new-class :LEXICAL)
                               sym
                             (gensym (string sym)))
                     :class new-class
                     :lambda-level lambda-level)))
                
                (add-binding (sym class)
                  (push (create-binding sym class) env))

                (process-fn ()
                  (let ((lambda-level (1+ lambda-level)))
                    (declare (special lambda-level))
                    (destructuring-bind (fnsym args &rest body) form
                      (dolist (arg args)
                        (let ((sym (get-binding-sym arg)))
                          (unless (member sym lambda-list-keywords)
                            (add-binding sym :LAMBDA-BINDING)
                            )))
                      (multiple-value-bind (new-body new-free)
                          (find-free-vars `(LOCALLY ,@body) env)
                        (values `(,fnsym ,args ,@(cdr new-body)) new-free))
                      )))
                
                (process-lambda ()
                  ;; (format t "~&Processing LAMBDA")
                  (destructuring-bind (lambdasym args &rest body) form
                    (declare (ignore lambdasym))
                    (multiple-value-bind (new-body new-free)
                        (find-free-vars `(FN ,args ,@body) env)
                      (cond (new-free
                             (let ((capture-bindings (mapcar (lambda (binding)
                                                               (list (binding-gsym binding)
                                                                     (binding-sym  binding)))
                                                             new-free)))
                               (values `(LET ,capture-bindings
                                          (LAMBDA ,args
                                            ,@(cddr new-body)))
                                       nil)))
                            
                            (t
                             (values
                              `(LAMBDA ,args
                                 ,@(cddr new-body))
                              nil))
                            ))))

                (process-function ()
                  ;; (format t "~&Processing FUNCTION")
                  (multiple-value-bind (new-form new-free)
                      (find-free-vars (cadr form) env)
                    (if (and (consp new-form)
                             (not (eql 'LAMBDA (car new-form))))
                        (values new-form new-free)
                      (values `(FUNCTION ,new-form) new-free))))
                
                (process-macrolet ()
                  ;; (format t "~&Processing MACROLET")
                  ;;
                  ;; already performed from Walker:Walk-Form, just
                  ;; strip out to avoid spurious warnings in compile
                  ;;
                  (destructuring-bind (sym bindings &rest body) form
                    (declare (ignore sym bindings))
                    (find-free-vars (prognify `(LOCALLY ,@body)) env)))
                
                (process-declare ()
                  ;; (format t "~&Processing DECLARE")
                  ;; handle (DECLARE SPECIAL)
                  (let (free)
                    (dolist (clause (cdr form))
                      (when (eql (car clause) 'SPECIAL)
                        (dolist (sym (cdr clause))
                          (let ((binding (lookup sym)))
                            (when (or (null binding)
                                      (eq :LEXICAL (binding-class binding)))
                              (let ((new-binding (create-binding sym :SPECIAL)))
                                (push new-binding free)
                                (push new-binding env))
                              )))
                        ))
                    (values form free)))

                (process-let ()
                  ;; (format t "~&Processing LET")
                  (destructuring-bind (letsym bindings &rest body) form
                    (let (free
                          new-bindings)
                      (case letsym
                        (LET
                            (let (new-env)
                              (dolist (binding bindings)
                                (let* ((sym  (get-binding-sym binding))
                                       (env-binding (lookup sym)))
                                  (when (or (null env-binding)
                                            (eq :SPECIAL (binding-class env-binding)))
                                    (push (create-binding sym :LEXICAL) new-env))))
                              (dolist (binding bindings)
                                (let* ((sym  (get-binding-sym binding))
                                       (val  (get-binding-val binding)))
                                  (multiple-value-bind (new-val new-free)
                                      (find-free-vars val env)
                                    (setf free (append new-free free)
                                          env  (append new-free env))
                                    (push `(,sym ,new-val) new-bindings))))
                              (setf env (append new-env env))))

                        (LET*
                            (dolist (binding bindings)
                              (let* ((sym  (get-binding-sym binding))
                                     (val  (get-binding-val binding))
                                     (env-binding (lookup sym)))
                                (multiple-value-bind (new-val new-free)
                                    (find-free-vars val env)
                                  (setf free (append new-free free)
                                        env  (append new-free env))
                                  (push `(,sym ,new-val) new-bindings)
                                  (when (or (null env-binding)
                                            (eq :SPECIAL (binding-class env-binding)))
                                    (push (create-binding sym :LEXICAL) env)) )))) )
                      (multiple-value-bind (new-body new-free)
                          (find-free-vars `(LOCALLY ,@body) env)
                        (values `(,letsym ,(nreverse new-bindings) ,@(cdr new-body))
                                new-free)))))
                
                (process-llet ()
                  ;; (format t "~&Processing LLET")
                  (destructuring-bind (letsym bindings &rest body) form
                    (let (new-bindings)
                      (dolist (binding bindings)
                        (let* ((sym    (get-binding-sym binding))
                               (bndstr (or (lookup sym)
                                           (create-binding sym :LEXICAL))))
                          (case (binding-class bndstr)
                            (:LEXICAL
                             (push binding new-bindings))
                            (:SPECIAL
                             (push binding new-bindings)
                             (push (create-binding sym :LEXICAL) env))
                            ((:GLOBAL :LAMBDA-GLOBAL)
                             (let ((new-bndstr (create-binding sym :ALPHALEX)))
                               (if (cdr binding)
                                   (push `(,(binding-gsym new-bndstr) ,(cadr binding)) new-bindings)
                                 ;; else
                                 (push (binding-gsym new-bndstr) new-bindings))
                               (push new-bndstr env)))
                            (t (error "Invalid LET binding")) ;; const
                            )))
                      (find-free-vars `(,(if (eql letsym 'LLET) 'LET 'LET*)
                                        ,(nreverse new-bindings) ,@body) env)
                      )))

                (process-quote ()
                  ;; (format t "~&Processing QUOTE")
                  (values form nil))

                (process-symbol ()
                  ;; (format t "~&Processing SYMBOL: ~A" form)
                  (let* ((binding (or (lookup form)
                                      (create-binding form :FREE)))
                         (class (binding-class binding)))
                    (cond ((or (eq class :CONST)
                               (eq class :LEXICAL)
                               (and (eq class :LAMBDA-GLOBAL)
                                    (= lambda-level (binding-lambda-level binding))))
                           (values (binding-sym binding) nil))
                          ((eq class :ALPHALEX)
                           (values (binding-gsym binding) nil))
                          ((or (eq class :LAMBDA-GLOBAL) ;; seen from nested lambda
                               (eq class :FREE))
                           (values (binding-gsym binding) (list binding)))
                          ((= 1 lambda-level) ;; top level lambda :GLOBAL or :SPECIAL or :FREE
                           (values (binding-gsym binding) (list binding)))
                          (t ;; nested lambda :GLOBAL or :SPECIAL or :FREE ref
                             (values (binding-gsym binding) nil))
                          )))

                (process-flet ()
                  (destructuring-bind (fletsym bindings &rest body) form
                    (let* (free
                          (new-env (mapcar (lambda (sym)
                                             (create-binding sym :FLET))
                                           (mapcar #'car bindings)))
                          new-bindings)
                      (case fletsym
                        (LABELS
                          (dolist (binding bindings)
                            (destructuring-bind (sym args &rest fnbody) binding
                              (multiple-value-bind (new-fnbody new-free)
                                  (find-free-vars `(FN ,args ,@fnbody)
                                                  (setf env (append new-env env)))
                                (setf free (append new-free free)
                                      env  (append new-free env))
                                (push `(,sym ,args ,@(cddr new-fnbody)) new-bindings)))))

                        (FLET
                            (dolist (binding bindings)
                              (destructuring-bind (sym args fnbody) binding
                                (multiple-value-bind (new-fnbody new-free)
                                    (find-free-vars `(FN ,args ,@fnbody) env)
                                  (setf free (append new-free free)
                                        env  (append new-free env))
                                  (push `(,sym ,args ,@(cddr new-fnbody)) new-bindings))))
                          (setf env (append new-env env)) ) )
                      
                      (multiple-value-bind (new-body new-free)
                          (find-free-vars `(PROGN ,@body) env)
                        (let ((capture-bindings (mapcar (lambda (binding)
                                                          (list (binding-gsym binding)
                                                                (binding-sym  binding)))
                                                        (append new-free free))))
                          (if capture-bindings
                              (values `(LET ,capture-bindings
                                         (,fletsym ,(nreverse new-bindings) ,@(cdr new-body)))
                                      nil)
                            ;; else
                            (values `(,fletsym ,(nreverse new-bindings) ,@(cdr new-body))
                                    nil))
                          )))))

                (process-progn ()
                  (let (free
                        new-clauses)
                    (destructuring-bind (progsym &rest clauses) form
                      (dolist (clause clauses)
                        (multiple-value-bind (new-clause new-free)
                            (find-free-vars clause env)
                          (push new-clause new-clauses)
                          (setf env (append new-free env)
                                free (append new-free free))))
                      (values `(,progsym ,(nreverse new-clauses)) free))
                    ))
                
                (process-other-consp ()
                  (cond ((and (consp (car form))
                              (eql 'LAMBDA (caar form)))
                         (find-free-vars `(FUNCALL ,@form) env))

                        #|
                        ((consp (car form))
                         ;; (format t "~%Processing clauses")
                         (let (free
                               new-clauses)
                           (dolist (clause form)
                             (multiple-value-bind (new-clause new-free)
                                 (find-free-vars clause env)
                               (push new-clause new-clauses)
                               (when new-free
                                 (setf free (append new-free free)
                                       env  (append new-free env)))
                               ))
                           (values (nreverse new-clauses)
                                   free)))
                        |#
                        
                        (t
                         (destructuring-bind (fn &rest rest) form
                           ;; (format t "~&Processing cons: ~A" fn)
                           (let (free
                                 new-args)
                             (dolist (arg rest)
                               (multiple-value-bind (new-arg new-free)
                                   (find-free-vars arg env)
                                 (push new-arg new-args)
                                 (when new-free
                                   (setf free (append new-free free)
                                         env  (append new-free env)))
                                 ))
                             (values `(,fn ,@(nreverse new-args))
                                     free))))
                        )) )
             (multiple-value-bind (new-form free)
                 (cond ((consp form)
                        (case (car form)
                          (LAMBDA        (process-lambda))
                          (FN            (process-fn))
                          ((SYMBOL-MACROLET MACROLET) (process-macrolet))
                          ((PROGN LOCALLY PROG1 PROG2) (process-progn))
                          ((LET LET*)    (process-let))
                          (DECLARE       (process-declare))
                          ((LABELS FLET) (process-flet))
                          (QUOTE         (process-quote))
                          (FUNCTION      (process-function))
                          ((LLET LLET*)  (process-llet))
                          (otherwise     (process-other-consp))))

                       ((symbolp form)  (process-symbol))

                       (t
                        ;; (format t "~&Processing atom: ~A" form)
                        (values form nil)) )
               (when *dbg*
                 (with-standard-io-syntax
                   (pprint `(-> ,new-form))))
               (values new-form free)))) )
      (find-free-vars (walker:walk-form form env) nil)
      )))

(defvar *in-lex* nil)

(defmacro with-lexical-closures (&body body &environment env)
  (let ((form (prognify `(PROGN ,@body))))
    (if *in-lex*
        form
      ;; else
      (let ((*in-lex* t))
        (rewrite-with-annotation form env)))
    ))

(defmacro lexical-1 (args &rest body)
  (destructuring-bind (ghd &rest gtl) args
    `(symbol-macrolet ((,ghd ,ghd))
       ,@(if gtl
             `((lexical-1 ,gtl ,@body))
           body))
    ))

(defmacro lexical (args &rest body)
  (if args
      `(lexical-1 ,args ,@body)
    (prognify `(progn ,@body))))

(editor:setup-indent "lexical" 1)


#|
(defvar x 15)
(defvar diddly 32)
(unintern 'x)

(with-lexical-closures
  (let ((ref 15))
    (labels ((gf ()
               (+ ref x)))
      (funcall-async #'gf))))

(with-lexical-closures
 (let ((u 1)
       (x 32)
       (y 15))
   (declare (special y))
   (locally ;; let ((y 77))
     (let ((v 2)
            (x "twently"))
           (symbol-macrolet ((c 3))
             (lambda (xx a b)
               (+ x xx y a b c)
               (lambda (z)
                 (+ x y z)))
             )))))

(setf *dbg* t)

(let ((form '(let ((u 1)
                  (x 32)
                  (y 15))
                (declare (special y))
                (locally ;; let ((y 77))
                  (llet ((v 2)
                        (x "twently"))
                    (symbol-macrolet ((c 3))
                      (lambda (xx a b)
                        (+ x xx y a b c)
                        (lambda (z)
                          (+ x y z)))))))
            ))
  (multiple-value-bind (new-form free)
      (rewrite-with-annotation form nil)
    (with-standard-io-syntax
      (pprint new-form))))

(let ((form '(let ((u 1)
                  (x 32)
                  (y 15))
                (declare (special y))
                (let ((y 77))
                  (let ((v 2)
                        (x "twently"))
                    (symbol-macrolet ((c 3))
                      (lambda (x a b)
                        (+ x y a b c)
                        (lambda (z)
                          (+ x y z)))))))
            ))
  (multiple-value-bind (new-form free)
      (rewrite-with-annotation form nil)
    (with-standard-io-syntax
      (pprint new-form))))

(inspect (compile nil (lambda (#:x #:y #:z)
                        (+ #:x #:y #:z))))

(symbol-macrolet ((xx 32))
  (lambda (xx) (print xx)))

(lexical (a b c)
         (let ((a 1)
               (b 2)
               (c 3))
           (+ a b c)))
|#
