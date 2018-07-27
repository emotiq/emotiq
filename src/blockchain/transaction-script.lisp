(in-package :emotiq/transaction/script)


(defvar *scripts* '()
  "An a-list of the form
    ((name . (arglist . body)) ...)")

(defun get-lock-script (name)
  "If found, returns a list of the form (NAME FUNCTION-NAME (ARGS...))"
  (assoc name *scripts*))

(defun make-lock-script (name)
  (let ((new (list name (list '()))))
    (push new *scripts*)
    new))

(defun get-or-make-lock-script (name)
  (or (get-lock-script name)
      (make-lock-script name)))

(defun get-unlock-script (name)
  (get-lock-script name))

;; For now, lock and unlock scripts are the same below this level, but keep
;; the distinction higher up for documentation, and maybe enforce some day.


(defun define-script (name arglist body)
  (let* ((entry (get-or-make-lock-script name))
         (arglist-and-body (cdr entry)))
    (setf (first arglist-and-body) (copy-tree arglist))
    (setf (cdr arglist-and-body) (copy-tree body))))

(defmacro def-script (name arglist &body body)
  `(progn
     (define-script ',name ',arglist ',body)
     ',name))

(defmacro def-lock-script (name arglist &body body)
  `(def-script ,name ,arglist ,@body))

(defmacro def-unlock-script (name arglist &body body)
  `(def-script ,name ,arglist ,@body))


(defvar *script-ops* '()
  "An a-list of the form
     ((name op-dispatch-function) ...)")

(defun get-script-op (name)
  "If found, returns a list of the form (NAME OP-FUNCTION-NAME)"
  (assoc name *script-ops*))

(defvar *script-vars* '()
  "An a-list of the form
     ((name var-value-fetch-function) ...)")

(defmacro def-script-var (name fetch-function-name)
  `(progn
     (push '(,name ,fetch-function-name)
           *script-vars*)
     ',name))


;;; A basic Lispy evaluator:
;;;
;;;   EVAL (expr X, environment N)
;;;     if X is a literal, return its literal value;
;;;     if X is a symbol, return (lookup-symbol X N);
;;;     ...
;;;     Finally: X is a list (fn . args): (APPLY fn args)
;;;
;;;   APPLY (function F, arguments Args)
;;;     if F is built in, return built-in interpretation of (F . Args)
;;;     let N = (look-up-bindings F)
;;;     let B = (look-up-body F)
;;;     return (eval B N)

;;; EVAL: evaluate EXPR, in the Lispy script language, with variable BINDINGS
;;; in effect to determine the value of free variables.  BINDINGS is an a-list
;;; of the form ((variable-symbol . value) ...).
;;;
;;; A few examples
;;; (eval '(list 1 2 3 foo 5) '((foo . 4))) => (1 2 3 4 5)
;;; (eval '(and (= (* a b) c) (or x y)) '((a . 1) (b . 2) (c . 2) (x . nil) (y . 13))) => 13

(defun eval (expr bindings)
  (cond
    ((atom expr)
     (case expr
       ((t) t)
       ((nil) nil)
       (otherwise
        (cond
          ((symbolp expr)
           (let ((entry? (assoc expr bindings)))
             (if entry?
                 (cdr entry?)
                 (error 'unbound-symbol expr))))
          ((or (numberp expr) (stringp expr))
           expr)
          (t
           (error 'strange-atom expr))))))
    ((atom (cdr expr))
     (error 'illegal-dotted-list expr))
    (t
     (case (first expr)
       ((quote) (if (cdr (rest expr))
                    (error 'too-many-args-to-quote expr)
                    (second expr)))
       ((not) (let ((rest (rest expr)))
                (if (not (and rest (consp rest)))
                    (error 'bad-args-to-not (rest expr))
                    (not (eval (first rest) bindings)))))
       ((and) (loop with v
                    for e in (rest expr)
                    while (setq v (eval e bindings))
                    finally (return v)))
       ((or) (loop for e in (rest expr) thereis (eval e bindings)))
       ((if) (let ((last (last expr)) length)
               (unless (atom (cdr last))
                 (error 'bad-if-form-non-list expr))
               (setq length (length expr))
               (unless (member length '(3 4))
                 (error 'bad-if-form-weird-length length expr))
               (if (eval (second expr) bindings)
                   (eval (third expr) bindings)
                   (and (eql length 4) (eval (fourth expr) bindings)))))
       ((+ - * = /= > >= < <=)
        (cl:apply (first expr) 
                  (loop for e in (rest expr) 
                     as v = (eval e bindings)
                     unless (numberp v)
                     do (error 'bad-non-numeric-arg-to-numeric-operator
                               (first expr) v expr)
                     collect (eval v bindings))))
       ((/) (let* ((args (rest expr))
                   (length (length args)))
              (if (> length 2)
                  (error 'n-arg-divide-not-allowed length expr)
                  (let* ((v-1 (eval (first args) bindings))
                         (v-2 (eval (second args) bindings)))
                    (unless (and (numberp v-1) (numberp v-2))
                      (error 'bad-non-numeric-arg-to-numeric-operator
                             (first expr) 
                             (if (numberp v-1) v-2 v-1)
                             expr))
                    (if (= v-2 0)
                        (error 'divide-by-zero expr)
                        (/ v-1 v-2))))))
       ((list) (loop for arg in (rest expr)
                     as v = (eval arg bindings)
                     collect v))
       ;; otherwise, assume function to apply
       (otherwise
        (let ((function-name (first expr))
              (args (loop for arg in (rest expr)
                       collect (eval arg bindings))))
          (apply function-name args bindings)))))))

(defun apply (function-name args bindings)
  (let ((script? (get-lock-script function-name)))
    (if script?
        (destructuring-bind (name vars body)
            script?
          (declare (ignore name))
          (loop with new-bindings = bindings
                for var in vars
                as arg in args
                do (setq new-bindings (acons var arg new-bindings))
                finally (return (eval body new-bindings))))
        (let ((script-op? (get-script-op function-name)))
          (if script-op?
              (destructuring-bind (name op-fn) 
                  script-op?
                (declare (ignore name))
                (cl:apply op-fn args))
              (error 'undefined-function-or-script-op
                     function-name))))))

(defun error (type &rest additional-args)
  (emotiq:em-warn "Error in EMOTIQ/TXN/SCRIPT:EVAL of type ~a: additional info: ~s"
                  type additional-args)
  (fail-script))
    

;;; Example spend transaction:
;;;
;;;   First, there's a UTXO like so to Alice's account:
;;;
;;;   transaction
;;;     transaction id: abc123....
;;;     [transaction output]
;;;       tx-out-public-key-hash: def456...
;;;       tx-out-amount: 50
;;;       tx-out-lock-script:
;;;         (public-key signature)
;;;         (and (public-key-equal-verify public-key ^tx-public-key-hash)
;;;              (check-signature public-key signature))
;;;     [transaction input]
;;;       ...
;;;
;;;  Then, there's a transaction that spends the UTXO:
;;;
;;;   transaction
;;;     transaction id: bcd234....
;;;     [transaction output]
;;;       ...
;;;     [transaction input]
;;;       tx-in-id: abc123....
;;;       tx-in-index: 0
;;;       tx-in-unlock-script:
;;;         (^tx-public-key ^tx-signature)

;; These need better logic for set up; 2nd args ignored!!
(def-script-var ^tx-public-key-hash ignore)
(def-script-var ^tx-public-key ignore)
(def-script-var ^tx-signature ignore)
(def-script-var ^current-epoch ignore)
(def-script-var ^initial-epoch-of-stake ignore)
(def-script-var ^n-epochs-until-unstake ignore)


(defvar *transaction-script-catch-tag* (list nil)
  "An arbitrary unique value used as the tag to throw out of a
   transaction script.")

(defvar *in-transaction-script-context* nil)

(defvar *current-script-op* nil)

(defun require-transaction-script-context ()
  (unless *in-transaction-script-context*
    (error "Must execute in a Transaction-Script context, can't continue.")))

(defun require-transaction-script-op-context ()
  (when (null *current-script-op*)
    (error "Must execute in a Transaction-Script-OP context, can't continue.")))
    

(defmacro def-script-op (name arglist &body body)
  (let* ((op-dispatch-function-name (intern (format nil "OP-~a" NAME)))
         (entry (list name op-dispatch-function-name)))
    `(progn
       (defun ,op-dispatch-function-name ,arglist
         (require-transaction-script-context)
         (let ((*current-script-op* ',name))
           ,@body))
       (unless (equal (get-script-op ',name) ',entry)
         (push ',entry *script-ops*))
       ',name)))


(defmacro in-transaction-script-context (&body body)
  `(let ((*in-transaction-script-context* t))
     (catch *transaction-script-catch-tag*
       (if (progn ,@body)
           t
           nil))))

(defvar *current-transaction* nil)

(defmacro in-transaction ((transaction) &body body)
  `(let* ((*current-transaction* ,transaction))
     ,@body))

(defun fail-script (&rest format)
  (cond
    (*current-transaction*
     (require-transaction-script-context)
     (if format
         (cl:apply #'emotiq:em-warn format)
         (emotiq:em-warn "Failure in script."))
     (throw *transaction-script-catch-tag* nil))
    (t
     (cerror "Continue regardless (presumably in a REPL)"
             "Not in a Transaction-Script context, as required."))))

  
(defparameter *debugging-chainlisp* nil
  "When true, fail-script-op signals a continuable error on a failed
  script operation instead of calling WARN.")


(defun fail-script-op ()
  (cond
    (*current-script-op*
     (require-transaction-script-op-context)
     (if *debugging-chainlisp*
         (cerror "Continue" "Failure in script op ~a" *current-script-op*)
         (emotiq:em-warn "Failure in script op ~a" *current-script-op*))
     (throw *transaction-script-catch-tag* nil))
    (t
     ;; Allow going off into the wild blue yonder, since this makes it
     ;; very convenient to run in a REPL and simply get back to top
     ;; level.  If deep down in the stack, it may not work out.
     (cerror "Continue regardless (presumably in a REPL)"
             "Not in a Transaction-Script-OP context, as required."))))





;;; MAP-TRANSACTION-TYPE-TO-UNLOCK-SCRIPT: map transaction-type to its
;;; corresponding Chainlisp function name.
;;;
;;; The currently supported types :spend, :spend-cloaked, and :collect all
;;; supply the same function: SCRIPT-SIG, which supplies the public key hash
;;; (a/k/a address) and signature for verification, needed to unlock the value
;;; of the transaction at hand.

(defun get-unlock-script-for-type (transaction-type)
  (get-unlock-script 
   (ecase transaction-type
     ((:spend :spend-cloaked :collect :coinbase)
      'script-sig))))

(defun get-lock-script-for-type (transaction-type)
  (get-lock-script 
   (ecase transaction-type
     ((:spend :collect :coinbase) 'script-pub-key)
     (:spend-cloaked 'script-pub-key-cloaked))))

;; ---*** note: per David we must ultimately make the :coinbase
;; ---*** transaction cloaked. Review later!  -mhd, 6/26/18



(defvar *failed-runs* nil)

(defun run (transaction transaction-input utxo)
  (in-transaction (transaction)
    (in-transaction-script-context
      (let* ((lock-script (txn:output-lock-script utxo))
             (unlock-script (txn:input-unlock-script transaction-input))
             (address (txn:output-address utxo)))
        (when (null lock-script)
          
          (push (list transaction transaction-input utxo) *failed-runs*)
          
          (error 'no-lock-script))
        (when (null unlock-script)
          
          (push (list transaction transaction-input utxo) *failed-runs*)
          
          (error 'no-unlock-script))
        (let* ((unlock-script-function-name (first unlock-script))
               (unlock-args
                (apply unlock-script-function-name
                       '()
                       `((^signature . ,(txn::%input-signature transaction-input))
                         (^public-key . ,(txn::%input-public-key transaction-input)))))
               (lock-script-function-name (first lock-script)))
          (apply lock-script-function-name
                 unlock-args
                 
                 ;; Establish and handle implicit/global
                 ;; parameters by putting them in the bindings
                 ;; list, and then referring to them as from
                 ;; script as script vars. Kludge this for now --
                 ;; automate soon.
                 
                 `((^tx-public-key-hash . ,address))))))))





(def-script-op public-key-equal-verify (public-key public-key-hash)
  (cond
    ((not (typep public-key 'pbc:public-key))
     (emotiq:em-warn "Arg public-key (~s) is not of correct type: ~s"
             public-key 'pbc:public-key)
     (fail-script-op))
    ((not (stringp public-key-hash))
     (emotiq:em-warn "Public-key-hash ~s not a string but string expected"
             public-key-hash)
     (fail-script-op))
    (t
     (let* ((public-key-hash-from-public-key (wallet:address public-key))
            (l1 (length public-key-hash))
            (l2 (length public-key-hash-from-public-key)))
       (cond
         ((not (stringp public-key-hash-from-public-key))
          (emotiq:em-warn "Public-key hash ~s derived from public-key ~s not a string but string expected"
                  public-key-hash-from-public-key public-key)
          (fail-script-op))
         ((not (= l1 l2))
          (emotiq:em-warn "Public key hash ~s length ~s not same as public key ~s hash ~s length ~s"
                  public-key-hash l2
                  public-key public-key-hash-from-public-key l2)
          (fail-script-op))
         ((wallet:address=
           public-key-hash
           public-key-hash-from-public-key)
          t)
         (t
          (fail-script-op)))))))

(def-script-op check-signature (public-key signature)
  (pbc:check-hash (txn:hash *current-transaction*) signature public-key))


(def-lock-script script-pub-key (public-key signature)
  (and (public-key-equal-verify public-key ^tx-public-key-hash)
       (check-signature public-key signature)))

(def-lock-script script-pub-key-cloaked (public-key signature proof message)
  (and (public-key-equal-verify public-key ^tx-public-key-hash)
       (check-signature public-key signature)
       ;; proof: a "bulletproof" (cloaked amount) giving the amount of tokens to
       ;; transfer

       ;; message: a message encrypted with recipient's public key containing
       ;; the amount and gamma
       ))

;;; foo asdf aslk


;;;
;;;

(def-unlock-script script-sig ()
  (list ^public-key ^signature))




