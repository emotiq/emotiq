;;; monads.lisp
;;; 27-May-2018 SVS

;;; Some monads for use in gossip replies

(in-package :gossip)

(defclass monad ()
  ())

(defclass metadata-mixin ()
  ((metadata :initarg :metadata :initform (kvs:make-store ':alist) :accessor metadata)))

(defclass augmented-data (monad metadata-mixin)
  ((data :initarg :data :initform nil :accessor data))
  (:documentation "Monad for data augmented with metadata."))

(defclass exception (monad metadata-mixin)
  ((name :initarg :name :initform nil :accessor name)
   (exception-condition :initarg :exception-condition :initform nil :accessor exception-condition
              :documentation "error condition if any"))
  (:documentation "Monad for some kind of exception, such as an error."))

(defgeneric unwrap (monad)
  (:documentation "The inverse of unit. If monad contains more than 1 primitive data slot, this should return
    them as multiple values."))

(defmethod unwrap ((monad monad))
  (error "Unwrap must be specified per specific monad type"))

(defmethod unwrap ((ad augmented-data))
  (values (data ad)
          (metadata ad)))

(defmethod unwrap ((ex exception))
  (values (name ex)
          (exception-condition ex)
          (metadata ex)))

(defmethod bind (fn (monad monad))
  "General bind. Reverse arguments from typical bind because this way works better for composition in Lisp.
   Fn should produce another monad, but this is not strictly enforced.
   You can use ad-lift to create such a function."
  (multiple-value-call fn (unwrap monad)))

(defmethod ad-lift (fn)
  "Lift for augmented-data. Returns a function made from fn that returns an augmented-data monad.
   Fn must be a function of 2 arguments (data and metadata) and produce 2 values (new-data and new-metadata)"
  (lambda (data metadata)
    (multiple-value-call 'augment (funcall fn data metadata))))

(defmethod add-metadata ((thing metadata-mixin) key datum)
  "Adds a metadata pair to metadata-containing thing"
  (kvs:relate! (metadata thing) key datum))

(defmethod get-metadata ((thing metadata-mixin) key)
  "Gets metadata value associated with key, if any"
  (kvs:lookup-key (metadata thing) key))

(defun augment (datum &optional (metadata (kvs:make-store ':alist)))
  "This is unit for the augmented-data monad"
  (make-instance 'augmented-data :data datum
    :metadata metadata))

(defun except (&rest args)
  "Unit for the exception monad"
  (apply 'make-instance 'exception args))

;; THE PROBLEM HERE is that we need to specifically NOT coalesce the data if the values of similar keys in the metadata differ.
;; In that case, we should just return a list of both monads. Which means that the function this returns must also
;; be able to accept a list in its first argument. Second argument will always be a singleton augmented-data object.
(defun coalescer (kind)
"Reduction function for a particular message kind.
  Must accept two augmented-data monads and produce a third."
  (let ((dc (data-coalescer kind)))
    (lambda (ad1 ad2)
      (cond ((consp ad1) ; ad1 is a list. So output will always be a list of the same length*, or 1 longer**.
             ;   * if metadata of ad2 matches one of the ad1 metadatas (it cannot possibly match more than one)***
             ;  ** if metadata of ad2 doesn't match any of the ad1 metadatas
             ; *** If it matched more than one, that would mean two of the metadatas in ad1 match each other, and that can never happen.
             (munge-augmented-data ad2 ad1 dc))
            (t ; both are singletons. [ad2 will never be a list because this is being called by reduce]
             (if (metadata-match? (metadata ad1) (metadata ad2))
                 (augment (funcall dc (data ad1) (data ad2)) (metadata ad1))
                 (list ad1 ad2)))))))

; fix a pretty horrible bug in this function in usocket
;  where
;  (USOCKET:IP= "127.0.0.1" #(127 0 0 1)) = T
;  but
;  (USOCKET:IP= #(127 0 0 1) "127.0.0.1") = NIL
(defun usocket::ip= (ip1 ip2)
  (etypecase ip1
    (string (string= ip1 (usocket::host-to-hostname ip2)))
    ((or (vector t 4)
         (array (unsigned-byte 8) (4))
         (vector t 16)
         (array (unsigned-byte 8) (16)))
     (equalp ip1 (usocket::host-to-vector-quad ip2)))
    (integer (= ip1 (usocket::host-byte-order ip2)))))

(defun gossip-equalp (d1 d2)
  "Like equalp but also tests for ip addresses"
  (or (equalp d1 d2)
      (and (integerp d1)
           (integerp d2)
           (usocket:ip= d1 d2))))

(defun metadata-match? (md1 md2)
  "Returns true if the two metadatas match, or nil if not.
   Metadata match if they are both nil, or if they both have identical keys and values,
   but possibly in different orders."
  (null (set-exclusive-or md1 md2 :test 'gossip-equalp)))

(defun munge-augmented-data (ad ad-list dcfun)
  "If ad has metadata that matches that of one of the ads in ad-list, coalesce it using dcfun into that list and return the list.
  If it doesn't match any in the list, return a new list with ad appended to ad-list"
  (cond ((null ad-list)
         (list ad))
        ((and (typep ad 'augmented-data) ; don't attempt to consolidate anything that isn't an augmented-data, like an exception or a primitive datum
              (typep (car ad-list) 'augmented-data)
              (metadata-match? (metadata ad) (metadata (car ad-list))))
         (cons (augment (funcall dcfun (data ad) (data (car ad-list))) (metadata ad))
               (cdr ad-list)))
        (t ; no match
         (cons (car ad-list) (munge-augmented-data ad (cdr ad-list) dcfun)))))

; Check to see if monad rules are followed. We're not messing with metadata here. These tests are too simple for that.
#+MONAD-TEST
(progn

(defun ainc3 (value md) (values (+ 3 value) md))
(defun asqrt (value md) (values (sqrt value) md))

; Left-unit [yeah I know. Unit is really on the right here because I reversed the arguments.]
(bind (ad-lift 'ainc3) (augment 5 '((:foo :bar)))) ; --> augmented-data with data = 8, metadata = '((:foo :bar))

; Right-unit [ditto]
(bind 'augment (augment 5 '((:diploid :bather))))  ; --> New augmented-data with data = 5, metadata = '((:diploid :bather))

; Associative
;;;  (bind g (bind f monad)) ==== (bind (lambda (value)
;;;                                       (bind g (f value)))
;;;                                       monad)

;;; Here, f is (ad-lift 'asqrt) and g is (ad-lift 'ainc3).

(bind (ad-lift 'ainc3) (bind (ad-lift 'asqrt) (augment -3 '((:neg :complex))))) ; --> augmented-data with data = #C(3.0 1.7320508). So asqrt happened first, then ainc3, as expected
(bind (lambda (value metadata)
         (bind (ad-lift 'ainc3) (funcall (ad-lift 'asqrt) value metadata)))
        (augment -3 '((:neg :complex))))                                        ; --> Ditto
)