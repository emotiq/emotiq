;; utilities.lisp -- a collection of useful routines
;;
;; DM/Emotiq  01/18
;; -----------------------------------------------------------------
#|
The MIT License

Copyright (c) 2018 Emotiq AG

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


(in-package :ads)

;; ---------------------------------------------------------------

(deftype vector-ub8 ()
  `(simple-array (unsigned-byte 8) (*)))

;; ---------------------------------------------------------------

(defmacro def-cached-var (name creator &optional cache-name)
  (let ((cname (or cache-name (intern (format nil "*~A*" (string name))))))
    `(progn
       (defvar ,cname nil)
       (defun ,name ()
         (or ,cname
             (setf ,cname ,creator)))) ))

#+:LISPWORKS
(editor:setup-indent "def-cached-var" 1)

;; -----------------------------------------------------------------------------
;; PRNG for system

(def-cached-var ctr-hash-prng
  #-:OS-WINDOWS (ironclad:make-prng :fortuna :seed :urandom)
  #+:OS-WINDOWS (lw:make-mt-random-state t))

(um:defmonitor
    ;; protected by a global lock
    ;; ctr-hash-prng is a shared mutable state
    ((rand (limit)
       #-:OS-WINDOWS (ironclad:strong-random limit (ctr-hash-prng))
       #+:OS-WINDOWS (lw:mt-random limit (ctr-hash-prng)))))

(defun rand-between (lower upper)
  (declare (integer lower upper))
  ;; generate random (lower <= n < upper)
  (+ lower (rand (- upper lower))))

;; --------------------------------------------------
;; Low level helper functions

(defvar *nibbles-to-chars* "0123456789abcdef")

(defun encode-bytes-to-string (bytes)
  (with-output-to-string (s)
    (map nil (lambda (b)
               (labels ((princ-nibble (bit-pos)
                          (princ (char *nibbles-to-chars*
                                       (ldb (byte 4 bit-pos) b))
                                 s)))
                 (princ-nibble 4)
                 (princ-nibble 0)))
         bytes)))

(defun decode-string-to-bytes (str)
  (labels ((nibble-val (c)
             (cond ((char<= #\0 c #\9)
                    (- (char-code c) #.(char-code #\0)))
                   ((char<= #\a c #\f)
                    (+ 10 (- (char-code c) #.(char-code #\a))))
                   ((char<= #\A c #\F)
                    (+ 10 (- (char-code c) #.(char-code #\A))))
                   (t
                    (error "Invalid hex-string ~A" str))
                   )))
    (let (tmp)
      (prog1
          (coerce
           (um:accum acc
             (map nil (lambda (c)
                        (cond (tmp
                               (acc (logior (ash tmp 4)
                                            (nibble-val c)))
                               (setf tmp nil))
                              
                              (t
                               (setf tmp (nibble-val c)))
                              ))
                  str))
           'vector-ub8)
        (assert (null tmp)))) ;; check that we had an even number of nibble chars
    ))

;; -----------------------------------------------------------------------------

(defun invalid-arg (x)
  (error "Invalid argument type ~A" x))

(defun singleton? (lst)
  ;; is this a list with only one item?
  (null (cdr lst)))

;; -----------------------------------------------------------------------------

(defclass queue ()
  ((hd  :accessor queue-hd)
   (tl  :accessor queue-tl)))

(defmethod initialize-instance :after ((q queue) &key initial-contents &allow-other-keys)
  (let ((lst (cons nil (copy-list initial-contents))))
    (setf (queue-hd q) lst
          (queue-tl q) (last lst))))

(defmethod queue-add ((q queue) item)
  ;; add item to tail of queue
  (setf (queue-tl q)
        (setf (cdr (queue-tl q)) (list item))))

(defmethod queue-empty-p ((q queue))
  (eq (queue-hd q) (queue-tl q)))

(defmethod queue-pop ((q queue))
  ;; pop first item from queue
  (when (queue-empty-p q)
    (error "Empty queue"))
  (pop (queue-hd q))
  (shiftf (car (queue-hd q)) nil)) ;; help out GC

(defmethod queue-contents ((q queue))
  ;; return the list of queue contents, emptying the queue
  (setf (queue-tl q) (queue-hd q))
  (shiftf (cdr (queue-hd q)) nil))

;; -----------------------------------------------------------------------------

(defmacro if-let ((arg form) t-form &optional f-form)
  `(let ((,arg ,form))
     (if ,arg
         ,t-form
       ,f-form)))

#+:LISPWORKS
(editor:setup-indent "if-let" 1)

(defmacro when-let ((arg form) &body body)
  `(let ((,arg ,form))
     (when ,arg
       ,@body)))

#+:LISPWORKS
(editor:setup-indent "when-let" 1)

;; ------------------------------------------------------------

(defmethod compare ((k1 real) (k2 real))
  (- k1 k2))

(defmethod compare ((k1 character) (k2 character))
  (compare (char-code k1) (char-code k2)))

(defmethod compare ((k1 symbol) (k2 symbol))
  (compare (symbol-name k1) (symbol-name k2)))

(defmethod compare ((k1 vector) (k2 vector))
  (let ((len1  (length k1))
        (len2  (length k2)))
    (labels ((iter (ix)
               (if (and (< ix len1)
                        (< ix len2))
                   (let ((cmp (compare (aref k1 ix) (aref k2 ix))))
                     (if (zerop cmp)
                         (iter (1+ ix))
                       cmp))
                 ;; else
                 (- len1 len2))))
      (iter 0))))

