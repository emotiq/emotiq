;; collector.lisp
;; --------------------------------------------------------------
;; Define our own collector objects that perform rapid nconc list
;; accumulation
;;
;; DM/RAL 02/07 -- added a Lock for MP safety
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

(in-package #:um.collector)
   
(defclass <collector> ()
  ((hd   :accessor collector-hd :initform nil)
   (tl   :accessor collector-tl :initform nil)))

(defmethod internal-mark-changed (obj &optional tf)
  (declare (ignore obj tf))
  nil)

(defmethod do-with-locked-instance (obj whostate timeout (fn function))
  (declare (ignore obj whostate timeout))
  (funcall fn))

(defmacro with-locked-instance ((obj &optional whostate timeout) &body body)
  `(do-with-locked-instance ,obj ,whostate ,timeout (lambda () ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-locked-instance" 1)

(defun internal-reset (c tf)
  (with-locked-instance (c)
    (setf (collector-hd c) nil
          (collector-tl c) nil)
    (internal-mark-changed c tf)))

(defmethod collector-reset ((c <collector>))
  (internal-reset c nil))

(defmethod collector-discard-contents ((c <collector>))
  (internal-reset c t))
  
(defmethod collector-nstuff-contents ((c <collector>) (lst list))
  (with-locked-instance (c)
    (setf (collector-tl c) (last lst)
          (collector-hd c) lst)
    (internal-mark-changed c)))

(defmethod collector-stuff-contents ((c <collector>) (lst list))
  (collector-nstuff-contents c (copy-list lst)))

(defmethod collector-ncontents ((c <collector>) &key (discard t))
  ;; make readout destructive to the collector object
  ;; so that we can't accidentally hand off a list that
  ;; is still undergoing active mutation
  ;;
  ;; If user doesn't want to discard contents,
  ;; then hand him a copy of the contents as they exist at this moment.
  (with-locked-instance (c)
    (if discard
        (prog1
            (collector-hd c)
          (collector-discard-contents c))
      ;; else
      (copy-list (collector-hd c)))))

(defmethod collector-length ((c <collector>))
  (length (collector-hd c)))

(defmethod collector-empty-p ((c <collector>))
  (null (collector-hd c)))

(defmethod collector-nappend ((c <collector>) (lst list))
  (with-locked-instance (c)
    (let ((penult (nconc (collector-tl c) lst)))
      (setf (collector-tl c) (last lst))
      (unless (collector-hd c)
        (setf (collector-hd c) penult))
      (internal-mark-changed c))))

(defmethod collector-append ((c <collector>) (lst list))
  (collector-nappend c (copy-list lst)))

(defmethod collector-append1 ((c <collector>) item)
  (collector-nappend c (list item)))

(defmethod collector-nprepend ((c <collector>) (lst list))
  (with-locked-instance (c)
    (setf (collector-hd c) (nconc lst (collector-hd c)))
    (unless (collector-tl c)
      (setf (collector-tl c) (last lst)))
    (internal-mark-changed c)))

(defmethod collector-prepend ((c <collector>) (lst list))
  (collector-nprepend c (copy-list lst)))

(defmethod collector-push ((c <collector>) item)
  (with-locked-instance (c)
    (push item (collector-hd c))
    (unless (collector-tl c)
      (setf (collector-tl c) (collector-hd c)))
    (internal-mark-changed c)))

(defmethod collector-pop ((c <collector>))
  (with-locked-instance (c)
    (prog1
        (pop (collector-hd c))
      (unless (collector-hd c)
        (setf (collector-tl c) nil))
      (internal-mark-changed c))))

(defun make-collector ()
  (make-instance '<collector>))

;; -------------------------------------------------

(defclass <monitored-object-mixin> ()
  ((changed  :accessor monitored-object-mixin-changed :initform nil)))

(defmethod internal-mark-changed ((obj <monitored-object-mixin>) &optional (t/f t))
  (setf (monitored-object-mixin-changed obj) t/f))

(defmethod mark-changed ((obj <monitored-object-mixin>) &optional (t/f t))
  (with-locked-instance (obj)
   (internal-mark-changed obj t/f)))

(defmethod nchanged-p ((obj <monitored-object-mixin>))
  ;; asking if changed also resets the monitor
  (with-locked-instance (obj)
   (shiftf (monitored-object-mixin-changed obj) nil)))

;; ---------------------------------------------------
(defclass <monitored-collector> (<monitored-object-mixin>
                                 <collector>)
  ())

(defun make-monitored-collector ()
  (make-instance '<monitored-collector>))


;; ----------------------------------------------------------
(defclass <mpsafe-mixin> ()
  ((lock :accessor mpsafe-lock
         :initform (mpcompat:make-lock :name "MPSafe Mixin Lock"))))

(defmethod do-with-locked-instance ((obj <mpsafe-mixin>) whostate timeout (fn function))
  (mp:with-lock ((mpsafe-lock obj) whostate timeout)
    (funcall fn)))

;; ----------------------------------------------------------
(defclass <mpsafe-collector> (<mpsafe-mixin>
                              <collector>)
  ())

(defclass <mpsafe-monitored-collector> (<mpsafe-mixin>
                                        <monitored-object-mixin>
                                        <collector>)
  ())

(defun make-mpsafe-collector ()
  (make-instance '<mpsafe-collector>))

(defun make-mpsafe-monitored-collector ()
  (make-instance '<mpsafe-monitored-collector>))

;; ------------------------------------------------
