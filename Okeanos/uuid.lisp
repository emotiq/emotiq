;; alloc.lisp
;; --------------------------------------------------------------------------------------
;;
;; DM/RAL  08/08
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
;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

;; --------------------------------------------------------------
#|
(defmethod make-uuid ((str string))
  (uuid:make-uuid-from-string str))

(defmethod make-uuid ((kw symbol))
  (uuid:make-uuid-from-string (symbol-name kw)))
|#

;; --------------------------------------------------------------
#|
(defmethod maybe-uuid (key)
  key)

(defmethod maybe-uuid ((key string))
  (let ((kuuid (is-string-uuid key)))
    (or kuuid key)))

(defun is-string-uuid (key)
  (if (and (= (length key) 36)
           (every #'(lambda (ch)
                    (or (char= #\-)
                        (digit-char-p ch 16)))
                  key))
      (ignore-errors (uuid:make-uuid-from-string key))
    ))

(defmethod maybe-uuid ((key symbol))
  (let ((kuuid (is-string-uuid (symbol-name key))))
    (or kuuid key)))
|#
;; --------------------------------------------------------------

(defun make-uuid ()
  (uuid:make-v1-uuid))



