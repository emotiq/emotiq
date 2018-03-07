;; alloc.lisp
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

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



