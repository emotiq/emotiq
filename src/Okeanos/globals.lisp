;; lock-daemon.lisp -- Read / Write Locking
;; --------------------------------------------------------------------------------------
;;
;; DM/RAL  07/09
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

(defvar *default-db-directory* (translate-logical-pathname "PROJECTS:LISP;OkeanosDB;"))

(defvar *db-files*           nil) ;; list of connected database database structs

(defvar *shutting-down*      nil) ;; only true during Lisp shutdown advice

(defvar *ts-offset*            0) ;; offset to bring TS up to file last TS

(defvar *timeout*             10) ;; timeout used for remote access

(defvar *dereference-slots*    t) ;; set to nil when marshaling references
(defvar *retrieving*         nil) ;; true when retrieving an object from DB

(defvar *current-okeanos-db* nil) ;; database instance when database is open/connected
(defvar *current-connection* nil) ;; cnx-info when in transaction

(defvar *current-commit-logfile* nil)

;; -------------------------------------------

(defgeneric persist (obj))
(defgeneric ref (obj))
(defgeneric deref (obj))
(defgeneric same-ref (a b))
(defgeneric valid-ref? (obj))

