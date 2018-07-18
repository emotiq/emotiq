;; lib-loads.lisp -- Load C libraries for PBC and Edwards Curves
;;
;; DM/Emotiq 07/18
;; ---------------------------------------------------------
#|
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
;; --------------------------------------------------------------------
;; The libs are defined and loaded here because CFFI expects them to be
;; loaded and defined prior to macro-expansion of certain CFFI:DEFCFUN's.
;;
;; Our system is no longer in any danger by having these libs loaded
;; along the way. Previously there were some MP issues that required a
;; careful load and initialization order. That is no longer the case.

(in-package :cl-user)

;; -----------------------------------------------------------------------

;; The Lisp runtime load might help us do this differently, but explicit
;; initialization is much easier to understand

(defun define-dev-dlls ()
  "loads the DLLs (.so and .dylib) at runtime, from pre-specified directories"
  (pushnew (asdf:system-relative-pathname :emotiq "../var/local/lib/")
           cffi:*foreign-library-directories* :test 'equal)
  (cffi:define-foreign-library
      :libEd3363 
    (:darwin     "libLispEd3363.dylib")
    (:linux      "libLispEd3363.so")
    (t (:default "libLispEd3363")))
  (cffi:define-foreign-library
      :libCurve1174
    (:darwin     "libLispCurve1174.dylib")
    (:linux      "libLispCurve1174.so")
    (t (:default "libLispCurve1174")))
  (cffi:define-foreign-library
      :libpbc 
    (:darwin     "libLispPBCIntf.dylib")
    (:linux      "libLispPBCIntf.so")
    (t (:default "libLispPBCIntf"))))

(defun define-production-dlls ()
  "loads the DLLs (.so and .dylib) at runtime, from the current directory"
  (cffi:define-foreign-library
      :libEd3363
   (:darwin     "libLispEd3363.dylib")
   (:linux      "libLispEd3363.so")
   (t (:default "libLispEd3363")))
  (cffi:define-foreign-library
      :libCurve1174
   (:darwin     "libLispCurve1174.dylib")
   (:linux      "libLispCurve1174.so")
   (t (:default "libLispCurve1174")))
  (cffi:define-foreign-library
      :libpbc
   (:darwin     "libLispPBCIntf.dylib")
   (:linux      "libLispPBCIntf.so")
   (t (:default "libLispPBCIntf"))))

(defun load-dlls()
  "load the dev or production dlls at runtime"
  (if (emotiq:production-p)
      (define-production-dlls)
    (define-dev-dlls))
  (cffi:use-foreign-library :libpbc)
  (cffi:use-foreign-library :libEd3363)
  (cffi:use-foreign-library :libCurve1174))

(load-dlls)
