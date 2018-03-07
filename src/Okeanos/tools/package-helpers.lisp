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

(defun find-bound-symbols-for-package (package)
  (sort (let ((syms nil)
              (package (find-package package)))
          (do-symbols (sym package)
            (when (and (eq (symbol-package sym) package)
                       (or (fboundp sym)
                           (boundp sym)))
              (push sym syms)))
          syms)
        #'string<))
#|
(let ((*print-length* nil))
  (pprint
   (find-bound-symbols-for-package :okeanos)))
|#
(defun collect-undefined-symbols-for-package (package)
  (let ((package (find-package package))
        (syms nil))
    (do-symbols (sym package)
      (when (and (eq (symbol-package sym) package)
                 (not (fboundp sym))
                 (not (boundp sym)))
        (push sym syms)))
    (sort syms #'string<)))
#|
(pprint (collect-undefined-symbols-for-package :okeanos.remote-access))
|#

(defun all-bound-symbols (parent-package)
  (let* ((parent-package (find-package parent-package))
         (child-packages (org.tfeb.hax.hierarchical-packages::package-children
                          parent-package)))
    (sort
     (loop for child-package in (cons parent-package child-packages) nconc
           (find-bound-symbols-for-package child-package))
     #'string<)))

#|
(setf *bound-symbols* (all-bound-symbols :okeanos))
|#

(defun find-definitions-for-symbol (symbol parent-package &optional bound-symbols)
  (let* ((parent-package (find-package parent-package))
         (bound-symbols  (or bound-symbols (all-bound-symbols parent-package)))
         (bindings       (remove-if (complement (um:curry #'string= symbol)) bound-symbols)))
    (sort bindings #'string< :key (um:compose #'package-name #'symbol-package))))

#|
(find-definitions-for-symbol 'basic-slot-value :okeanos)
|#

(defun find-bindings-for-undefined-symbols (package)
  (let* ((package (find-package package))
         (undefs  (collect-undefined-symbols-for-package package))
         (parent  (org.tfeb.hax.hierarchical-packages::package-parent package))
         (bound   (all-bound-symbols parent))
         (imports (make-hash-table)))
    (loop for undef in undefs
          for defs = (find-definitions-for-symbol undef parent bound)
          do
          (loop for def in defs
                for pkg = (symbol-package def)
                do
                (setf (gethash pkg imports)
                      (cons def (gethash pkg imports))) ))
    (let ((imps nil))
      (maphash (lambda (pkg syms)
                 (push (cons (package-name pkg)
                             (sort syms #'string<))
                       imps))
               imports)
      (apply #'concatenate 'string 
             (mapcar (lambda (lst)
                       (format nil "~%(:import-from #:~A~{~&#:~A~})"
                               (car lst) (cdr lst)))
                     (sort imps #'string< :key #'car))))))

#|
(pprint (org.tfeb.hax.hierarchical-packages::package-children :okeanos))
(let ((*print-length* nil))
  (pprint
   (find-bindings-for-undefined-symbols :okeanos.test)))
|#
