
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
