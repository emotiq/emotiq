;;; clozure.lisp
;;; 20-Apr-2018 SVS
;;; Patches for CCL

;;; Allow definitions to contain #S(structure...) inlines in CCL.
#+CLOZURE
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmethod make-load-form ((s structure-object) &optional env)
    (make-load-form-saving-slots s :environment env)))
