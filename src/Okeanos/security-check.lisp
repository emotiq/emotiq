
(in-package :user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (has-super-user-dongle-p)
    (unless (has-okno-api-license-p)
      (error "Need API License for OKNO")
      )))

#|
(defun delete-okno-packages ()
  (dolist (pkg (topo-sort
                (um:keep-if 'find-package
                            '(;; okno
                              com.sd.okeanos
                              ))
                #'(lambda (p1 p2)
                  (member (find-package p1) (package-use-list p2)))
                ))
    ;; (delete-package pkg)
    (rename-package pkg (uuid-string))
    ))
|#
