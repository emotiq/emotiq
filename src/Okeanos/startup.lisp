
(in-package :user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (has-super-user-dongle-p)
    ;; (delete-bfly-packages)
    (unuse-package :com.sd.okeanos.int :com.sd.okeanos.user)
    (delete-package :com.sd.okeanos.int)
    (do-external-symbols (sym :com.sd.okeanos.user)
      (import sym :com.sd.okeanos.user))))

