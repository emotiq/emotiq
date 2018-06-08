(in-package :emotiq/filesystem)

;;;; Single source unit defining abstractions for absolute filesystem
;;;; references.
;;;;
;;;; We need to hand out different values for filesystem location for
;;;; development versus runtime.  By centralizing such references in a
;;;; single source unit, we gain measure of control over locations.
;;;;
;;;; TODO: for textual resources that need to be present at production
;;;; time, we have to somehow "walk" their reference to copy them for
;;;; a "resource bundle" that can then be distributed.

(defun var/log/ ()
  "Absolute cl:pathname of the directory to persist logs and traces of system activity."
  (let ((d (asdf:system-relative-pathname :emotiq "../var/log/")))
    (ensure-directories-exist d)
    d))

(defun etc/ ()
  (let ((d (asdf:system-relative-pathname :emotiq "../var/etc/")))
    (ensure-directories-exist d)
    d))




