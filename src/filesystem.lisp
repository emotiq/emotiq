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

(defparameter *subpath* nil "Directory subpath to be added to each of the below. Must end with /. Useful for testing.")

(defun ensure-relative-directory (path)
  "Ensure str represents a relative directory path"
  (let ((dir (uiop:ensure-directory-pathname path)))
    ; now force it to be relative
    (make-pathname :directory `(:relative ,@(cdr (pathname-directory dir))))))

(defun subpath ()
  "Returns properly-formed *subpath*, if any"
  (when *subpath*
    ; TODO wrap this in a handler-case eventually because it can error if bad stuff is in *subpath*
    (ensure-relative-directory *subpath*)))

(defun var/log/ ()
  "Absolute cl:pathname of the directory to persist logs and traces of system activity."
  (let ((d (asdf:system-relative-pathname :emotiq "../var/log/")))
    (uiop:if-let (subpath (subpath)) 
      (setf d (merge-pathnames subpath d)))
    (ensure-directories-exist d)
    d))

(defun etc/ ()
  "All configuration files for a node"
  (let ((d (asdf:system-relative-pathname :emotiq "../var/etc/")))
    (uiop:if-let (subpath (subpath))
      (setf d (merge-pathnames subpath d)))
    (ensure-directories-exist d)
    d))

(defun tmp/ ()
  "Writable filesystem for temporary output"
  (let ((d  #p"/var/tmp/emotiq/"))
    (uiop:if-let (subpath (subpath))
      (setf d (merge-pathnames subpath d)))
    (ensure-directories-exist d)))
  
(defun emotiq/user/root/ ()
  #+:windows
  (error "Not implemented on Windows")
  #+:linux
  (let ((root (merge-pathnames ".emotiq/" (user-homedir-pathname))))
    (uiop:if-let (subpath (subpath))
      (setf root (merge-pathnames subpath root)))
    root)
  #+:darwin
  (let ((root (merge-pathnames "Emotiq/"
                                                  (merge-pathnames "Library/Application Support/"
                                                                   (user-homedir-pathname)))))
    (uiop:if-let (subpath (subpath))
      (setf root (merge-pathnames subpath root)))
    root))

(defun emotiq/wallet/ ()
  "The pathname for the directory containing wallets"
  (let ((wallet (merge-pathnames (make-pathname :directory '(:relative "wallet"))
                                 (emotiq/user/root/))))
    (uiop:if-let (subpath (subpath))
      (setf wallet (merge-pathnames subpath wallet)))
    wallet))


