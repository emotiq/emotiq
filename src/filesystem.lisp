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

(defun var/log/ ()
  "Absolute cl:pathname of the directory to persist logs and traces of system activity."
  (let ((d (asdf:system-relative-pathname :emotiq "../var/log/")))
    (when *subpath*
      (setf d (merge-pathnames *subpath* d)))
    (ensure-directories-exist d)
    d))

(defun etc/ ()
  "All configuration files for a node"
  (let ((d (asdf:system-relative-pathname :emotiq "../var/etc/")))
    (when *subpath*
      (setf d (merge-pathnames *subpath* d)))
    (ensure-directories-exist d)
    d))

(defun tmp/ ()
  "Writable filesystem for temporary output"
  (let ((d  #p"/var/tmp/emotiq/"))
    (when *subpath*
      (setf d (merge-pathnames *subpath* d)))
    (ensure-directories-exist d)))
  
(defun emotiq/user/root/ ()
  #+:windows
  (error "Not implemented on Windows")
  #+:linux
  (if *subpath*
      (merge-pathnames *subpath* (merge-pathnames ".emotiq/" (user-homedir-pathname)))
      (merge-pathnames ".emotiq/" (user-homedir-pathname)))
  #+:darwin
  (if *subpath*
      (merge-pathnames *subpath* (merge-pathnames "Emotiq/"
                                                  (merge-pathnames "Library/Application Support/"
                                                                   (user-homedir-pathname))))
      (merge-pathnames "Emotiq/"
                       (merge-pathnames "Library/Application Support/"
                                        (user-homedir-pathname)))))

(defun emotiq/wallet/ ()
  "The pathname for the directory containing wallets"
  (if *subpath*
      (merge-pathnames *subpath* (merge-pathnames (make-pathname :directory '(:relative "wallet"))
                                                  (emotiq/user/root/)))
      (merge-pathnames (make-pathname :directory '(:relative "wallet"))
                       (emotiq/user/root/))))


