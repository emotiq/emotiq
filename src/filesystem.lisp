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

(defun emotiq/user/root/ ()
  "The root of all user persistence"
  (let ((d (if (not (emotiq:production-p))
               (asdf:system-relative-pathname :emotiq "../var/etc/") ;; TODO rename other than etc
               #+:windows
               (merge-pathnames "Emotiq/" (user-homedir-pathname))
               #+:linux
               (merge-pathnames ".emotiq/" (user-homedir-pathname))
               #+:darwin
               (merge-pathnames "Emotiq/"
                                (merge-pathnames "Library/Application Support/"
                                                 (user-homedir-pathname))))))
    (ensure-directories-exist d)
    d))


(defun var/log/ ()
  "Absolute cl:pathname of the directory to persist logs and traces of system activity."
  (merge-pathnames "var/log/" (emotiq/user/root/)))
    

(defun etc/ ()
  "All configuration files for a node"
  (emotiq/user/root/)) ;;; FIXME: use a read-only directory structure
                       ;;; underneath main user persistence

(defun tmp/ ()
  "Writable filesystem for temporary output"
  (let ((d "#p/var/tmp/emotiq/"))
    (ensure-directories-exist d)
    d))
  

(defun emotiq/wallet/ ()
  "The pathname for the directory containing wallets"
  (let ((wallets-directory (merge-pathnames "wallet/" (emotiq/user/root/))))
    (ensure-directories-exist wallets-directory)
    wallets-directory))


(defun ensure-relative-directory (path)
  "Ensure str represents a relative directory path"
  (let ((dir (uiop:ensure-directory-pathname path)))
    ; now force it to be relative
    (make-pathname :directory `(:relative ,@(cdr (pathname-directory dir))))))


(defun new-temporary-directory (&key (root #p"/var/tmp/emotiq/"))
  (loop
     :with sub-directory = (make-pathname
                            :defaults root
                            :directory (append (pathname-directory root)
                                               (list (symbol-name (gensym)))))
     :until (not (probe-file sub-directory))
     :finally (progn
                (ensure-directories-exist sub-directory)
                (return sub-directory))))
       
       


