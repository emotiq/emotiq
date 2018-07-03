(in-package :gossip/config)

(defparameter *keypairs-filename* "keypairs.conf")
(defparameter *hosts-filename*    "hosts.conf")
(defparameter *machine-filename*  "local-machine.conf")

;;; XXX No need for these to be parameters?
(defparameter *keypair-db-path* nil "Full path to keypairs config file")
(defparameter *hosts-db-path*   nil "Full path to hosts config file")
(defparameter *machine-db-path* nil "Full path to local-machine config file")


(defun initialize (&key (root-path (emotiq/fs:etc/)))
  (when root-path
    (setf *keypair-db-path* (merge-pathnames *keypairs-filename* root-path))
    (setf *hosts-db-path*   (merge-pathnames *hosts-filename*    root-path))
    (setf *machine-db-path* (merge-pathnames *machine-filename*  root-path))))

(defun get-values ()
  "Return the values for the current network configuration.
The first value is the database of all keypairs in the test network.
The second value contains all hosts.
The third value contains the configuration of this node."
  (unless (and *keypair-db-path*
               *hosts-db-path*
               *machine-db-path*)
    (initialize))
  (handler-case
  (let ((keypairs (read-pairs-database *keypair-db-path*))
        (hosts nil)
        (local-machine nil))
    (flet ((badfile (filename)
             (error "~S file not found or invalid" filename)))
      (unless keypairs (badfile *keypairs-filename*))
      (setf hosts (read-pairs-database *hosts-db-path*))
          (unless hosts (gossip::log-event :WARN (format nil "'~A' invalid or empty" *hosts-filename*)))
      (setf local-machine (read-local-machine-configuration *machine-db-path*))
      (unless local-machine (badfile *machine-filename*))
          (values keypairs hosts local-machine)))
    (error (e)
           (emotiq:note "Configuration strategy failed because ~a" e)
           nil)))

(defun read-pairs-database (pathname)
  (when (and pathname (probe-file pathname))
    (let ((pair nil)
          (pairs nil))
      (let ((*read-supress* t))
        (with-open-file (s pathname)
          (setf pairs
                (loop while (setf pair (read s nil nil nil)) collect pair))))
      pairs)))
  
(defun read-local-machine-configuration (&optional (pathname *machine-db-path*))
  (when (probe-file pathname)
    (let ((*read-supress* t))
      (with-open-file (s pathname :direction :input :if-does-not-exist :error)
        (let ((form (read s nil nil nil)))
          form)))))




