(in-package :emotiq/config)

;;;; HACK:  for true testnet this has to work without this file
;;;;
;;;; Configuration artifacts for test network as a whole
;;;; Rename/repackage into emotiq/config/network

(defparameter *stakes-filename*
  (make-pathname :name "stakes" :type "conf"))
(defparameter *max-stake*
  (truncate (/ 1E9 1E3))  ;; ratio of total coins to stakers
  "Maximum amount to award a staked entity")

(defun get-stakes (&key (root (emotiq/fs:etc/)))
  (let ((p (merge-pathnames *stakes-filename* root)))
    (when (probe-file p)
      (with-open-file (o p
                         :direction :input)
        (loop :with form
           :while (setf form (core-crypto:read-safely o)) :collecting form)))))

(defparameter *keypairs-filename*
  (make-pathname :name "keypairs" :type "conf"))

(defun read-pairs-database (pathname)
  (unless (and pathname (probe-file pathname))
    (emotiq:note "No pairs database found at '~a'" pathname)
    (return-from read-pairs-database nil))
  (with-open-file (s pathname)
    (loop :for pair = (core-crypto:read-safely s) 
       :until (not pair)
       :collect pair)))

(defun get-keypairs (&key (root (emotiq/fs:etc/)))
  (let ((pathname (merge-pathnames *keypairs-filename* root)))
    (read-pairs-database pathname)))

(defun get-nth-key (n
                    &key (root (emotiq/fs:etc/)))
  "Return the keying triple from the network configuration for Nth id"
  (let ((public-private (nth n (get-keypairs :root root))))
    (values 
     (pbc:make-keying-triple
      (first public-private)
      (second public-private))
     public-private)))

