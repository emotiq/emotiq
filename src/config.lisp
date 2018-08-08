(in-package :emotiq/config)

(defparameter *conf-filename*
  (make-pathname :name "emotiq-conf"
                 :type "json"))
(defparameter *hosts-filename*
  (make-pathname :name "hosts"
                 :type "conf"))
(defparameter *local-machine-filename*
  (make-pathname :name "local-machine"
                 :type "conf"))
(defparameter *genesis-block-filename*
  (make-pathname :name "emotiq-genesis-block"
                 :type "json"))

(defun settings (&key (root (emotiq/fs:etc/)))
  (let ((p (merge-pathnames *conf-filename* root)))
    (unless (probe-file p)
      (emotiq:note "No configuration able to be read from '~a'" p)
      (return-from settings nil))
    (cl-json:decode-json-from-source p)))

(defun setting (key &key (root (emotiq/fs:etc/)))
  (let ((c (settings :root root)))
    (alexandria:assoc-value c key)))

(defun gossip-get-values (&key (root (emotiq/fs:etc/)))
  "Return the values for the current network configuration.

  The first value is the database of all keypairs in the test network.

  The second value contains all hosts.

  The third value contains the gossip local machine configuration of this node."
  (handler-case
      (let ((keypairs (get-keypairs :root root))
            (hosts (read-pairs-database
                    (merge-pathnames *hosts-filename* root)))
            (gossip (read-gossip-configuration
                     (merge-pathnames *local-machine-filename* root))))
        (values
         keypairs hosts gossip))
    (error (e)
      (emotiq:note "Configuration strategy failed because ~a" e)
      nil)))

(defun read-gossip-configuration (pathname)
  (when (probe-file pathname)
    (with-open-file (s pathname :direction :input :if-does-not-exist :error)
      (let ((form (core-crypto:read-safely s)))
        form))))


(defun local-machine (&key (root (emotiq/fs:etc/)))
  (let ((form (with-open-file (o (merge-pathnames *local-machine-filename* root))
                (core-crypto:read-safely o))))
    (destructuring-bind (&key eripa
                              (gossip-port
                               (emotiq/config:setting :gossip-server-port))
                              pubkeys)
        form
      (values `(:eripa ,eripa :gossip-port ,gossip-port :pubkeys ,pubkeys)))))


  
  

