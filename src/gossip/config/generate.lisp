(in-package :gossip/config)

(defparameter *nominal-gossip-port* 65002 "Duplicate of same symbol in gossip package
    to avoid package interdependencies")

#+(or)
(defun generate-network (records &key (root (emotiq/fs:etc/)))
  "Generate configuration directories for gossip network for RECORDS at directory ROOT

Returns a list of directions created with valid configurations.

  The ROOT defaults to the value returned by 'EMOTIQ/FS:ETC/'."
  (let* ((key-records (generate-keys records))
         (pubkeys (loop :for record :in key-records :collect (car (fourth record))))
         directories)
    (dolist (key-record key-records)
      (destructuring-bind (host eripa port (public private))
          key-record
        (declare (ignore private))
        (let ((path (make-pathname :directory (append (pathname-directory root)
                                                      (list (format nil "~a:~a" host gossip-port)))
                                   :defaults root)))
          (push 
           (generate-node host eripa gossip-port public path)
           directories))))
    directories))

(defun generate-node (&key root host eripa gossip-port public private key-records)
  (declare (ignore host)) ;; Hmm?
  (ensure-directories-exist root)
  (emotiq:note "~&Writing configuration to '~a'.~&" root)

  (write-local-machine-conf
   (merge-pathnames *machine-filename* root)
   eripa gossip-port public)
  (write-hosts-conf
   (merge-pathnames *hosts-filename* root)
   key-records)
  (write-keypairs-conf
   (merge-pathnames *keypairs-filename* root)
   key-records)
  root)

(defun write-local-machine-conf (path eripa port public)
  (with-open-file (o path
                     :direction :output
                     :if-exists :supersede)
    (format o "~s~&" `(:eripa ,eripa
                       :gossip-port ,port
                       :pubkeys (,public)))))

(defun write-hosts-conf (path records)
  (with-open-file (o path
                     :direction :output
                     :if-exists :supersede)
    (dolist (record records)
      (format o "~s~&" `(,(getf record :hostname)
                         ,(getf record :gossip-server-port))))))

(defun write-keypairs-conf (path records)
  (with-open-file (o path
                     :direction :output
                     :if-exists :supersede)
    (format o ";;; ~A~%" *keypairs-filename*)
    (format o ";;; THIS FILE IS FOR TESTING ONLY~%")
    (dolist (record records)
      (format o "~s~&" `(,(getf record :public)
                         ,(getf record :private))))))




       
