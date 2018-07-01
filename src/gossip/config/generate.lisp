(in-package :gossip/config)

(defparameter *nominal-gossip-port* 65002 "Duplicate of same symbol in gossip package
    to avoid package interdependencies")

#+OBSOLETE
(defparameter *aws-example* 
  '((:hostname "emq-01.aws.emotiq.ch" :ip "34.239.111.18" :port 65002)
    (:hostname "emq-02.aws.emotiq.ch" :ip "52.54.224.84"  :port 65002)
    (:hostname "emq-03.aws.emotiq.ch" :ip "34.238.39.58"  :port 65002)))

(defparameter *aws-example* 
  '((:hostname "zt-emq-01.zt.emotiq.ch" :ip "10.178.2.166")
    (:hostname "zt-emq-02.zt.emotiq.ch" :ip "10.178.0.71" )
    (:hostname "zt-emq-03.zt.emotiq.ch" :ip "10.178.15.71")))

(defparameter *max-stake* (truncate 1E9))

(defun generate (records &key (root #p"/var/tmp/conf/") (max-stake *max-stake*))
  "Generate configuration directories of an Emotiq testnet for RECORDS at directory ROOT
The ROOT defaults to '/var/tmp/conf'."
  (let* ((key-records (generate-keys records))
         (pubkeys (loop :for record :in key-records :collect (car (fourth record))))
         (stakes (generate-stakes pubkeys max-stake))
         directories)
    (dolist (key-record key-records)
      (destructuring-bind (host eripa port (public private))
          key-record
        (declare (ignore private))
        (let ((directory (make-pathname :directory (append (pathname-directory root)
                                                           (list (format nil "~a:~a" host port)))
                                        :defaults root)))
          (emotiq:note "~&Writing configuration to '~a'.~&" directory)
          (ensure-directories-exist directory)
          (write-local-machine-conf
           (merge-pathnames *machine-filename* directory)
           eripa port public)
          (write-hosts-conf
           (merge-pathnames *hosts-filename* directory)
           key-records)
          (write-keypairs-conf
           (merge-pathnames *keypairs-filename* directory)
           key-records)
          (write-stakes-conf
           (merge-pathnames *stakes-filename* directory)
           stakes)
          (push directory directories))))
    directories))

(defun generate-stakes (pubkeys max-stake)
  "Given a list of pubkeys, generate a random stake for each"
  (mapcar (lambda (pubkey)
            (list pubkey (random max-stake)))
          pubkeys))

(defun generate-keys (records)
  (loop
     :for record :in records
     :for host = (getf record :hostname)
     :for eripa = (getf record :ip)
     :for port = (getf record :port)
     :collecting (list host eripa (or port *nominal-gossip-port*) (make-key-integers))))

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
    (dolist (key-record records)
      (destructuring-bind (host eripa port (public private))
          key-record
        (declare (ignore eripa public private))
        (format o "~s~&" `(,host ,port))))))

(defun write-keypairs-conf (path records)
  (with-open-file (o path
                     :direction :output
                     :if-exists :supersede)
    (format o ";;; ~A~%" *keypairs-filename*)
    (format o ";;; THIS FILE IS FOR TESTING ONLY~%")
    (dolist (key-record records)
      (destructuring-bind (host eripa port (public private))
          key-record
        (declare (ignore host eripa port))
        (format o "~s~&" `(,public ,private))))))

(defun write-stakes-conf (path records)
  (with-open-file (o path
                     :direction :output
                     :if-exists :supersede)
    (format o ";;; ~A~%" *stakes-filename*)
    (format o ";;; THIS FILE IS FOR TESTING ONLY~%")
    (dolist (stake records)
      (format o "~s~%" stake))))

;;; FIXME: should be moved as part of the crypto API
(defun make-key-integers ()
  "Makes a public/private keypair and returns a list of two integers thereof"
  (let ((keypair (pbc:make-key-pair (list :lisp-authority (uuid:make-v1-uuid)))))
    (list
     ; public is first
     (vec-repr:int
      (pbc:keying-triple-pkey keypair))
     ; private is second
     (vec-repr:int
      (pbc:keying-triple-skey keypair)))))



       
