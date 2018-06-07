(defpackage gossip/config
  (:use :cl)
  (:export
   #:generate))

(in-package :gossip/config)

(defun make-key-integers ()
  "Makes a public/private keypair and returns a list of two integers thereof"
  (let ((keypair (pbc:make-key-pair (list :lisp-authority (uuid:make-v1-uuid)))))
    (list
     ; public is first
     (vec-repr::convert-vec-to-int
      (vec-repr::bev-vec
       (pbc::public-key-val
        (pbc::keying-triple-pkey keypair))))
     ; private is second
     (vec-repr::convert-vec-to-int
      (vec-repr::bev-vec
       (pbc::secret-key-val
        (pbc::keying-triple-skey keypair)))))))

(defparameter *sample* 
  '((:hostname "emq-01.aws.emotiq.ch" :ip "34.239.111.18" :port 65002)
    (:hostname "emq-02.aws.emotiq.ch" :ip "52.54.224.84" :port 65002)
    (:hostname "emq-03.aws.emotiq.ch" :ip "34.238.39.58" :port 65002)))

(defun generate-keys (records)
  (loop
     :for record :in records
     :for host = (getf record :hostname)
     :for eripa = (getf record :ip)
     :for port = (getf record :port)
     :collecting (list host eripa port (make-key-integers))))


(defun generate (records &key (root #p"/var/tmp/conf/"))
  (let ((keys (generate-keys records)))
    (loop
       :for (host eripa port (public . private))
       :in keys
       :do (let ((directory (merge-pathnames (format nil "~a/" host) root)))
             (format *error-output* "~&Writing configuration for ~a~&~tto ~a.~&" host directory)
             (ensure-directories-exist directory)
             (write-local-machine-conf
              (merge-pathnames "local-machine.conf" directory)
              eripa port public)
             (write-hosts-conf
              (merge-pathnames "hosts.conf" directory)
              keys)
             (write-keypairs-conf
              (merge-pathnames "keypairs.conf" directory)
              keys)))))

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
    (loop :for (host eripa port (public  private)) :in records
       :doing (format o "~s~&" `(,host ,port)))))

(defun write-keypairs-conf (path records)
  (with-open-file (o path
                     :direction :output
                     :if-exists :supersede)
    (loop :for (host eripa port (public  private)) :in records
       :doing (format o "~s~&" `(,public ,private)))))

(defun write-pubkeys-conf (path records)
  (with-open-file (o path
                     :direction :output
                     :if-exists :supersede)
    (loop :for (host eripa port (public  private)) :in records
       :doing (format o "~s~&" public))))



       



  
