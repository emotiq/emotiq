(in-package :gossip/config)

(defparameter *nominal-gossip-port* 65002 "Duplicate of same symbol in gossip package
    to avoid package interdependencies")

;;; FIXME refactor the interface for passing parameters to something
;;; simpler gossip/config should probably declare a CLOS object as
;;; containing its configuration needs rather than relying on this
;;; mess.
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
      (format o "~s~&" `(,(getf record :ip)
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




       
