(in-package :emotiq/config)

(defparameter *emotiq-conf*
  (make-pathname :defaults (emotiq/fs:etc/)
                 :name "emotiq-conf"
                 :type "json"))

(defun settings/read () 
  (unless (probe-file *emotiq-conf*)
    (emotiq:note "No configuration able to be read from '~a'" *emotiq-conf*)
    (return-from settings/read nil))
  ;;; TODO: do gossip/configuration sequence
  (cl-json:decode-json-from-source *emotiq-conf*))


