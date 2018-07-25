(in-package :emotiq/config)

(defun emotiq-conf ()
  (make-pathname :defaults (emotiq/fs:etc/)
                 :name "emotiq-conf"
                 :type "json"))

(defun settings/read (&optional key)
  (unless (probe-file (emotiq-conf))
    (emotiq:note "No configuration able to be read from '~a'" (emotiq-conf))
    (return-from settings/read nil))
  ;;; TODO: do gossip/configuration sequence
  (let ((c (cl-json:decode-json-from-source (emotiq-conf))))
    (if key
        (alexandria:assoc-value c key)
        c)))
