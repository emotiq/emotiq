(in-package :emotiq/config)

(defun emotiq-conf ()
  (make-pathname :defaults (emotiq/fs:etc/)
                 :name "emotiq-conf"
                 :type "json"))

(defun generated-directory (configuration)
  (let ((host
         (alexandria:assoc-value configuration :hostname))
        (ip
         (alexandria:assoc-value configuration :ip))
        (gossip-server-port
         (alexandria:assoc-value configuration :gossip-server-port))
        (rest-server-port
         (alexandria:assoc-value configuration :rest-server-port))
        (websocket-server-port
         (alexandria:assoc-value configuration :websocket-server-port)))
    (make-pathname
     :directory `(:relative
                  ,(format nil "~{~a~^-~}"
                           (list host ip gossip-server-port rest-server-port websocket-server-port))))))


(defun settings/read (&optional key)
  (unless (probe-file (emotiq-conf))
    (emotiq:note "No configuration able to be read from '~a'" (emotiq-conf))
    (return-from settings/read nil))
  ;;; TODO: do gossip/configuration sequence
  (let ((c (cl-json:decode-json-from-source (emotiq-conf))))
    (if key
        (alexandria:assoc-value c key)
        c)))
