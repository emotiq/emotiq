(in-package :emotiq/config)

(defparameter *dns-ip-zt.emotiq.ch* 
  '((:hostname "zt-emq-01.zt.emotiq.ch"
     :ip "10.178.2.166"
     ;;; The ports take the default values if not otherwise specified
     :gossip-server-port 65002
     :rest-server-port 3140
     :websocket-server-port 3145)
    (:hostname "zt-emq-02.zt.emotiq.ch"
     :ip "10.178.0.71")
    (:hostname "zt-emq-03.zt.emotiq.ch"
     :ip "10.178.15.71")))

(defparameter *emotiq-conf*
  (make-pathname :defaults (emotiq/fs:etc/)
                 :name "emotiq-conf"
                 :type "json"))

(um:defconstant+ +default-configuration+
    `((:rest-server
       . :true)
      (:rest-server-port
       . 3140)
      (:websocket-server
       . :true)
      (:websocket-server-port
       . 3145)
      (:gossip-server
       . :true)
      (:gossip-server-port
       . 65002)
      (:genesis-block
       . "emotiq-genesis-block.json")))

(defun generated-directory (configuration)
  (let ((host (alexandria:assoc-value configuration :host))
        (ip (alexandria:assoc-value configuration :ip))
        (gossip-server-port (alexandria:assoc-value configuration :gossip-server-port))
        (rest-server-port (alexandria:assoc-value configuration :rest-server-port))
        (websocket-server-port (alexandria:assoc-value configuration :websocket-server-port)))
    (make-pathname
     :directory `(:relative
                  ,(format nil "~{~a~^-~}"
                           (list host ip gossip-server-port rest-server-port websocket-server-port)))
     :defaults nil)))

(defun network/generate (&key
                           (root (emotiq/fs:tmp/))
                           (nodes-dns-ip *dns-ip-zt.emotiq.ch*)
                           (force nil)
                           (settings-key-value-alist nil))
  "Generate a test network configuration"
  (let ((nodes (keys/generate nodes-dns-ip))
        directories)
    (dolist (node nodes)
      (let ((configuration
             (copy-alist +default-configuration+))
            (hostname
             (alexandria:assoc-value node :hostname))
            (ip
             (alexandria:assoc-value node :ip)))
        (push (cons :hostname hostname)
              configuration)
        (push (cons :ip ip)
              configuration)
        (when settings-key-value-alist
          (loop :for (key . value)
             :in settings-key-value-alist
             :doing (push `(,key . ,value) configuration)))
        (let ((relative-path (generated-directory configuration)))
          (let ((path (merge-pathnames relative-path root)))
            (push 
             (node/generate path
                            configuration
                            :force force
                            :key-records nodes)
             directories)))))
    directories))

(defun node/generate (directory
                      configuration
                      &key
                        key-records
                        (force nil))
  "Generate a compete Emotiq node description within DIRECTORY for CONFIGURATION"
  (gossip/config:generate-node
   :path directory
   :host (alexandria:assoc-value configuration :host)
   :eripa (alexandria:assoc-value configuration :ip)
   :port (alexandria:assoc-value configuration :gossip-server-port)
   :key-records key-records)
  (with-open-file (o path
                     :direction :output)
    (cl-json:encode-json configuration o))
  (let ((genesis-block-path (merge-pathnames
                             (assoc :genesis-block configuration)
                             (emotiq/fs:etc/))))
    (unless (or force
                (probe-file genesis-block-path))
      (let ((genesis-block
             (cosi/proofs:create-genesis-block (public-key witnesses-and-stakes))))
        (with-open-file (o genesis-block-path
                           :direction :ouput
                           :if-exists :supersede)
          (with-open-file 
              (cl-json:encode-json genesis-block-path o)))))))
  
(defun settings/read () 
  (unless (probe-file *emotiq-conf*)
    (emotiq:note "No configuration able to be read from '~a'" *emotiq-conf*)
    (return-from settings/read nil))
  ;;; TODO: do gossip/configuration sequence
  (cl-json:decode-json-from-source *emotiq-conf*))


