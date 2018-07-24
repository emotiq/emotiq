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

(defun make-configuration (node &key address-for-coins stakes public private)
  (let ((configuration
         (copy-alist +default-configuration+)))
    (loop
       :for (key . value) :in (alexandria:plist-alist node)
       :doing (push (cons key value) configuration))
    (when address-for-coins
      (push (cons :address-for-coins
                  address-for-coins)
            configuration))
    (when stakes
      (push (cons :stakes
                  stakes)
            configuration))
    (when public
      (push (cons :public
                  public)
            configuration))
    (when private
      (push (cons :private
                  private)
            configuration))
    configuration))

(defun network/generate (&key
                           (root (emotiq/fs:tmp/))
                           (nodes-dns-ip *dns-ip-zt.emotiq.ch*)
                           (force nil)
                           (settings-key-value-alist nil))
  "Generate a test network configuration

Returns a list of directories in which configurations have been generated."
  (let* ((nodes (keys/generate nodes-dns-ip))
         (stakes (stakes/generate (mapcar (lambda (plist)
                                            (getf plist :public))
                                          nodes)))
         directories
         configurations)
    (dolist (node nodes)
      (let ((configuration
             (make-configuration
              node
              :address-for-coins (getf (first nodes) :public)
              :public (getf node :public)
              :private (getf node :private)
              :stakes stakes)))
        ;;; Override by pushing ahead in alist
        (when settings-key-value-alist
          (loop :for setting
             :in settings-key-value-alist
             :doing (push setting configuration)))
        (let ((relative-path (generated-directory configuration)))
          (let ((path (merge-pathnames relative-path root))
                (configuration (copy-alist configuration)))
            (push 
             (node/generate path
                            configuration
                            :force force
                            :key-records nodes)
             directories)
            (push configuration configurations)))))
    (values
     directories
     configurations)))

(defun node/generate (directory
                      configuration
                      &key
                        key-records
                        (force nil))
  "Generate a complete Emotiq node description within DIRECTORY for CONFIGURATION"
  (declare (ignore force)) ;; FIXME code explicit re-generation
  (gossip/config:generate-node 
   :root directory
   :host (alexandria:assoc-value configuration :hostname)
   :eripa (alexandria:assoc-value configuration :ip)
   :gossip-port (alexandria:assoc-value configuration :gossip-server-port)
   :public (alexandria:assoc-value configuration :public)
   :private (alexandria:assoc-value configuration :private)
   :key-records key-records)
  (with-open-file (o (make-pathname :defaults directory
                                    :name (pathname-name *emotiq-conf*)
                                    :type "json")
                     :if-exists :supersede
                     :direction :output)
    (cl-json:encode-json
     ;;; configuration is an alist with later values overriding
     ;;; earlier ones.  The CL-JSON serialization will not add keys
     ;;; that have already been seen, so by reversing the alist we
     ;;; should get the correct JSON serialization
     (reverse configuration)
     o))
  (stakes/write (alexandria:assoc-value configuration :stakes)
                :path (make-pathname :defaults directory
                                     :name "stakes"
                                     :type "conf"))
  (genesis/create configuration :directory directory :force t)
  directory)
  
(defun settings/read (&optional key) 
  (unless (probe-file *emotiq-conf*)
    (emotiq:note "No configuration able to be read from '~a'" *emotiq-conf*)
    (return-from settings/read nil))
  ;;; TODO: do gossip/configuration sequence
  (let ((c (cl-json:decode-json-from-source (emotiq-conf))))
    (if key
        (alexandria:assoc-value c key)
        c)))
