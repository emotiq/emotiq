(in-package :emotiq/config)

;;; We wish to interpret a list of plists, as this is what the
;;; devops-ansible hook passes the network generation routines
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
    `((:hostname
       . "localhost")
      (:ip
       . "127.0.0.1")
      (:rest-server
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
      (:genesis-block-file
       . "emotiq-genesis-block.json")))

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
    

(defun network/generate (&key
                           (root (emotiq/fs:tmp/))
                           (nodes-dns-ip *dns-ip-zt.emotiq.ch*)
                           (force nil)
                           (settings-key-value-alist nil))
  "Generate a test network configuration"
  (let* ((nodes (keys/generate nodes-dns-ip))
         (stakes (stakes/generate (mapcar (lambda (plist)
                                            (getf plist :public))
                                          nodes)))
         directories)
    (dolist (node nodes)
      (let ((configuration
             (copy-alist +default-configuration+))
            (hostname
             (getf node :hostname))
            (ip
             (getf node :ip)))
        (push (cons :hostname hostname)
              configuration)
        (push (cons :ip ip)
              configuration)
        (push (cons :public
                    (getf node :public))
              configuration)
        (push (cons :private
                    (getf node :private))
              configuration)
        (push (cons :address-for-coins
                    (getf (first nodes) :public))
              configuration)
        (push (cons :stakes
                    stakes)
              configuration)
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
             directories)))))
    directories))

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
    (cl-json:encode-json configuration o))
  (stakes/write (alexandria:assoc-value configuration :stakes)
                :path (make-pathname :defaults directory
                                     :name "stakes"
                                     :type "conf"))
  (genesis/create configuration :directory directory :force t))
  
(defun settings/read () 
  (unless (probe-file *emotiq-conf*)
    (emotiq:note "No configuration able to be read from '~a'" *emotiq-conf*)
    (return-from settings/read nil))
  ;;; TODO: do gossip/configuration sequence
  (cl-json:decode-json-from-source *emotiq-conf*))


(defun ensure-defaults (&key
                          (c (copy-alist +default-configuration+))
                          force 
                          (nodes-dns-ip *dns-ip-zt.emotiq.ch* nodes-dns-ip-p))
  "Ensure that configuration will start up, even in the absence of explicit configuration"
  (let ((root (merge-pathnames (make-pathname
                                :directory '(:relative "var"))
                               (emotiq/fs:tmp/)))
        (local (generated-directory c))
        (destination (emotiq/fs:etc/)))
    (unless force 
      (when (not (zerop
                  (length (directory
                           (make-pathname :name :wild :type :wild
                                           :defaults destination))))))

      (warn "Refusing to overwrite existing '~a' with defaults. Use force to try again." destination)
      (return-from ensure-defaults (settings/read)))
    (unless nodes-dns-ip-p
      (push
       `(:hostname "localhost" ;; FIXME: introspect local hostname
                   :ip "127.0.0.1")
       nodes-dns-ip))
    (ensure-directories-exist root)
    (network/generate :root root
                      :nodes-dns-ip nodes-dns-ip)
    (uiop:run-program
     (format nil "rsync -avzP ~a ~a"
             (merge-pathnames local root)
             destination))
    (settings/read)))

  



    
