(in-package :emotiq/config/generate)

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
       . "emotiq-genesis-block.json"))
  "The default configuration alist

Values are changed by pushing key value cons at the beginning of the list,
where they override any later identical keys according to the semantics of CL:ASSOC")

(defun generate-keys (records)
  "Generate keys for a list of plist RECORDS"
  (loop
     :for (public-key private-key) = (pbc:make-keying-integers)
     :for record :in records
     :collecting (append record
                         ;;; HACK adapt to gossip/config needs
                         (unless (find :gossip-server-port record)
                           (list :gossip-server-port 65002))
                         (list :public public-key)
                         (list :private private-key))))

(defun generate-stakes (public-keys &key (max-stake emotiq/config:*max-stake*))
  "Given a list of PUBLIC-KEYS, generate a random stake for each

Returns the enumeration of lists of public keys and staked amounts."
  (mapcar (lambda (pkey)
            (list pkey (random max-stake)))
          public-keys))

;;; A list of plists drives the generation routines, as this is what the
;;; devops-ansible hook passes the network generation routines
(defparameter *eg-config-zerotier*
  '((:hostname "zt-emq-01.zt.emotiq.ch"
     :ip "10.178.2.166"
     ;;; The ports take the default values if not otherwise specified
     :gossip-server-port 65002
     :rest-server-port 3140
     :websocket-server-port 3145)
    (:hostname "zt-emq-02.zt.emotiq.ch"
     :ip "10.178.0.71")
    (:hostname "zt-emq-03.zt.emotiq.ch"
     :ip "10.178.15.71"))
  "An example of the syntax to generate network artifacts for a Zerotier testnet configuration")

(defparameter *eg-config-localhost*
  '((:hostname "localhost"
     :local-process-nodes 4
     :gossip-server-port 65002
     :rest-server-port 3140
     :websocket-server-port 3145))
  "An example of the syntax to generate a purely local process 4 node network")

(defun generate-network (&key
                           (root (emotiq/fs:new-temporary-directory))
                           (nodes-dns-ip *eg-config-zerotier*)
                           (force nil)
                           (settings-key-value-alist nil))
  "Generate a test network configuration

Files are generated under the ROOT directory.

NODES-DNS-IP contains a plist of keys describing the tcp/udp service
endpoints at which the nodes will run.  See *EG-CONFIG-LOCALHOST* and
*EG-CONFIG-ZEROTIER* for examples of the configuration syntax."
  (let* ((nodes-with-keys (generate-keys nodes-dns-ip))
         (stakes (generate-stakes (mapcar (lambda (plist)
                                            (getf plist :public))
                                          nodes-with-keys)))
         (address-for-coins (getf (first nodes-with-keys) :public))
         directories configurations)
    (dolist (node nodes-with-keys)
      (let ((configuration
             (make-configuration node
                                 :address-for-coins address-for-coins
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
             (generate-node path
                            configuration
                            :force force
                            :key-records nodes-with-keys)
             directories)
            (push configuration configurations)))))
    (values 
     directories
     configurations)))

(defun make-configuration (node
                           &key
                             address-for-coins
                             stakes)
  (let ((configuration
         (copy-alist +default-configuration+)))
    (loop
       :for key-value-cons :in (alexandria:plist-alist node)
       :doing (push key-value-cons configuration))
    (when address-for-coins
      (push (cons :address-for-coins address-for-coins) configuration))
    (when stakes 
      (push (cons :stakes stakes) configuration))
    configuration))

(defun write-configuration (directory configuration)
  (with-open-file (o (merge-pathnames emotiq/config:*conf-filename* directory)
                     :if-exists :supersede
                     :direction :output)
    (cl-json:encode-json 
     (alexandria:alist-hash-table configuration)
     o)))

(defun generate-node (directory
                      configuration
                      &key
                        key-records
                        (force nil))
  "Generate a complete Emotiq node description within DIRECTORY for CONFIGURATION

Returns the directory in which the node configuration was written as the primary values.

The genesis block for the node is returned as the second value.
"
  (declare (ignore force)) ;; FIXME code explicit re-generation
  (emotiq:note "Writing node configuration to '~a'." directory)

  (generate-gossip-node
   :root directory
   :host (alexandria:assoc-value configuration :hostname)
   :eripa (alexandria:assoc-value configuration :ip)
   :gossip-port (alexandria:assoc-value configuration :gossip-server-port)
   :public (alexandria:assoc-value configuration :public)
   :key-records key-records)
  (write-configuration directory configuration)
  (output-stakes directory (alexandria:assoc-value configuration :stakes))
  (output-keypairs directory key-records)
  (values
   directory
   (create-genesis configuration :directory directory :force t)))

(defun create-genesis (configuration
                       &key
                         (directory (emotiq/fs:tmp/))
                         (force nil))
  (let ((genesis-block-path (merge-pathnames
                             (alexandria:assoc-value configuration
                                                     :genesis-block-file)
                             directory)))
    (when (or force
              (not (probe-file genesis-block-path)))
      (let ((genesis-block
             (cosi/proofs:create-genesis-block
              (alexandria:assoc-value configuration :address-for-coins)
              (alexandria:assoc-value configuration :stakes))))
        (with-open-file (o genesis-block-path
                           :direction :output
                           :if-exists :supersede)
          (cl-json:encode-json genesis-block o))
        ;;; FIXME: JSON doesn't currently round-trip very well, so use
        ;;; LISP-OBJECT-ENCODER as a workaround.
        (let ((p (make-pathname :type "loenc"
                                :defaults genesis-block-path)))
          (with-open-file (o p
                             :element-type '(unsigned-byte 8)
                             :direction :output
                             :if-exists :supersede)
            (lisp-object-encoder:serialize genesis-block o)))
        genesis-block))))

(defun output-stakes (directory stakes)
  (with-open-file (o (merge-pathnames emotiq/config:*stakes-filename* directory)
                     :direction :output
                     :if-exists :supersede)
    (format o ";;; Stakes for a generated testnet configuration~%")
    (dolist (stake stakes)
      (format o "~s~%" stake))))


(defun output-keypairs (directory nodes)
  (with-open-file (o (merge-pathnames emotiq/config:*keypairs-filename* directory)
                     :direction :output
                     :if-exists :supersede)
    (dolist (node nodes)
      (format o "~s~%" (list (getf node :public) (getf node :private))))))

(defun generated-directory (configuration)
  "For CONFIGURATION return a uniquely named relative directory for the network generation process"
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
                           (list host ip
                                 gossip-server-port rest-server-port websocket-server-port))))))


(defun generate-gossip-node (&key root host eripa gossip-port public key-records)
  (declare (ignore host)) ;; Hmm?
  (ensure-directories-exist root)
  (write-gossip-conf root
                     :eripa eripa :port gossip-port :public-key-or-keys public)
  (write-hosts-conf root
                    key-records)
  root)

(defun write-gossip-conf (directory &key eripa port public-key-or-keys)
  (let ((p (merge-pathnames emotiq/config:*local-machine-filename* directory)))
    (with-open-file (o p
                       :direction :output
                       :if-exists :supersede)
      (format o "~s~&"
              `(:eripa ,eripa
                :gossip-port ,port
                :pubkeys ,public-key-or-keys)))))

(defun write-hosts-conf (directory records)
  (let ((p (merge-pathnames emotiq/config:*hosts-filename* directory)))
    (with-open-file (o p
                       :direction :output
                       :if-exists :supersede)
      (dolist (record records)
        (format o "~s~&" `(,(getf record :ip)
                            ,(getf record :gossip-server-port)))))))

(defun ensure-defaults (&key
                          force
                          (for-purely-local nil)
                          (without-gossip nil)
                          (destination (emotiq/fs:etc/))
                          (nodes-dns-ip *eg-config-localhost*))
  "Ensure that configuration will start up, even in the absence of explicit configuration

Place resulting configuration in DESTINATION, defaulting to the
current value returned by (emotiq/fs:etc/).

With FORCE true, overwrite destination without warning. 

With FOR-PURELY-LOCAL, emits a purely local configuration for the local node.

WITHOUT-GOSSIP disables the gossip/cosi system from starting up. "
  (let ((root (emotiq/fs:new-temporary-directory)))
    (unless force
      (when (not (zerop
                  (length (directory
                           (make-pathname :name :wild :type :wild
                                           :defaults destination))))))
      (warn "Refusing to overwrite existing '~a' with defaults. Use force to try again." destination)
      (return-from ensure-defaults (emotiq/config:settings :root destination)))
    (ensure-directories-exist root)
    (let ((directories
           (generate-network :root root
                             :nodes-dns-ip nodes-dns-ip)))
      (uiop:run-program
       (format nil "rsync -avzP ~a ~a"
               (first directories)
               destination))
      (when for-purely-local 
        ;;; FIXME kludge
        (uiop:run-program
         (format nil "cat /dev/null > ~a"
                 (merge-pathnames emotiq/config:*hosts-filename* destination)))
        (let* ((keypairs
                (emotiq/config:get-keypairs :root destination))
               (local-machine
                (emotiq/config:local-machine :root destination))
               (eripa
                (getf local-machine :eripa))
               (gossip-port
                (getf local-machine :gossip-port))
               (result
                (write-gossip-conf destination
                                   :eripa eripa 
                                   :port gossip-port
                                   :public-key-or-keys (mapcar 'first keypairs))))
          (when without-gossip
            (let ((configuration (emotiq/config:settings :root destination)))
              (push (cons :gossip-server "false") configuration)
              (write-configuration destination configuration)))
          (emotiq:note "Finished mangling configuration for purely local nodes~%~t~a"
                       result)))
      (values 
       (emotiq/config:settings :root destination)
       destination))))


