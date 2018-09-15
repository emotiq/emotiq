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
  "Generate keys for a list of plist RECORDS

The following are valid configuration of a plist record

    :hostname

    :ip 

    :gossip-server-port

    :websocket-server-port

    :rest-server-port

    :local-process-nodes
"
  (let (result)
    (dolist (record records)
      (let* ((key-or-keys (loop
                             :repeat (getf record :local-process-nodes 1)
                             :collecting (pbc:make-key-pair-uuid)))
             (public-keys (mapcar 'pbc:keying-triple-pkey key-or-keys))
             (secret-keys (mapcar 'pbc:keying-triple-skey key-or-keys)))
        (push (append record
                      ;; HACK adapt to gossip configuration needs.
                      ;; Better to have a template with default values?
                      (unless (find :gossip-server-port record)
                        (list :gossip-server-port 65002))
                      (list :public-keys public-keys)
                      (list :private-keys secret-keys))
              result)))
    (nreverse result)))

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

(defun public-keys (nodes)
  "Return the in-order list of all public keys for a NODES plist."
  (um:flatten
   (mapcar
    (lambda (plist)
      (getf plist :public-keys))
    nodes)))

(defun generate-network (&key
                           (root (emotiq/fs:new-temporary-directory))
                           (nodes-plist *eg-config-zerotier*)
                           (force nil)
                           (settings-key-value-alist nil))
  "Generate a test network configuration

Files are generated under the ROOT directory.

NODES-PLIST contains a plist of keys describing the tcp/udp service
endpoints at which the nodes will run.  See *EG-CONFIG-LOCALHOST* and
*EG-CONFIG-ZEROTIER* for examples of the configuration syntax."
  (let* ((nodes-with-keys (generate-keys nodes-plist))
         (pkeys (public-keys nodes-with-keys))
         (stakes (generate-stakes pkeys))
         ;; currently pay all genesis coin to the first pkey
         (address-for-coins (first pkeys))
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
  (let ((configuration (copy-alist +default-configuration+)))
    (loop
       :for key-value-cons :in (alexandria:plist-alist node)
       :doing (push key-value-cons configuration))
    (when address-for-coins
      (push (cons :address-for-coins address-for-coins) configuration))
    (when stakes
      (push (cons :stakes stakes) configuration))
    configuration))

(defun generate-node (directory
                      configuration
                      &key
                        key-records
                        (force nil))
  "Generate a complete Emotiq node description within DIRECTORY for CONFIGURATION

Returns the directory in which the node configuration was written as
the primary value.

The genesis block for the node is returned as the second value.
"
  (declare (ignore force)) ;; FIXME code explicit re-generation
  (emotiq:note "Writing node configuration to '~a'." directory)

  (generate-gossip-node
   :root directory
   :host (alexandria:assoc-value configuration :hostname)
   :eripa (alexandria:assoc-value configuration :ip)
   :gossip-port (alexandria:assoc-value configuration :gossip-server-port)
   :public-keys (alexandria:assoc-value configuration :public-keys)
   :key-records key-records)
  (with-open-file (o (merge-pathnames emotiq/config:*conf-filename* directory)
                     :if-exists :supersede
                     :direction :output)
    (cl-json:encode-json
     (alexandria:alist-hash-table configuration)
     o))

  (output-stakes directory (alexandria:assoc-value configuration :stakes))
  (output-keypairs directory key-records)
  (values
   directory
   (create-genesis configuration :directory directory :force t)))

(defmacro with-safe-output (&body body)
  `(with-standard-io-syntax
     (let ((*print-readably* t))
       ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-safe-output" 0)

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
            (with-safe-output
              (lisp-object-encoder:serialize genesis-block o))))
        genesis-block))))

(defun output-stakes (directory stakes)
  (with-open-file (o (merge-pathnames emotiq/config:*stakes-filename* directory)
                     :direction :output
                     :if-exists :supersede)
    (with-safe-output
      (format o ";;; Stakes for a generated testnet configuration~%")
      (dolist (stake stakes)
        (format o "~s~%" stake)))))


(defun output-keypairs (directory nodes)
  (with-open-file (o (merge-pathnames emotiq/config:*keypairs-filename* directory)
                     :direction :output
                     :if-exists :supersede)
    (with-safe-output
      (dolist (node nodes)
        (format o "~s~%" (list (getf node :public-keys)
                               (getf node :private-keys)))))))

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
                                 gossip-server-port
                                 rest-server-port
                                 websocket-server-port))))))

(defun generate-gossip-node (&key
                               root host eripa
                               gossip-port public-keys key-records)
  (declare (ignore host)) ;; Hmm?
  (ensure-directories-exist root)
  (write-gossip-conf root
                     :eripa eripa :port gossip-port :public-keys public-keys)
  (write-hosts-conf root
                    key-records)
  root)

(defun write-gossip-conf (directory &key eripa port public-keys)
  (let ((p (merge-pathnames emotiq/config:*local-machine-filename* directory)))
    (with-open-file (o p
                       :direction :output
                       :if-exists :supersede)
      (with-safe-output
        (format o "~s~&"
                `(:eripa ,eripa
                  :gossip-port ,port
                  :pubkeys ,public-keys))))))

(defun write-hosts-conf (directory records)
  (let ((p (merge-pathnames emotiq/config:*hosts-filename* directory)))
    (with-open-file (o p
                       :direction :output
                       :if-exists :supersede)
      (with-safe-output
        (dolist (record records)
          (format o "~s~&" `(,(getf record :ip)
                             ,(getf record :gossip-server-port))))))))

(defun ensure-defaults (&key
                          force
                          (for-purely-local nil)
                          (destination (emotiq/fs:etc/))
                          (nodes-plist *eg-config-zerotier* nodes-plist-p))
  "Ensure enough default configuration so that EMOTIQ:MAIN will run

With FORCE true, overwrite destination without warning.

With FOR-PURELY-LOCAL, emits a purely local configuration for the local node."
  (let ((root (emotiq/fs:new-temporary-directory)))
    (unless force
      (when (not (zerop
                  (length (directory
                           (make-pathname :name :wild :type :wild
                                          :defaults destination))))))
      (warn "Refusing to overwrite existing '~a' with defaults. Use force to try again."
            destination)
      (return-from ensure-defaults (emotiq/config:settings)))
    (ensure-directories-exist root)
    (let ((directories
           (generate-network :root root
                             :nodes-plist nodes-plist)))
      (uiop:run-program
       (format nil "rsync -avzP ~a ~a"
               (first directories)
               destination))
      (when for-purely-local
        ;;; FIXME kludge
        (uiop:run-program
         (format nil "cat /dev/null > ~a"
                 (merge-pathnames emotiq/config:*hosts-filename* destination)))
        (let* ((keypairs (emotiq/config:get-keypairs))
               (local-machine (emotiq/config:local-machine))
               (eripa (getf local-machine :eripa))
               (gossip-port (getf local-machine :gossip-port))
               (result
                (write-gossip-conf destination
                                   :eripa eripa
                                   :port gossip-port
                                   :public-keys (mapcar 'first keypairs))))
          (emotiq:note "Finished mangling configuration for purely local nodes~%~t~a"
                       result)))
      (values
       (emotiq/config:settings)
       destination))))
