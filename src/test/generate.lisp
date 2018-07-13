(in-package :emotiq-config-generate-test)

(define-test generate-network ()
  (let ((nodes '((:hostname "127.0.0.1" :ip "127.0.0.1"
                  :gossip-server-port 65000 :rest-server-port 3140
                  :websocket-server-port 3145)
                 (:hostname "127.0.0.1" :ip "127.0.0.1"
                  :gossip-server-port 65001 :rest-server-port 3141
                  :websocket-server-port 3146)
                 (:hostname "127.0.0.1" :ip "127.0.0.1"
                  :gossip-server-port 65002 :rest-server-port 3142
                  :websocket-server-port 3147)))
        (root (merge-pathnames (make-pathname :directory `(:relative ,(symbol-name (gensym))))
                               (emotiq/fs:tmp/))))
    (ensure-directories-exist root)
    (multiple-value-bind (directories configurations)
        (emotiq/config:network/generate :nodes-dns-ip nodes :root root)
      (loop
         :for c :in configurations
         :doing (assert-true
                 (probe-file
                  (merge-pathnames (emotiq/config::generated-directory c) root)))))))



