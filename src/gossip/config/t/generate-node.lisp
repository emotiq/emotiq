(in-package :cl-user)

(prove:plan 1)
(let ((root #p"/var/tmp/emotiq/gossip/config/sample-node/")
      (node
       `(:host "emq-01.aws.emotiq.ch"
         :eripa "34.239.111.18"
         :gossip-port 65002
         :key-records nil))) ;; FIXME
  (ensure-directories-exist root)
  (let ((p (gossip/config:generate-node
            :path root
            :host "emq-01.aws.emotiq.ch"
            :eripa "34.239.111.18"
            :gossip-port 65002
            :key-records nil))) ;; FIXME
    (prove:plan 2)
    (prove:ok p
              (format nil 
                      "Generated test network configuration in~&~t~a~& for ~&~s~&…"
                      root))
    (prove:ok (probe-file p)
              (format nil
                      "Generated configuration directory '~a' exists…"
                      p))))
                      
;;; FIXME: remove directory from filesystem

(prove:finalize)



   
