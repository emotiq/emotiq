(in-package :cl-user)

(prove:plan 1)
(let ((root #p"/var/tmp/emotiq/gossip/config/sample-node/"))
  (ensure-directories-exist root)
  (let ((p (gossip/config:generate-node
            :root root
            :host "emq-01.aws.emotiq.ch"
            :eripa "34.239.111.18"
            :gossip-port 65002
            :key-records nil))) ;; FIXME
    (prove:plan 2)
    (prove:ok p
              (format nil 
                      "Generating gossip node configuration in~&~t~a~&…"
                      root))
    (prove:ok (probe-file p)
              (format nil
                      "Generated configuration directory '~a' exists…"
                      p))))
                      
;;; FIXME: remove directory from filesystem

(prove:finalize)



   
