(in-package :cl-user)

(prove:plan 1)
(let ((root #p"/var/tmp/emotiq/gossip/config/")
      (network
       `((:hostname "emq-01.aws.emotiq.ch"
                    :ip "34.239.111.18"
                    :port 65002)
         (:hostname "emq-02.aws.emotiq.ch"
                    :ip "52.12.224.84"
                    :port 8080)
         (:hostname "emq-02.aws.emotiq.ch"
                    :ip "52.54.123.84"
                    :port 65001)
         (:hostname "emq-03.aws.emotiq.ch"
                    :ip "52.54.224.84"
                    :port 443)
         (:hostname "emq-05.aws.emotiq.ch"
                    :ip "34.217.39.58"
                    :port 22))))
  (ensure-directories-exist root)
  (prove:ok (gossip/config:generate network :root root)
            (format nil "Generating test network configuration in~&~t~a~& for ~&~s~&…"
                    root network))
  (let* ((hostname
          (getf (alexandria:random-elt gossip/config:*aws-example*)
                :hostname))
         (p
          (make-pathname
           :directory `(,@(pathname-directory root) ,hostname)
           :defaults root)))
    (prove:plan 1)
    (prove:ok (probe-file p)
              (format nil "Generated configuration directory '~a' exists…"
                      p))))
;;; FIXME: remove directory from filesystem

(prove:finalize)



   
