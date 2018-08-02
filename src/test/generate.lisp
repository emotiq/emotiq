(in-package :emotiq-config-generate-test)

;;;; INTERNAL testing


(define-test key-generation ()
  (let* ((devops-plist emotiq/config/generate:*dns-ip-zt.emotiq.ch*)
         (nodes-dns-ip devops-plist)
         (nodes (emotiq/config/generate:generate-keys nodes-dns-ip)))
    (assert-eq (length devops-plist)
               (length nodes))))


;;;; EXTERNAL testing


(define-test network-generation ()
   (let* ((devops-plist emotiq/config/generate:*dns-ip-zt.emotiq.ch*)
          (directories (emotiq/config/generate:generate-network :nodes-dns-ip devops-plist)))
     (assert-eq (length devops-plist)
                (length directories))))


;;;; FIXME:  this test will fail unless `cosi-bls` is loaded.  How to test?
#+(or)
(define-test get-stakes ()
  (assert-true (emotiq/config:get-stakes)))

;;;; FIXME: Ensure that a configuration has been generated, then read
;;;; its genesis block, but do this without overwriting an existing configuration.
#+(or)
(define-test get-genesis-block ()
  (assert-true (emotiq/config:get-genesis-block)))
