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


(define-test generate-localhost-many-ports ()
 (let ((service-description
        '((:hostname "127.0.0.1" :ip "127.0.0.1"
           :gossip-server-port 65000 :rest-server-port 3140 :websocket-server-port 4145)
          (:hostname "127.0.0.1" :ip "127.0.0.1"
           :gossip-server-port 65001 :rest-server-port 3141 :websocket-server-port 4146)
          (:hostname "127.0.0.1" :ip "127.0.0.1"
           :gossip-server-port 65002 :rest-server-port 3142 :websocket-server-port 4147)
          (:hostname "127.0.0.1" :ip "127.0.0.1" :gossip-server-port 65003 :rest-server-port 3143 :websocket-server-port 4148)
          (:hostname "127.0.0.1" :ip "127.0.0.1" :gossip-server-port 65004 :rest-server-port 3144 :websocket-server-port 4149)
          (:hostname "127.0.0.1" :ip "127.0.0.1" :gossip-server-port 65005 :rest-server-port 3145 :websocket-server-port 4150)
          (:hostname "127.0.0.1" :ip "127.0.0.1" :gossip-server-port 65006 :rest-server-port 3146 :websocket-server-port 4151)
          (:hostname "127.0.0.1" :ip "127.0.0.1"
           :gossip-server-port 65007 :rest-server-port 3147 :websocket-server-port 4152)
          (:hostname "127.0.0.1" :ip "127.0.0.1"
           :gossip-server-port 65008 :rest-server-port 3148 :websocket-server-port 4153)
          (:hostname "127.0.0.1" :ip "127.0.0.1"
           :gossip-server-port 65009 :rest-server-port 3149 :websocket-server-port 4154)
          (:hostname "127.0.0.1" :ip "127.0.0.1"
           :gossip-server-port 65010 :rest-server-port 3150 :websocket-server-port 4155)
          (:hostname "127.0.0.1" :ip "127.0.0.1"
           :gossip-server-port 65011 :rest-server-port 3151 :websocket-server-port 4156)
          (:hostname "127.0.0.1" :ip "127.0.0.1"
           :gossip-server-port 65012 :rest-server-port 3152 :websocket-server-port 4157))))
   (let ((result 
          (emotiq/config/generate:generate-network :nodes-dns-ip service-description)))
     (assert-true result))))


