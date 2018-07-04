(in-package :cl-user)


;;; INTERNAL testing 
(prove:plan 1)
(let* ((nodes-dns-ip emotiq/config:*dns-ip-zt.emotiq.ch*)
       (nodes (emotiq/config::keys/generate nodes-dns-ip)))
  (prove:is (length nodes)
            3
            "Node key generation for three nodes…")
  (prove:diag (format nil "Nodes created as ~%~t~s" nodes)))

;;; EXTERNAL testing
(prove:plan 1)
(let ((nodes-dns-ip emotiq/config:*dns-ip-zt.emotiq.ch*))
  (prove:ok
   (emotiq/config:network/generate :nodes-dns-ip nodes-dns-ip)
   (format nil "Generation of zt.emotiq.ch devnet artifacts from~%~&~s…"
           nodes-dns-ip)))

(prove:plan 1)
(let ((stakes (emotiq/config:get-stakes)))
  (prove:ok stakes
            "GET-STAKES returns something…"))
  
  
(prove:finalize)
  
 
