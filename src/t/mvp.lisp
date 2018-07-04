(in-package :cl-user)

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
  
 
