;; rh-commont.lisp -- Randhound Common Code between Client/Server
;;
;; DM/Emotiq  03/18
;; ---------------------------------------------------------------
#|
The MIT License

Copyright (c) 2018 Emotiq AG

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :randhound/common)

;; ---------------------------------------------------------------

(defun NYI (&rest args)
  (error "Not yet implemented: ~A" args))

;; ---------------------------------------------------------------
;; Nodes Directory

(defvar *max-bft*  0) ;; max number of Byzantine nodes

;; -------------------------------------------------------------

(defun broadcast-message (msg pkeys)
  (let ((me  (node-pkey (current-node))))
    (if (find me pkeys
              :test 'int=)
        (cosi-simgen:broadcast+me msg)
      (cosi-simgen:broadcast-to-others msg))))

(defun send-message (msg pkey)
  (apply 'send pkey msg))

(defun broadcast-grp+me (msg &key graphID)
  (cond (*use-real-gossip*
         (gossip:singlecast msg :graphID nil)
         (gossip:broadcast  msg :graphID graphID))
        
        (t
         (let ((me (node-pkey (current-node))))
           (apply 'send me msg)
           (mapc (lambda (pkey)
                   (apply 'send pkey msg))
                 (remove me graphID :test 'int=))))
        ))

(defun broadcast-grp (msg &key graphID)
  (cond (*use-real-gossip*
         (gossip:broadcast msg :graphID graphID))
        
        (t
         (let ((me (node-pkey (current-node))))
           (mapc (lambda (pkey)
                   (apply 'send pkey msg))
                 (remove me graphID :test 'int=))))
        ))
                          
;; ------------------------------------------------------------------

(defstruct randhound-state
  config commit)

(defstruct session-config
  pkeys tgrps max-bft purpose tstamp)

(defstruct subgroup-commit
  epoch thresh encr-shares proofs chks rval)


                
