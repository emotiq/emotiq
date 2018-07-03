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

(defun get-nodes-vector ()
  (mapcar 'first (get-witness-list)))

;; -------------------------------------------------------------

(defun make-randhound-msg (msg)
  `(:randhound ,@msg))

(defun broadcast-message (msg pkeys)
  (let ((me   (node-pkey (current-node)))
        (rmsg (make-randhound-msg msg)))
    (if (find me pkeys
              :test 'int=)
        (cosi-simgen:broadcast+me rmsg)
      (cosi-simgen:broadcast-to-others rmsg))))

(defun send-message (msg pkey)
  (apply 'send pkey (make-randhound-msg msg)))

(defun broadcast-grp+me (msg &key graphID)
  (let ((rmsg (make-randhound-msg msg)))
    (cond (*use-real-gossip*
           (gossip:singlecast rmsg :graphID nil)
           (gossip:broadcast  rmsg :graphID graphID))

          (t
           (let ((me (node-pkey (current-node))))
             (apply 'send me rmsg)
             (mapc (lambda (pkey)
                     (apply 'send pkey rmsg))
                   (remove me graphID :test 'int=))))
          )))

(defun broadcast-grp (msg &key graphID)
  (let ((rmsg (make-randhound-msg msg)))
    (cond (*use-real-gossip*
           (gossip:broadcast rmsg :graphID graphID))

          (t
           (let ((me (node-pkey (current-node))))
             (mapc (lambda (pkey)
                     (apply 'send pkey rmsg))
                   (remove me graphID :test 'int=))))
          )))
                          

;; ------------------------------------------------------------------

(defstruct randhound-state
  config commit)

(defstruct session-config
  pkeys tgrps max-bft purpose tstamp)

(defstruct subgroup-commit
  thresh encr-shares proofs)


                
