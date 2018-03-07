;; rh-client.lisp -- Randhound Client
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

(in-package :randhound/client)

;; ---------------------------------------------------------------

(defun create-server-groups (vnodes)
  ;; divide servers into groups based on hash of node ID (pkey) and
  ;; random seed
  ;;
  ;; With N available servers, divide into Sqrt(N) groups, each with
  ;; approx Sqrt(N) servers / group
  ;;
  (let ((nel    (length vnodes))
        (ngrp   (isqrt nel))
        (tgrp   (make-array ngrp))   ;; vector of server lists
        (r      (ctr-drbg-int 256))) ;; random seed
    (loop for node across vnodes do
          ;; cheap hash is xor
          (let* ((pkey  (node-assoc-pkey node))
                 (ix    (mod (logxor r (need-integer-form pkey)) ngrp)))
            (push pkey (aref tgrp ix))))
    ;; return vector of groups, each group a list of pkeys
    tgrp))

(defstruct session-config
  pkeys tgrps max-bft purpose tstamp)

(defun construct-session-config (vnodes tgrps max-bft purpose)
  (make-session-config
   :pkeys   (map 'vector 'node-assoc-pkey vnodes)
   :tgrps   tgrps
   :max-bft max-bft
   :purpose purpose
   :tstamp  (get-universal-time)))

(defstruct session-config-message
  hash-config tgrps purpose tstamp)

(defun initialization (purpose max-bft)
  (let* ((vnodes  (get-nodes-vector))
         (nnodes  (length vnodes))
         (nneed   (1+ (* 3 max-bft))))
    (if (< nnodes nneed)
        (error "Not enough servers for BFT")
      ;; else
      (let* ((vnodes   (subseq vnodes 0 nneed))
             (tgrps    (create-server-groups vnodes))
             (config   (construct-session-config vnodes tgrps max-bft purpose))
             (hconfig  (sha3/256-buffers config))
             (init-msg (make-session-config-message
                        :hash-config hconfig
                        :tgrps       tgrps
                        :purpose     purpose
                        :tstamp      (session-config-tstamp config))))
        (record-to-log config)
        (record-to-log init-msg)
        (broadcast-message init-msg vnodes)))
    ))
         
