;; rh-init.lisp - Randhound init
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

;; -------------------------------------------------------
;; For non-sim world

(defun init ()
  (let* ((witnesses (mapcar 'first (get-witness-list)))
         (max-bft   (floor (length witnesses) 3)))
    (setf (node-rh-state (current-node))
          (make-randhound-state
           :config (make-session-config
                    :pkeys   witnesses
                    :max-bft max-bft
                    :purpose :election
                    :tstamp  (uuid:make-v1-uuid))
           :commit (make-subgroup-commit
                    :thresh      0
                    :encr-shares nil
                    :proofs      nil)))
    ))

;; ---------------------------------------------------------------------------------
#|
(defvar *keys-file* (asdf:system-relative-pathname :randhound "config/keys.lisp"))

(defun load-nodes ()
  ;; use this to load the keys database from disk.
  (let ((lst (with-open-file (f *keys-file*
                                :direction :input)
               (read f))))
    (let* ((nnodes  (length lst))
           (max-bft (floor (1- nnodes) 3)))
      (setf *max-bft* max-bft)
      (dolist (grp lst)
        (let ((pkey (getf grp :pkey))
              (ip   (getf grp :ip))
              (port (getf grp :port))
              (r    (getf grp :r))
              (s    (getf grp :s)))
          (unless (validate-pkey pkey r s)
            (error "Invalid public key: ~A" pkey))
          (setf (gethash pkey *node-table*) (make-node-assoc
                                             :pkey  pkey
                                             :ip    ip
                                             :port  port))
          )))))

|#
