;; rh-sim.lisp -- Simulation support for RandHound
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

;; ------------------------------------------------------
;; Simulation support

#|
(defvar *sim-pkey-skey-table* (make-hash-table
                           :test 'equal))

(defun dotted-ip-string (bytes)
  (format nil "~{~d~^:~}" (coerce bytes 'list)))

(defvar *sim-keys-file* (asdf:system-relative-pathname :randhound "sim-config/sim-keys.lisp"))

(defun sim-skey-for-pkey (pkey)
  (first (gethash pkey *sim-pkey-skey-table*)))

(defun build-sim-nodes (&optional (nbr 16))
  ;; NOTE: you don't want to run this very often. It takes a long time
  ;; due to PKBF2 @ 2048 iters of SHA3. About 2.6 sec/key.
  (loop repeat nbr do
        (let* ((lst   (make-random-keypair))
               (pkey  (getf lst :pkey))
               (skey  (getf lst :skey))
               (r     (getf lst :r))
               (s     (getf lst :s))
               (ip    (dotted-ip-string (ctr-drbg 32)))
               (port  (ctr-drbg-int 16)))
          (add-node pkey ip port)
          (setf (gethash pkey *sim-pkey-skey-table*) (list skey :r r :s s))))
  (let* ((vnodes  (get-nodes-vector))
         (nnodes  (length vnodes))
         (max-bft (floor (1- nnodes) 3))
         (lst     (loop for assoc across vnodes
                        collect
                        (let ((sk  (gethash (node-assoc-pkey assoc) *sim-pkey-skey-table*)))
                          (list
                           ;; use a non-brittle format for external store, just
                           ;; basic Lisp data
                           :pkey  (node-assoc-pkey assoc)
                           :ip    (node-assoc-ip   assoc)
                           :port  (node-assoc-port assoc)
                           :skey  (pop sk)
                           :r     (getf sk :r)
                           :s     (getf sk :s))))
                  ))
    (setf *max-bft* max-bft)
    (ensure-directories-exist *sim-keys-file* :verbose t)
    (with-open-file (f *sim-keys-file*
                       :direction :output
                       :if-exists :rename
                       :if-does-not-exist :create)
      (with-standard-io-syntax
        (format f ";; file of ~A simulation keys" nbr)
        (pprint lst f))
      )))

#|
;; gen up 300 sim nodes without tying up the REPL
(ac:spawn (lambda ()
            (ac:pr :start-build-sim-nodes)
            (build-sim-nodes 300)
            (ac:pr :done-build-sim-nodes)))
|#

(defun load-sim-nodes ()
  ;; use this to load the keys database from disk.
  ;; much faster for startup.
  (let ((lst (with-open-file (f *sim-keys-file*
                                :direction :input)
               (read f))))
    (let* ((nnodes  (length lst))
           (max-bft (floor (1- nnodes) 3)))
      (setf *max-bft* max-bft)
      (dolist (grp lst)
        (let ((pkey (getf grp :pkey))
              (ip   (getf grp :ip))
              (port (getf grp :port))
              (skey (getf grp :skey))
              (r    (getf grp :r))
              (s    (getf grp :s)))
          (unless (validate-pkey pkey r s)
            (error "Invalid public key: ~A" pkey))
          (setf (gethash pkey *node-table*) (make-node-assoc
                                             :pkey  pkey
                                             :ip    ip
                                             :port  port)
                (gethash pkey *sim-pkey-skey-table*) (list skey
                                                           :r  r
                                                           :s  s))
          )))))

|#
