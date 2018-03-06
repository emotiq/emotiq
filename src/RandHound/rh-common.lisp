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
;; Nodes Directory

(defstruct node-assoc
  pkey ip port)

(defvar *node-table*  (make-hash-table
                       :test 'equal)) ;; to compare case-sensitive Base-58 strings in pkeys

(defun init-nodes ()
  (clrhash *node-table*))

(defun add-node (pkey ip port)
  (setf (gethash pkey *node-table*)
        (make-node-assoc
         :pkey  pkey
         :ip    ip
         :port  port)))

(defun remove-node (pkey)
  (remhash pkey *node-table*))

(defun find-node (pkey)
  (gethash pkey *node-table*))

(defun get-nodes-vector ()
  (let ((nodes (sort (coerce
                      (um:accum acc
                        (maphash (lambda (k v)
                                   ;; accumulate nodes with the
                                   ;; numeric value of the public key
                                   ;; for use in sort
                                   (acc (cons (need-integer-form k) v)))
                                 *node-table*))
                      'vector)
                     '<
                     :key 'first)))
    ;; discard the numeric pkeys used by sort
    (map-into nodes 'cdr nodes)))

;; ------------------------------------------------------
;; Simulation support

(defvar *sim-pkey-skey-table* (make-hash-table
                           :test 'equal))

(defun dotted-ip-string (bytes)
  (format nil "~{~d~^:~}" (coerce bytes 'list)))

(defvar *sim-keys-file* (asdf:system-relative-pathname :randhound "sim-config/sim-keys.lisp"))

(defun sim-skey-for-pkey (pkey)
  (first (gethash pkey *sim-pkey-skey-table*)))

(defun build-sim-nodes (&optional (nbr 16))
  ;; NOTE: you don't want to run this very often. It takes a long time
  ;; due to PKBF2 @ 2048 iters of SHA3. About 2sec/key.
  (init-nodes)
  (clrhash *sim-pkey-skey-table*)
  (loop repeat nbr do
        (let* ((lst   (make-random-keypair))
               (pkey  (getf lst :pkey))
               (skey  (getf lst :skey))
               (r     (getf lst :r))
               (s     (getf lst :s))
               (ip    (dotted-ip-string (ctr-drbg 32)))
               (port  (ctr-drbg-int 16)))
          (add-node pkey ip port)
          (setf (gethash pkey *sim-pkey-skey-table*) (list skey r s))))
  (ensure-directories-exist *sim-keys-file* :verbose t)
  (with-open-file (f *sim-keys-file*
                     :direction :output
                     :if-exists :rename
                     :if-does-not-exist :create)
    (with-standard-io-syntax
      (pprint
       (loop for assoc across (get-nodes-vector)
             collect
             (let ((sk  (gethash (node-assoc-pkey assoc) *sim-pkey-skey-table*)))
               (cons assoc sk)))
       f))))

(defun load-sim-nodes ()
  (let ((lst (with-open-file (f *sim-keys-file*
                                :direction :input)
               (read f))))
    (init-nodes)
    (clrhash *sim-pkey-skey-table*)
    (dolist (grp lst)
      (destructuring-bind (assoc &rest sk) grp
        (let ((pkey (node-assoc-pkey assoc)))
          (setf (gethash pkey *node-table*) assoc
                (gethash pkey *sim-pkey-skey-table*) sk))
        ))))

;; -------------------------------------------------------


                
