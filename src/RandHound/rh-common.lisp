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

(defvar *max-bft*  0) ;; max number of Byzantine nodes

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

;; -------------------------------------------------------------

(defun NYI (&rest args)
  (error "Not yet implemented: ~A" args))

(defvar *sim-log* nil) ;; in-memory log for sim, FIFO order

(defun record-to-log (msg)
  (push msg *sim-log*))

(defun broadcast-message (msg nodes)
  (loop for node across nodes do
        (send-message msg (node-assoc-ip node) (node-assoc-port node))))

(defun send-message (msg ip port)
  (declare (ignore msg ip port))
  (NYI :send-message))




                
