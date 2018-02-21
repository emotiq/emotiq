;; cosi-keying.lisp -- Keying for Cosi networks
;;
;; DM/Emotiq 02/18
;; ----------------------------------------------------------------
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

(in-package :cosi-simgen)

(import 'ecc-crypto-b571:random-between)

;; --------------------------------------------
;; Hashing with SHA3

(defun select-sha3-hash ()
  (let ((nb  (1+ (integer-length *ed-q*))))
    (cond ((> nb 384) :sha3)
          ((> nb 256) :sha3/384)
          (t          :sha3/256)
          )))

(defun sha3-buffers (&rest bufs)
  ;; assumes all buffers are UB8
  (let ((dig  (ironclad:make-digest (select-sha3-hash))))
    (dolist (buf bufs)
      (ironclad:update-digest dig buf))
    (values (ironclad:produce-digest dig)
            dig)))

(defun stretched-hash (nstretch &rest bufs)
  (multiple-value-bind (hash dig) (apply 'sha3-buffers bufs)
    (loop repeat nstretch do
          (ironclad:update-digest dig hash)
          (dolist (buf bufs)
            (ironclad:update-digest dig buf))
          (setf hash (ironclad:produce-digest dig)))
    hash))

(defun convert-ub8v-to-int (vec)
  (let ((ans  0))
    (loop for v across vec do
          (setf ans (logior (ash ans 8) v)))
    ans))

(defun mod-r (v)
  (mod v *ed-r*))

(defun hash-to-int (vec)
  (mod-r (convert-ub8v-to-int vec)))

(defun add-mod-r (a b)
  (add-mod *ed-r* a b))

(defun sub-mod-r (a b)
  (sub-mod *ed-r* a b))

(defun mult-mod-r (a b)
  (mult-mod *ed-r* a b))

(defun top-random (limit)
  (random-between (ash limit -1) limit))

(defun hash-to-id (pt pkey)
  (hash-to-int
   (sha3-buffers (loenc:encode
                  (list (ed-compress-pt pt)
                        pkey)))
   ))

(defun crypto-puzzle (skey pkey)
  (let* ((k   (top-random *ed-r*))
         (kpt (ed-nth-pt k))
         (id  (hash-to-id kpt pkey))
         (chk (sub-mod-r k (mult-mod-r id skey))))
    (values id chk)))

(defun pt-mul (pt k)
  (ed-mul (ed-decompress-pt pt) k))

(defun validate-pkey (id pkey chk)
  (let* ((kpt (ed-add (ed-nth-pt chk)
                      (pt-mul pkey id)))
         (chk-id (hash-to-id kpt pkey)))
    (eql chk-id id)))
           
(defun make-id (skey pkey)
  (multiple-value-bind (id chk) (crypto-puzzle skey pkey)
    (list
     :skey skey
     :pkey pkey
     :id   id
     :chk  chk)))

(defun make-random-keypair (seed)
  ;; from anything as a seed, return private, public key pair
  (let* ((skey  (hash-to-int
                 (stretched-hash (top-random 4096)
                                 (loenc:encode (list (uuid:make-v1-uuid) seed)))))
         (pkey (ed-compress-pt (ed-nth-pt skey))))
    (make-id skey pkey)))

(defun make-deterministic-keypair (seed)
  (let* ((skey (hash-to-int
                (sha3-buffers (loenc:encode seed))))
         (pkey (ed-compress-pt (ed-nth-pt skey))))
    (make-id skey pkey)))

                         
