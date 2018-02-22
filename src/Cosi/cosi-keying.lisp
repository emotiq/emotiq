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

(defpackage :cosi-keying
  (:use
   :common-lisp
   :ecc-crypto-b571
   :crypto-mod-math
   :edwards-ecc)
  (:export
   :make-keypair
   :validate-pkey
   :ed-dsa
   :ed-dsa-validate
   ))

(in-package :cosi-keying)

;; --------------------------------------------
;; Hashing with SHA3
#|
(defun select-sha3-hash ()
  (let ((nb  (1+ (integer-length *ed-q*))))
    (cond ((> nb 384) :sha3)
          ((> nb 256) :sha3/384)
          (t          :sha3/256)
          )))

(defun csha3-buffers (&rest bufs)
  ;; assumes all buffers are UB8
  (let ((dig  (ironclad:make-digest (select-sha3-hash))))
    (dolist (buf bufs)
      (ironclad:update-digest dig buf))
    (values (ironclad:produce-digest dig)
            dig)))

(defun stretched-hash (nstretch &rest bufs)
  (multiple-value-bind (hash dig) (apply 'csha3-buffers bufs)
    (loop repeat nstretch do
          (ironclad:update-digest dig hash)
          (dolist (buf bufs)
            (ironclad:update-digest dig buf))
          (setf hash (ironclad:produce-digest dig)))
    hash))
|#
;; -----------------------------------------------

(defun mod-r (v)
  (mod v *ed-r*))

(defun hash-to-int (vec)
  (mod-r (ed-convert-lev-to-int vec)))

(defun add-mod-r (a b)
  (add-mod *ed-r* a b))

(defun sub-mod-r (a b)
  (sub-mod *ed-r* a b))

(defun mult-mod-r (a b)
  (mult-mod *ed-r* a b))

;; ---------------------------------------------------

#|
(defun top-random (limit)
  (random-between (ash limit -1) limit))

(defun hash-to-id (pt pkey)
  (hash-to-int
   (csha3-buffers (loenc:encode
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
|#

(defun ed-dsa (msg skey id)
  (let* ((h     (ed-convert-lev-to-int
                 (sha3-buffers
                  ;; full 512 bits
                  (ed-convert-int-to-lev skey))))
         (a     0)
         (bits  (byte (- *ed-nbits* 5) 3)))
    (setf (ldb bits a) (ldb bits h)
          (ldb (byte 1 (1- *ed-nbits*)) a) 1)
    (let* ((msg-enc   (loenc:encode msg))
           (pkey      (ed-nth-pt a))
           (pkey-cmpr (ed-compress-pt pkey))
           (r         (ed-convert-lev-to-int
                       (sha3-buffers
                        (ed-convert-int-to-lev (ldb (byte *ed-nbits* *ed-nbits*) h))
                        msg-enc)))
           (rpt       (ed-nth-pt r))
           (rpt-cmpr  (ed-compress-pt rpt))
           (s         (add-mod-r r
                                 (mult-mod-r a
                                             (ed-convert-lev-to-int
                                              (sha3-buffers
                                               (ed-convert-int-to-lev rpt-cmpr)
                                               (ed-convert-int-to-lev pkey-cmpr)
                                               msg-enc))
                                             ))))
      (list
       :msg   msg
       :id    id
       :pkey  pkey-cmpr
       :r     rpt-cmpr
       :s     s)
      )))

(defun ed-dsa-validate (msg pkey r s)
  (ed-pt=
   (ed-nth-pt s)
   (ed-add (ed-decompress-pt r)
           (ed-mul (ed-decompress-pt pkey)
                   (ed-convert-lev-to-int
                    (sha3-buffers
                     (ed-convert-int-to-lev r)
                     (ed-convert-int-to-lev pkey)
                     (loenc:encode msg)))
                   ))))

(defun make-keypair (seed)
  (let* ((skey  (ldb (byte (1+ *ed-nbits*) 0)
                     (ed-convert-lev-to-int
                      (sha3-buffers (loenc:encode seed)))))
         (plist (ed-dsa nil skey nil))
         (pkey  (getf plist :pkey))
         (id    (getf plist :s))
         (chk   (getf plist :r)))
    (list
     :skey skey
     :pkey pkey
     :id   id
     :chk  chk)))

(defun validate-pkey (id pkey chk)
  (ed-dsa-validate nil pkey chk id))
        
;; ------------------------------------------------------

