;; pbc.lisp -- PBC (Pairing Based Crypto) in Lisp
;;
;; DM/Emotiq 03/18
;; ---------------------------------------------------------
#|
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

(defpackage :pbc-interface
  (:use :common-lisp
        :core-crypto)
  (:import-from :ecc-crypto-b571
   :encode-bytes-to-base58
   :decode-bytes-from-base58)
  (:export
   :init-pairing
   :set-generator
   
   :make-key-pair
   :set-secret-key
   :set-public-key

   :sign-message
   :check-message
   ))

(in-package :pbc-interface)

;; -----------------------------------------------------------------------

(fli:disconnect-module :pbclib
                       :remove t)

(fli:register-module :pbclib
                     :dlopen-flags t
                     :real-name  "/usr/local/lib64/libLispPBCIntf.dylib")

;; -----------------------------------------------------------------------
;; for initial test of strings transfer to/from C/Lisp

(fli:define-foreign-function (_echo "echo" :source)
    ((nel     :long)
     (msg     (:pointer (:unsigned :char)))
     (ret-msg (:pointer (:unsigned :char))))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(defun echo ()
  (let ((msg "Hello Dave!"))
    (fli:with-dynamic-foreign-objects ()
      (let ((buf (fli:allocate-dynamic-foreign-object
                  :type '(:unsigned :char) :nelems 1024)))
        (let ((nel  (_echo (length msg)
                           (fli:convert-to-dynamic-foreign-string
                            msg
                            :external-format :ASCII)
                           buf)))
          (list nel (subseq (fli:convert-from-foreign-string
                             buf
                             :external-format :ASCII)
                            0 nel))
          )))))

;; -------------------------------------------------
;; Init interface - this must be performed first

(fli:define-foreign-function (_init-pairing "init_pairing" :source)
    ((param-text  (:pointer (:unsigned :char)))
     (ntext       :long))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

;; -------------------------------------------------
;; Query interface

(fli:define-foreign-function (_get-g "get_g" :source)
    ((pbuf        (:pointer :void))
     (pnel        (:pointer :long)))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(fli:define-foreign-function (_get-h "get_h" :source)
    ((pbuf        (:pointer :void))
     (pnel        (:pointer :long)))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

;; -------------------------------------------------
;; Setter interface

(fli:define-foreign-function (_set-g "set_g" :source)
    ((pbuf   (:pointer (:unsigned :char))))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(fli:define-foreign-function (_set-h "set_h" :source)
    ((pbuf   (:pointer (:unsigned :char))))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

;; -------------------------------------------------
;; Keying interface

(fli:define-foreign-function (_make-key-pair "make_key_pair" :source)
    ((pbuf   (:pointer (:unsigned :char)))
     (nel    :long))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_get-secret-key "get_secret_key" :source)
    ((pbuf        (:pointer :void))
     (pnel        (:pointer :long)))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(fli:define-foreign-function (_get-public-key "get_public_key" :source)
    ((pbuf        (:pointer :void))
     (pnel        (:pointer :long)))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(fli:define-foreign-function (_set-secret-key "set_secret_key" :source)
    ((sbuf  (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_set-public-key "set_public_key" :source)
    ((pbuf  (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

;; -------------------------------------------------
;; BLS Signatures

(fli:define-foreign-function (_sign-hash "sign_hash" :source)
    ((pbuf   (:pointer (:unsigned :char)))
     (nel    :long))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_get-signature "get_signature" :source)
    ((pbuf        (:pointer :void))
     (pnel        (:pointer :long)))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(fli:define-foreign-function (_check-signature "check_signature" :source)
    ((psig   (:pointer (:unsigned :char)))
     (phash  (:pointer (:unsigned :char)))
     (nhash  :long)
     (pkey   (:pointer (:unsigned :char))))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

;; -------------------------------------------------

(fli:define-foreign-function (_mul-g1-pts "mul_G1_pts" :source)
    ((psum  (:pointer (:unsigned :char)))
     (p1    (:pointer (:unsigned :char)))
     (p2    (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_mul-g2-pts "mul_G2_pts" :source)
    ((psum  (:pointer (:unsigned :char)))
     (p1    (:pointer (:unsigned :char)))
     (p2    (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_add-zr-vals "add_Zr_vals" :source)
    ((zsum  (:pointer (:unsigned :char)))
     (z1    (:pointer (:unsigned :char)))
     (z2    (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

;; -------------------------------------------------

(defstruct curve-params
  pairing-text
  gen)

(defvar *curve-fr256-params*
  (make-curve-params
   :pairing-text
   #>.end
type f
q 16283262548997601220198008118239886027035269286659395419233331082106632227801
r 16283262548997601220198008118239886026907663399064043451383740756301306087801
b 10476541659213232777352255224319706265440471807344192411073251777589416636392
beta 2588849289436542488537732220497504302700946308066126767616133606209888506551
alpha0 15760619495780482509052463852330180194970833782280957391784969704642983647946
alpha1 3001017353864017826546717979647202832842709824816594729108687826591920660735
.end
   :gen  "13RiMW9z5MYdCDHqMecQ6YJJoHChzhgBpuhWZrtaZuWRSN4oEbB2UGYDfPWUyCV7qZJradJojpCoZR7YFH6RESP5"))

(defparameter *G1-size* nil) ;; G1 corresponds to the h curve
(defparameter *G2-size* nil) ;; G2 corresponds to the g curve for keying
(defparameter *Zr-size* nil) ;; Zr corresponds to the secret-key

(defparameter *pairing-init*  nil) ;; t if we have done init-pairing
(defparameter *g2-init*       nil) ;; t if we have done set-generator
(defparameter *zr-init*       nil) ;; t if we have done set-secret-key

;; -------------------------------------------------

(defun init-pairing (&optional (params *curve-fr256-params*))
  (fli:with-dynamic-foreign-objects ()
    (setf *G1-size* nil
          *G2-size* nil
          *Zr-size* nil
          *G2-init* nil
          *Zr-init* nil)
    (um:bind* ((:struct-accessors curve-params ((txt pairing-text)
                                                (gen gen)) params)
               (ntxt (length txt)))
      (assert (zerop (_init-pairing (fli:convert-to-dynamic-foreign-string
                                     txt
                                     :external-format :ASCII)
                                    ntxt)))
      (setf *pairing-init* t)
      (get-secret-key) ;; Zr - fill in the field sizes
      (get-g)  ;; G2
      (get-h)  ;; G1 (always the shorter)
      (when gen
        (set-generator gen))
      (values))))

;; -------------------------------------------------

(defun xfer-foreign-to-lisp (fbuf nel)
  (let ((lbuf (make-array nel
                          :element-type '(unsigned-byte 8))))
    (loop for ix from 0 below nel do
          (setf (aref lbuf ix) (fli:dereference fbuf :index ix)))
    (encode-bytes-to-base58 lbuf)))

(defmethod decode-bytes (bytes nb)
  bytes)

(defmethod decode-bytes ((bytes string) nb)
  (decode-bytes-from-base58 bytes nb))

(defun make-fli-buffer (nb &optional initial-contents)
  ;; this must only be called from inside of a WITH-DYNAMIC-FOREIGN-OBJECTS
  (let ((bytes (decode-bytes initial-contents nb)))
    (fli:allocate-dynamic-foreign-object
     :type   '(:unsigned :char)
     :nelems nb
     :initial-contents (coerce bytes 'list))))

(defmacro with-fli-buffers (buffers &body body)
  `(fli:with-dynamic-foreign-objects ()
     (let ,(mapcar #`(,(first a1) (make-fli-buffer ,(second a1) ,(third a1))) buffers)
       ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-fli-buffers" 1)

;; -------------------------------------------------
;; GET-ELEMENT -- read a (possibly compressed) element from the C
;; layer.  On first attempt, we generally don't know the size
;; involved, so a first call is made with a null buffer pointer. The C
;; world returns the actual neededd lenght to us, and stores that into
;; the :LONG count buffer. Then we proceed as normal.

(defun get-element (nb-sym get-fn)
  (assert *pairing-init*)
  (fli:with-dynamic-foreign-objects ()
    (let* ((nb (symbol-value nb-sym))
           (lenbuf (fli:allocate-dynamic-foreign-object
                    :type :long)))
      (if nb
          (let ((buf (make-fli-buffer nb)))
            (setf (fli:dereference lenbuf) nb)
            (assert (eql nb (funcall get-fn buf lenbuf)))
            (xfer-foreign-to-lisp buf nb))
        ;; else - size unknown
        (progn
          (setf nb
                (setf (symbol-value nb-sym)
                      (funcall get-fn fli:*null-pointer* lenbuf)))
          (let ((buf (make-fli-buffer nb)))
            (assert (eql nb (funcall get-fn buf lenbuf)))
            (xfer-foreign-to-lisp buf nb)))
        ))))
              
;; -------------------------------------------------

(defun get-g ()
  (get-element '*g2-size* '_get-g))

(defun get-h ()
  (get-element '*g1-size* '_get-h))

(defun get-signature ()
  (get-element '*g1-size* '_get-signature))

(defun get-public-key ()
  (get-element '*g2-size* '_get-public-key))

(defun get-secret-key ()
  (get-element '*zr-size* '_get-secret-key))

;; -------------------------------------------------

(defun set-element (bytes set-fn nb)
  (with-fli-buffers ((buf nb bytes))
    (funcall set-fn buf)))

(defun set-generator (g-bytes)
  (assert *pairing-init*)
  (set-element g-bytes '_set-g *g2-size*)
  (setf *g2-init* t))

(defun set-h (h-bytes)
  (assert *pairing-init*)
  (set-element h-bytes '_set-h *g1-size*))

(defun set-public-key (pkey-bytes)
  (assert *pairing-init*)
  (set-element pkey-bytes '_set-public-key *g2-size*))

(defun set-secret-key (skey-bytes)
  (assert *pairing-init*)
  (assert *g2-init*)
  (set-element skey-bytes '_set-secret-key *zr-size*)
  (setf *zr-init* t
        *g2-init* t))

;; -------------------------------------------------

(defun make-key-pair (seed)
  (assert *pairing-init*)
  (assert *g2-init*)
  (let* ((hsh  (sha3/256-buffers seed))
         (hlen (length hsh)))
    (with-fli-buffers ((hbuf hlen hsh))
      (_make-key-pair hbuf hlen)
      (setf *zr-init* t
            *g2-init* t)
      (list (get-secret-key)
            (get-public-key))
      )))

(defun sign-hash (hash-bytes)
  (assert *pairing-init*)
  (assert *zr-init*)
  (let ((nhash (length hash-bytes)))
    (with-fli-buffers ((hbuf nhash hash-bytes))
      (_sign-hash hbuf nhash)
      (get-signature)
      )))

(defun check-hash (hash-bytes sig-bytes pkey-bytes)
  (assert *pairing-init*)
  (assert *g2-init*)
  (let ((nhash  (length hash-bytes)))
    (with-fli-buffers ((sbuf *g1-size* sig-bytes)
                       (hbuf nhash     hash-bytes)
                       (pbuf *g2-size* pkey-bytes))
      (zerop (_check-signature sbuf hbuf nhash pbuf))
      )))

;; --------------------------------------------------------------
;; BLS Signatures on Messages - result is a triple (MSG, SIG, PKEY)

(defun sign-message (msg)
  (list msg
        (sign-hash (sha3/256-buffers msg))
        (get-public-key)))

(defun check-message (msg-list)
  (destructuring-bind (msg sig-bytes pkey-bytes) msg-list
    (check-hash (sha3/256-buffers msg)
                sig-bytes
                pkey-bytes)))

;; --------------------------------------------------------
;; Curve field operations -- to match academic papers, we utilize the
;; "bent" nomenclature where curve point addition is denoted by field
;; multiplication, curve point scalar multiplication is denoted as
;; field exponentiation.

(defun mul-g1-pts (pt1 pt2)
  ;; multiply two elements from G1 field (always the shorter field rep)
  (assert *pairing-init*)
  (with-fli-buffers ((p1-buf   *g1-size* pt1)
                     (p2-buf   *g1-size* pt2)
                     (psum-buf *g1-size*))
    (_mul-g1-pts psum-buf p1-buf p2-buf)
    (xfer-foreign-to-lisp psum-buf *g1-size*)
    ))

(defun mul-g2-pts (pt1 pt2)
  ;; multiply two elements from G2 field
  (assert *pairing-init*)
  (with-fli-buffers ((p1-buf   *g2-size* pt1)
                     (p2-buf   *g2-size* pt2)
                     (psum-buf *g2-size*))
    (_mul-g2-pts psum-buf p1-buf p2-buf)
    (xfer-foreign-to-lisp psum-buf *g2-size*)
    ))

(defun add-zr-vals (z1 z2)
  ;; add two elements from Zr ring
  (assert *pairing-init*)
  (with-fli-buffers ((z1-buf   *zr-size* z1)
                     (z2-buf   *zr-size* z2)
                     (zsum-buf *zr-size*))
    (_add-zr-vals zsum-buf z1-buf z2-buf)
    (xfer-foreign-to-lisp zsum-buf *zr-size*)
    ))

;; --------------------------------------------------------
;; BLS MultiSignatures

(defun combine-signatures (msg-list1 msg-list2)
  ;; BLS multi-signature is the product of the G1 and G2 elements
  ;; between them
  (destructuring-bind (msg1 sig1 pkey1) msg-list1
    (destructuring-bind (msg2 sig2 pkey2) msg-list2
      ;; no point combining signatures unless the message was the
      ;; same for both...
      (assert (equalp (sha3/256-buffers msg1)
                      (sha3/256-buffers msg2)))
      (list msg1
        (mul-g1-pts sig1  sig2)
        (mul-g2-pts pkey1 pkey2))
      )))

;; --------------------------------------------------------
(init-pairing)
;; --------------------------------------------------------
#|
(init-pairing)

;; check BLS Signatures
(destructuring-bind (skey pkey) (make-key-pair :dave)
  (let* ((msg  :hello-dave)
         (hash (sha3/256-buffers msg)))
    (sign-hash hash)
    (let* ((sig (get-signature)))
      (check-hash hash sig pkey))))

;; verify multi-sig BLS -- YES!!
(destructuring-bind (skey1 pkey1) (make-key-pair :key1)
  (let* ((msg    :this-is-a-test)
         (smsg1  (sign-message msg)))
    (assert (check-message smsg1))
    (destructuring-bind (skey2 pkey2) (make-key-pair :key2)
      (let ((smsg2 (sign-message msg)))
        (assert (check-message smsg2))
        (check-message (combine-signatures smsg1 smsg2))
        ))))

;; verify multi-sig BLS -- YES!!
(destructuring-bind (skey1 pkey1) (make-key-pair :key1)
  (let* ((msg    :this-is-a-test)
         (hsh    (sha3/256-buffers msg))
         (sig1   (sign-hash hsh)))
    (assert (check-hash hsh sig1 pkey1))
    (destructuring-bind (skey2 pkey2) (make-key-pair :key2)
      (let ((sig2 (sign-hash hsh)))
        (assert (check-hash hsh sig2 pkey2))
        (let ((sig   (mul-g1-pts sig1 sig2))
              (pkey  (mul-g2-pts pkey1 pkey2)))
          (check-hash hsh sig pkey)
          )))))

;; check that p1 + p2 = (s1 + s2)*g
(destructuring-bind (sk1 pk1) (make-key-pair :key1)
  (destructuring-bind (sk2 pk2) (make-key-pair :key2)
    (let* ((pksum (mul-g2-pts pk1 pk2))
           (zsum  (add-zr-vals sk1 sk2)))
      (set-secret-key zsum)
      (let ((pk (get-public-key)))
        (list pksum pk)))))

;; check that sig(sum) = sum(sig)
(destructuring-bind (sk1 pk1) (make-key-pair :key1)
  (let* ((msg  :testing)
         (hsh  (sha3/256-buffers msg))
         (sig1 (sign-hash hsh)))
  (destructuring-bind (sk2 pk2) (make-key-pair :key2)
    (let* ((pksum (mul-g2-pts pk1 pk2))
           (zsum  (add-zr-vals sk1 sk2))
           (sig2  (sign-hash hsh))
           (ssum  (mul-g1-pts sig1 sig2)))
      (check-hash hsh ssum pksum)
      (set-secret-key zsum)
      (list (sign-hash hsh) ssum)
      ))))
|#
