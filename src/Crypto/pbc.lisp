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
  (:nicknames :pbc)
  (:import-from :ecc-crypto-b571
   :encode-bytes-to-base58
   :decode-bytes-from-base58
   :convert-lev-to-int
   :convert-int-to-lev)
  (:export
   :init-pairing
   
   :make-key-pair
   :check-public-key

   :set-secret-key
   :set-public-key

   :make-public-subkey
   :make-secret-subkey
   :ibe-encrypt
   :ibe-decrypt
   
   :hash
   :sign-message       ;; BLS Sigs
   :check-message
   :combine-signatures ;; for BLS MultiSigs

   :with-crypto
   ))

(in-package :pbc-interface)

;; -----------------------------------------------------------------------

(fli:disconnect-module :pbclib
                       :remove t)

(fli:register-module :pbclib
                     :dlopen-flags t
                     :real-name
                     #+:MACOSX "/usr/local/lib64/libLispPBCIntf.dylib"
                     #+:LINUX  "/usr/local/lib/libLispPBCIntf.so"
                     )

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

(fli:define-foreign-function (_get-g2 "get_g2" :source)
    ((pbuf        (:pointer :void))
     (nbuf        :long))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(fli:define-foreign-function (_get-g1 "get_g1" :source)
    ((pbuf        (:pointer :void))
     (nbuf        :long))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(fli:define-foreign-function (_get-pairing "get_pairing" :source)
    ((pbuf        (:pointer :void))
     (nbuf        :long))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

;; -------------------------------------------------
;; Setter interface

(fli:define-foreign-function (_set-g2 "set_g2" :source)
    ((pbuf   (:pointer (:unsigned :char))))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(fli:define-foreign-function (_set-g1 "set_g1" :source)
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

(fli:define-foreign-function (_make-public-subkey "make_public_subkey" :source)
    ((abuf   (:pointer (:unsigned :char)))
     (pbuf   (:pointer (:unsigned :char)))
     (hbuf   (:pointer (:unsigned :char)))
     (hlen   :long))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_make-secret-subkey "make_secret_subkey" :source)
    ((abuf   (:pointer (:unsigned :char)))
     (sbuf   (:pointer (:unsigned :char)))
     (hbuf   (:pointer (:unsigned :char)))
     (hlen   :long))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_sakai-kasahara-encrypt "sakai_kasahara_encrypt" :source)
    ((rbuf   (:pointer (:unsigned :char))) ;; returned R point in G2
     (pbuf   (:pointer (:unsigned :char))) ;; returned pairing for encryption in GT
     (pkey   (:pointer (:unsigned :char))) ;; public subkey
     (hbuf   (:pointer (:unsigned :char))) ;; hash(ID, msg)
     (hlen   :long))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_sakai-kasahara-decrypt "sakai_kasahara_decrypt" :source)
    ((pbuf   (:pointer (:unsigned :char)))  ;; returned pairing for decryptin in GT
     (rbuf   (:pointer (:unsigned :char)))  ;; R point in G2
     (skey   (:pointer (:unsigned :char)))) ;; secret subkey in G1
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_sakai-kasahara-check "sakai_kasahara_check" :source)
    ((rbuf   (:pointer (:unsigned :char)))  ;; R point in G2
     (pkey   (:pointer (:unsigned :char)))  ;; public subkey in G2
     (hbuf   (:pointer (:unsigned :char)))  ;; hash(ID, msg)
     (hlen   :long))
  :result-type :long
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_get-secret-key "get_secret_key" :source)
    ((pbuf        (:pointer :void))
     (nbuf        :long))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(fli:define-foreign-function (_get-public-key "get_public_key" :source)
    ((pbuf        (:pointer :void))
     (nbuf        :long))
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
     (nbuf   :long))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_get-signature "get_signature" :source)
    ((pbuf        (:pointer :void))
     (nbuf        :long))
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

(fli:define-foreign-function (_inv-zr-val "inv_Zr_val" :source)
    ((zinv  (:pointer (:unsigned :char)))
     (z     (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_exp-G1z "exp_G1z" :source)
    ((gexp  (:pointer (:unsigned :char)))
     (g     (:pointer (:unsigned :char)))
     (z     (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_exp-G2z "exp_G2z" :source)
    ((gexp  (:pointer (:unsigned :char)))
     (g     (:pointer (:unsigned :char)))
     (z     (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_compute-pairing "compute_pairing" :source)
    ((hbuf  (:pointer (:unsigned :char)))
     (gbuf  (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

;; -------------------------------------------------

(defstruct curve-params
  pairing-text
  g1 g2)

;; from: genfparam 256
(defparameter *curve-fr256-params*
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
   :g1  "k4D43rUYfy8Jb5svA84MDPBqGopzEnY3p5hWftjb4ajH"
   :g2  "eHkMQzBYHLpN2qtj12c77Ka8F7cJA5eTVgxYiou2dwxzJwb9yGs8HDAKUvF5CNUtH4Vx6ir2gVv4u49qoKhPjjsf2"
   ))

(defparameter *curve-default-ar160-params*
  (make-curve-params
   :pairing-text
   #>.end
type a
q 8780710799663312522437781984754049815806883199414208211028653399266475630880222957078625179422662221423155858769582317459277713367317481324925129998224791
h 12016012264891146079388821366740534204802954401251311822919615131047207289359704531102844802183906537786776
r 730750818665451621361119245571504901405976559617
exp2 159
exp1 107
sign1 1
sign0 1
.end
   :g1  "XvHFWryy2nHDz1uGzXUC1Yh4iHRMqVXxmi6an9KEg3TzXc6Bu4EjapeiAEgZYdKTCxPCYm7ibXBVkHL1HuxxxaVwD"
   :g2  "HbeSBqyJxwNfsHEuneihwwKiKErSz9TwqrxnUFn6m7ky6mtnLLifFPdGjGYwcC9gPTpSspf7R5Nm6EjyNiacukBC"
   ))

(defparameter *curve*    nil)
(defparameter *G1-size*  nil) ;; G1 corresponds to the h curve
(defparameter *G2-size*  nil) ;; G2 corresponds to the g curve for keying
(defparameter *Zr-size*  nil) ;; Zr corresponds to the secret-key
(defparameter *GT-size*  nil) ;; GT corresponds to the pairings

(defparameter *g2-init*  nil) ;; t if we have done set-generator
(defparameter *zr-init*  nil) ;; t if we have done set-secret-key

;; -------------------------------------------------

(defun need-pairing ()
  (unless *curve*
    (error "Pairing not initialized. Use INIT-PAIRING.")))

(defun need-keying ()
  (need-pairing)
  (unless *zr-init*
    (error "Keying not established. Use SET-SECRET-KEY.")))

(defun need-generator ()
  (need-pairing)
  (unless *g2-init*
    (error "Generator not established. Use SET-GENERATOR.")))

;; -------------------------------------------------

(defun init-pairing (&optional (params *curve-fr256-params*))
  (fli:with-dynamic-foreign-objects ()
    (setf *G1-size* nil
          *G2-size* nil
          *Zr-size* nil
          *GT-size* nil
          
          *G2-init* nil
          *Zr-init* nil
          *curve*   nil)
    (um:bind* ((:struct-accessors curve-params ((txt pairing-text)
                                                (g1  g1)
                                                (g2  g2)) params)
               (ntxt (length txt)))
      (assert (zerop (_init-pairing (fli:convert-to-dynamic-foreign-string
                                     txt
                                     :external-format :ASCII)
                                    ntxt)))
      (setf *curve* params)
      (get-secret-key) ;; Zr - fill in the field sizes
      (get-g1)
      (get-g2)
      (if g1
          (set-g1 g1)
        (setf (curve-params-g1 params) (get-g1)))
      (if g2
          (set-g2 g2)
        (setf (curve-params-g2 params) (get-g2)))
      (values))))

;; -------------------------------------------------
;; PBC lib expects all values as big-endian
;; We work internally with little-endian values

(defun encode-bytes (bytes)
  ;; make little endian base58 string from big-endian vector
  (encode-bytes-to-base58 (reverse bytes)))

(defun xfer-foreign-to-lisp (fbuf nel)
  (let ((lbuf (make-array nel
                          :element-type '(unsigned-byte 8))))
    (loop for ix from 0 below nel do
          (setf (aref lbuf ix) (fli:dereference fbuf :index ix)))
    (encode-bytes lbuf)))

(defmethod decode-bytes (bytes nb)
  ;; lib expects big-endian vectors
  (reverse bytes))

(defmethod decode-bytes ((bytes integer) nb)
  ;; produce big-endian vector from integer
  (convert-int-to-nbytesv bytes nb))

(defmethod decode-bytes ((bytes string) nb)
  ;; decode little-endian base58 strings to big-endian vectors of UB8
  (reverse (decode-bytes-from-base58 bytes nb)))

(defun make-fli-buffer (nb &optional initial-contents)
  ;; this must only be called from inside of a WITH-DYNAMIC-FOREIGN-OBJECTS
  ;; initial-contents should be in little-endian order
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
  (need-pairing)
  (let ((nb (symbol-value nb-sym)))
    (unless nb
      ;; size unknown - query first
      (setf nb
            (setf (symbol-value nb-sym)
                  (funcall get-fn fli:*null-pointer* 0))))
    (with-fli-buffers ((buf nb))
      (assert (eql nb (funcall get-fn buf nb)))
      (xfer-foreign-to-lisp buf nb))
    ))
              
;; -------------------------------------------------

(defun get-g2 ()
  (get-element '*g2-size* '_get-g2))

(defun get-g1 ()
  (get-element '*g1-size* '_get-g1))

(defun get-signature ()
  (get-element '*g1-size* '_get-signature))

(defun get-public-key ()
  (get-element '*g2-size* '_get-public-key))

(defun get-secret-key ()
  (get-element '*zr-size* '_get-secret-key))

(defun get-pairing ()
  (get-element '*gt-size* '_get-pairing))

(defun get-order ()
  (let ((txt (curve-params-pairing-text *curve*)))
    (read-from-string txt t nil
                      :start (+ (search "r " txt
                                        :test 'string-equal)
                                2))))

;; -------------------------------------------------

(defun set-element (bytes set-fn nb)
  (need-pairing)
  (with-fli-buffers ((buf nb bytes))
    (funcall set-fn buf)))

(defun set-g1 (g1-bytes)
  (set-element g1-bytes '_set-g1 *g1-size*))

(defun set-g2 (g2-bytes)
  (set-element g2-bytes '_set-g2 *g2-size*))

(defun set-generator (g-bytes)
  (set-element g-bytes '_set-g *g2-size*)
  (setf *g2-init* t))

(defun set-public-key (pkey-bytes)
  (set-element pkey-bytes '_set-public-key *g2-size*))

(defun set-secret-key (skey-bytes)
  (need-generator)
  (set-element skey-bytes '_set-secret-key *zr-size*)
  (setf *zr-init* t))

;; -------------------------------------------------

(defun hash (&rest args)
  ;; produce a UB8V of the args
  (let ((hv  (apply 'sha3/256-buffers args)))
    (values (reverse hv) (length hv))))
        
(defun sign-hash (hash-bytes)
  ;; hash-bytes is UB8V
  (need-keying)
  (let ((nhash (length hash-bytes)))
    (with-fli-buffers ((hbuf nhash hash-bytes))
      (_sign-hash hbuf nhash)
      (get-signature)
      )))

(defun check-hash (hash-bytes sig-bytes pkey-bytes)
  ;; hash-bytes is UB8V
  (need-generator)
  (let ((nhash  (length hash-bytes)))
    (with-fli-buffers ((sbuf *g1-size* sig-bytes)
                       (hbuf nhash     hash-bytes)
                       (pbuf *g2-size* pkey-bytes))
      (zerop (_check-signature sbuf hbuf nhash pbuf))
      )))

;; --------------------------------------------------------------
;; BLS Signatures on Messages - result is a triple (MSG, SIG, PKEY)

(defun sign-message (msg)
  (list :msg  msg                     ;; original message
        :sig  (sign-hash (hash msg))  ;; signature
        :pkey (get-public-key)))      ;; public key of signature

(defun get-fields (lst keys)
  (mapcar (um:curry 'getf lst) keys))

(defun check-message (msg-list)
  (destructuring-bind (msg sig-bytes pkey-bytes)
      (get-fields msg-list '(:msg :sig :pkey))
    (check-hash (hash msg)
                sig-bytes
                pkey-bytes)))

;; --------------------------------------------------------------
;; Keying - generate secret and authenticated public keys

(defun make-key-pair (seed)
  ;; seed can be literally anything at all...
  (need-generator)
  (multiple-value-bind (hsh hlen) (hash seed)
    (with-fli-buffers ((hbuf hlen hsh))
      (_make-key-pair hbuf hlen)
      (setf *zr-init* t)
      (let* ((pkey (get-public-key)) ;; public key
             (skey (get-secret-key)) ;; secret key
             (sig  (sign-hash (hash pkey)))) ;; signature on public key
        ;; return 3 values: public key, signature on public key, secret key
        (values pkey sig skey))
      )))

(defun check-public-key (pkey psig)
  (check-hash (hash pkey)
              psig
              pkey))

;; -----------------------------------------------------------------------
;; Sakai-Haskara Encryption

(defun make-public-subkey (pkey seed)
  (need-generator)
  (multiple-value-bind (hsh hlen) (hash seed)
    (with-fli-buffers ((hbuf hlen      hsh)
                       (pbuf *g2-size* pkey)
                       (abuf *g2-size*))
      (_make-public-subkey abuf pbuf hbuf hlen)
      (xfer-foreign-to-lisp abuf *g2-size*))))

(defun make-secret-subkey (skey seed)
  (need-generator)
  (multiple-value-bind (hsh hlen) (hash seed)
    (with-fli-buffers ((hbuf hlen      hsh)
                       (sbuf *zr-size* skey)
                       (abuf *g1-size*))
      (_make-secret-subkey abuf sbuf hbuf hlen)
      (xfer-foreign-to-lisp abuf *g1-size*))))

;; --------------------------------------------------------------

(defun ibe-encrypt (msg pkey id)
  ;; msg should be hash-sized vector of UB8
  ;; values are R and CryptoText, both in BASE58 encoding
  (multiple-value-bind (rhsh hlen) (hash id msg)
    (let ((pkid (make-public-subkey pkey id)))
      (with-fli-buffers ((hbuf  hlen       rhsh)
                         (pbuf  *gt-size*)
                         (kbuf  *g2-size*  pkid)
                         (rbuf  *g2-size*))
        (_sakai-kasahara-encrypt rbuf pbuf kbuf hbuf hlen)
        (let* ((pval (hash (xfer-foreign-to-lisp pbuf *gt-size*)))
               (cmsg (encode-bytes-to-base58 (map 'vector 'logxor pval msg))))
        (values (xfer-foreign-to-lisp rbuf *g2-size*) ;; R
                cmsg) ;; crypto-text
        )))))
                             
(defun ibe-decrypt (rval cmsg skey pkey id)
  ;; rval is base58 R value provided in crypto message pair
  ;; cmsg should be base58 encoded hash-sized vector of UB8
  (let ((skid (make-secret-subkey skey id))
        (pkey (make-public-subkey pkey id)))
    (with-fli-buffers ((pbuf  *gt-size*)
                       (kbuf  *g1-size*  skid)
                       (rbuf  *g2-size*  rval))
      (_sakai-kasahara-decrypt pbuf rbuf kbuf)
      (let* ((pval (hash (xfer-foreign-to-lisp pbuf *gt-size*)))
             (msg  (map 'vector 'logxor pval (decode-bytes-from-base58 cmsg))))
        (multiple-value-bind (hval hlen) (hash id msg)
          (with-fli-buffers ((hbuf hlen      hval)
                             (kbuf *g2-size* pkey))
            (when (zerop (_sakai-kasahara-check rbuf kbuf hbuf hlen))
              msg))
          )))))
                             
(defun compute-pairing (hval gval)
  (need-pairing)
  (with-fli-buffers ((hbuf  *g1-size*  hval)
                     (gbuf  *g2-size*  gval))
    (_compute-pairing hbuf gbuf)
    (get-pairing)))

;; --------------------------------------------------------
;; Curve field operations -- to match academic papers, we utilize the
;; "bent" nomenclature where curve point addition is denoted by field
;; multiplication, curve point scalar multiplication is denoted as
;; field exponentiation.

(defun mul-g1-pts (pt1 pt2)
  ;; multiply two elements from G1 field (always the shorter field rep)
  (need-pairing)
  (with-fli-buffers ((p1-buf   *g1-size* pt1)
                     (p2-buf   *g1-size* pt2)
                     (psum-buf *g1-size*))
    (_mul-g1-pts psum-buf p1-buf p2-buf)
    (xfer-foreign-to-lisp psum-buf *g1-size*)
    ))

(defun mul-g2-pts (pt1 pt2)
  ;; multiply two elements from G2 field
  (need-pairing)
  (with-fli-buffers ((p1-buf   *g2-size* pt1)
                     (p2-buf   *g2-size* pt2)
                     (psum-buf *g2-size*))
    (_mul-g2-pts psum-buf p1-buf p2-buf)
    (xfer-foreign-to-lisp psum-buf *g2-size*)
    ))

(defun add-zr-vals (z1 z2)
  ;; add two elements from Zr ring
  (need-pairing)
  (with-fli-buffers ((z1-buf   *zr-size* z1)
                     (z2-buf   *zr-size* z2)
                     (zsum-buf *zr-size*))
    (_add-zr-vals zsum-buf z1-buf z2-buf)
    (xfer-foreign-to-lisp zsum-buf *zr-size*)
    ))

(defun inv-zr-val (z)
  ;; compute inverse of z over Zr
  (need-pairing)
  (with-fli-buffers ((zinv-buf  *zr-size*)
                     (z-buf     *zr-size* z))
    (_inv-zr-val zinv-buf z-buf)
    (xfer-foreign-to-lisp zinv-buf *zr-size*)))

(defun exp-G1z (g1 z)
  ;; exponentiate an element of G1 by element z of ring Zr
  (need-pairing)
  (with-fli-buffers ((g1-buf   *g1-size* g1)
                     (z-buf    *zr-size* z)
                     (g1^z-buf *g1-size*))
    (_exp-G1z g1^z-buf g1-buf z-buf)
    (xfer-foreign-to-lisp g1^z-buf *g1-size*)
    ))

(defun exp-G2z (g2 z)
  ;; exponentiate an element of G2 by element z of ring Zr
  (need-pairing)
  (with-fli-buffers ((g2-buf   *g2-size* g2)
                     (z-buf    *zr-size* z)
                     (g2^z-buf *g2-size*))
    (_exp-G2z g2^z-buf g2-buf z-buf)
    (xfer-foreign-to-lisp g2^z-buf *g2-size*)
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
      (assert (equalp (hash msg1)
                      (hash msg2)))
      (list msg1
        (mul-g1-pts sig1  sig2)
        (mul-g2-pts pkey1 pkey2))
      )))

;; --------------------------------------------------------
;; (init-pairing *curve-default-ar160-params*)
(init-pairing)
;; --------------------------------------------------------
#|
(init-pairing)

;; check BLS Signatures
(multiple-value-bind (pkey psig skey) (make-key-pair :dave)
  (let* ((msg  :hello-dave)
         (hash (hash msg)))
    (sign-hash hash)
    (let* ((sig (get-signature)))
      (check-hash hash sig pkey))))

;; verify multi-sig BLS -- YES!!
(multiple-value-bind (pkey psig skey) (make-key-pair :key1)
  (let* ((msg    :this-is-a-test)
         (smsg1  (sign-message msg)))
    (assert (check-message smsg1))
    (multiple-value-bind (pkey2 psig skey2) (make-key-pair :key2)
      (let ((smsg2 (sign-message msg)))
        (assert (check-message smsg2))
        (check-message (combine-signatures smsg1 smsg2))
        ))))

;; verify multi-sig BLS -- YES!!
(multiple-value-bind (pkey1 psig skey1) (make-key-pair :key1)
  (let* ((msg    :this-is-a-test)
         (hsh    (hash msg))
         (sig1   (sign-hash hsh)))
    (assert (check-hash hsh sig1 pkey1))
    (multiple-value-bind (pkey2 psig skey2) (make-key-pair :key2)
      (let ((sig2 (sign-hash hsh)))
        (assert (check-hash hsh sig2 pkey2))
        (let ((sig   (mul-g1-pts sig1 sig2))
              (pkey  (mul-g2-pts pkey1 pkey2)))
          (check-hash hsh sig pkey)
          )))))

;; check that p1 + p2 = (s1 + s2)*g
(multiple-value-bind (pk1 psig sk1) (make-key-pair :key1)
  (multiple-value-bind (pk2 psig sk2) (make-key-pair :key2)
    (let* ((pksum (mul-g2-pts pk1 pk2))
           (zsum  (add-zr-vals sk1 sk2)))
      (set-secret-key zsum)
      (let ((pk (get-public-key)))
        (list pksum pk)))))

;; check that sig(sum) = sum(sig)
(multiple-value-bind (pk1 psig sk1) (make-key-pair :key1)
  (let* ((msg  :testing)
         (hsh  (hash msg))
         (sig1 (sign-hash hsh)))
  (multiple-value-bind (pk2 psig sk2) (make-key-pair :key2)
    (let* ((pksum (mul-g2-pts pk1 pk2))
           (zsum  (add-zr-vals sk1 sk2))
           (sig2  (sign-hash hsh))
           (ssum  (mul-g1-pts sig1 sig2)))
      (check-hash hsh ssum pksum)
      (set-secret-key zsum)
      (list (sign-hash hsh) ssum)
      ))))

|#

;; ------------------------------------------------------
;;
;; The PBC & GNU MP libs are possibly not thread-safe nor reentrant.
;; The PBC interface in LibLispPBCInterface is certainly not
;; reentrant, as it depends on C global values. And it is not
;; thread-safe since those globals could change from activities in
;; other threads.
;;
;; Hence we need a way to ensure that all use of it goes through a
;; single thread. Actors to the resuce...
;;
;; Note that because of activities requested by other client code, the
;; values of PBC elements may be arbitrary. If you need a known
;; environment, you should reestablish it on entry. This applies
;; in particular to the secret and public keying in effect.

(defvar *crypto-boss*
  (ac:make-actor
   (lambda (fn)
     (funcall fn))))

(defmacro with-crypto ((&key skey pkey) &body body)
  `(ac:ask *crypto-boss* (lambda ()
                            ,@(when skey
                                `(set-secret-key ,skey))
                            ,@(when pkey
                                `(set-public-key ,pkey))
                            ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-crypto" 1)

 
