;; pbc-cffi.lisp -- PBC (Pairing Based Crypto) in Lisp using CFFI
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

(asdf:load-system :cffi)

(defpackage :pbc-interface
  (:use :common-lisp
        :core-crypto
        :vec-repr
        :hash)
  (:nicknames :pbc)
  (:export
   ;; classes and their slot readers
   :crypto-val
   :crypto-val-vec
   :g1-cmpr
   :g1-cmpr-pt
   :g2-cmpr
   :g2-cmpr-pt
   :zr
   :zr-val
   :gt
   :gt-val
   :public-key
   :public-key-val
   :secret-key
   :secret-key-val
   :signature
   :signature-val
   :pairing
   :pairing-val
   :crypto-text
   :crypto-text-vec
   
   :init-pairing
   :set-generator  ;; 1 each for G1, and G2 groups
   
   :get-g1
   :get-g2
   :get-order
   
   :make-key-pair
   :check-public-key

   :make-public-subkey
   :make-secret-subkey
   :ibe-encrypt
   :ibe-decrypt
   
   :sign-message       ;; BLS Sigs
   :check-message
   :combine-signatures ;; for BLS MultiSigs

   :compute-pairing
   :mul-pts  ;; bent nomenclature for ECC
   :add-zrs
   :inv-zr
   :expt-pt-zr
   :add-pts  ;; non-bent nomenclature for ECC
   :mul-pt-zr
   
   :keying-triple
   :keying-triple-pkey
   :keying-triple-sig
   :keying-triple-skey
   
   :signed-message
   :signed-message-msg
   :signed-message-sig
   :signed-message-pkey

   :crypto-packet
   :crypto-packet-pkey
   :crypto-packet-id
   :crypto-packet-tstamp
   :crypto-packet-rval
   :crypto-packet-cmsg

   :g1-from-hash
   :g2-from-hash
   :zr-from-hash

   :compute-vrf
   :validate-vrf
   :vrf
   :vrf-seed
   :vrf-x
   :vrf-y
   :vrf-proof

   :make-pedersen-proof
   :validate-pedersen-proof
   :make-cloaked-proof
   :validate-cloaked-proof
   ))

(in-package :pbc-interface)

;; -----------------------------------------------------------------------

(cffi:define-foreign-library libpbc
 (:darwin #.(concatenate 'string 
		       (namestring (asdf:system-relative-pathname 'emotiq "../var/local/lib"))
		       "/libLispPBCIntf.dylib"))
 (:linux #.(concatenate 'string 
		     (namestring (asdf:system-relative-pathname 'emotiq "../var/local/lib"))
		     "/libLispPBCIntf.so"))
 (t (:default "libLispPBCIntf"))
 )

(cffi:use-foreign-library libpbc)

;; -----------------------------------------------------------------------
;; Init interface - this must be performed first

(cffi:defcfun ("init_pairing" _init-pairing) :long
  (param-text  :pointer :unsigned-char)
  (ntext       :long)
  (psizes      :pointer :long))

;; -------------------------------------------------
;; Query interface

(cffi:defcfun ("get_g2" _get-g2) :long
  (pbuf        :pointer :void)
  (nbuf        :long))

(cffi:defcfun ("get_g1" _get-g1) :long
  (pbuf        :pointer :void)
  (nbuf        :long))

;; -------------------------------------------------
;; Setter interface

(cffi:defcfun ("set_g2" _set-g2) :long
  (pbuf   :pointer :unsigned-char))

(cffi:defcfun ("set_g1" _set-g1) :long
  (pbuf   :pointer :unsigned-char))

;; -------------------------------------------------
;; Keying interface

(cffi:defcfun ("make_key_pair" _make-key-pair) :void
  (skbuf  :pointer :unsigned-char)
  (pkbuf  :pointer :unsigned-char)
  (hbuf   :pointer :unsigned-char)
  (nhash  :long))

(cffi:defcfun ("make_public_subkey" _make-public-subkey) :void
  (abuf   :pointer :unsigned-char)
  (pbuf   :pointer :unsigned-char)
  (hbuf   :pointer :unsigned-char)
  (hlen   :long))

(cffi:defcfun ("make_secret_subkey" _make-secret-subkey) :void
  (abuf   :pointer :unsigned-char)
  (sbuf   :pointer :unsigned-char)
  (hbuf   :pointer :unsigned-char)
  (hlen   :long))

(cffi:defcfun ("sakai_kasahara_encrypt" _sakai-kasahara-encrypt) :void
  ;; aka SAKKE, IETF RFC 6508
  (rbuf   :pointer :unsigned-char) ;; returned R point in G2
  (pbuf   :pointer :unsigned-char) ;; returned pairing for encryption in GT
  (pkey   :pointer :unsigned-char) ;; public subkey in G2
  (hbuf   :pointer :unsigned-char) ;; hash(ID, msg)
  (hlen   :long))

(cffi:defcfun ("sakai_kasahara_decrypt" _sakai-kasahara-decrypt) :void
  ;; aka SAKKE, IETF RFC 6508
  (pbuf   :pointer :unsigned-char)  ;; returned pairing for decryptin in GT
  (rbuf   :pointer :unsigned-char)  ;; R point in G2
  (skey   :pointer :unsigned-char)) ;; secret subkey in G1

(cffi:defcfun ("sakai_kasahara_check" _sakai-kasahara-check) :long
  ;; aka SAKKE, IETF RFC 6508
  (rbuf   :pointer :unsigned-char)  ;; R point in G2
  (pkey   :pointer :unsigned-char)  ;; public subkey in G2
  (hbuf   :pointer :unsigned-char)  ;; hash(ID, msg)
  (hlen   :long))

;; -------------------------------------------------
;; BLS Signatures

(cffi:defcfun ("sign_hash" _sign-hash) :void
  (sig    :pointer :unsigned-char)
  (skbuf  :pointer :unsigned-char)
  (pbuf   :pointer :unsigned-char)
  (nbuf   :long))

(cffi:defcfun ("check_signature" _check-signature) :long
  (psig   :pointer :unsigned-char)
  (phash  :pointer :unsigned-char)
  (nhash  :long)
  (pkey   :pointer :unsigned-char))

;; -------------------------------------------------

(cffi:defcfun ("mul_G1_pts" _mul-g1-pts) :void
  (p1    :pointer :unsigned-char)
  (p2    :pointer :unsigned-char))

(cffi:defcfun ("mul_G2_pts" _mul-g2-pts) :void
  (p1    :pointer :unsigned-char)
  (p2    :pointer :unsigned-char))

(cffi:defcfun ("add_Zr_vals" _add-zr-vals) :void
  (z1    :pointer :unsigned-char)
  (z2    :pointer :unsigned-char))

(cffi:defcfun ("inv_Zr_val" _inv-zr-val) :void
  (z     :pointer :unsigned-char))

(cffi:defcfun ("exp_G1z" _exp-G1z) :void
  (g     :pointer :unsigned-char)
  (z     :pointer :unsigned-char))

(cffi:defcfun ("exp_G2z" _exp-G2z) :void
  (g     :pointer :unsigned-char)
  (z     :pointer :unsigned-char))

(cffi:defcfun ("compute_pairing" _compute-pairing) :void
  (gtbuf :pointer :unsigned-char)  ;; result returned here
  (hbuf  :pointer :unsigned-char)
  (gbuf  :pointer :unsigned-char))

;; ----------------------------------------------------

(cffi:defcfun ("get_G1_from_hash" _get-g1-from-hash) :void
  (g1buf :pointer :unsigned-char)
  (hbuf  :pointer :unsigned-char)
  (nhash :long))

(cffi:defcfun ("get_G2_from_hash" _get-g2-from-hash) :void
  (g2buf :pointer :unsigned-char)
  (hbuf  :pointer :unsigned-char)
  (nhash :long))

(cffi:defcfun ("get_Zr_from_hash" _get-zr-from-hash) :void
  (zrbuf :pointer :unsigned-char)
  (hbuf  :pointer :unsigned-char)
  (nhash :long))

;; -------------------------------------------------
;; Abstract superclass for crypto objects. These are just wrappers
;; around UB8V objects. All subclasses share the same immutable slot.

(defclass crypto-val (ub8v-repr)
  ((val  :reader   crypto-val-vec
         :initarg  :value)))

(defmethod ub8v-repr ((x crypto-val))
  (crypto-val-vec x))

;; -------------------------------------------------
;; Useful subclasses

(defclass g1-cmpr (crypto-val)
  ((val :reader g1-cmpr-pt
       :initarg  :pt)
   ))

(defclass signature (g1-cmpr)
  ((val :reader signature-val
       :initarg  :val)))

(defclass g2-cmpr (crypto-val)
  ((val  :reader g2-cmpr-pt
        :initarg  :pt)
   ))

(defclass public-key (g2-cmpr)
  ((val  :reader public-key-val
        :initarg  :val)))

(defclass gt (crypto-val)
  ((val  :reader  gt-val
         :initarg   :val)))

(defclass pairing (gt)
  ((val  :reader pairing-val
         :initarg  :val)))

(defclass zr (crypto-val)
  ((val  :reader  zr-val
         :initarg   :val)))

(defclass secret-key (zr)
  ((val :reader secret-key-val
        :initarg  :val)))

(defclass crypto-text (crypto-val)
  ((val  :reader  crypto-text-vec
         :initarg :vec)))

;; -------------------------------------------------

(defstruct curve-params
  pairing-text
  g1 g2 order g1-len g2-len zr-len gt-len)

;; ---------------------------------------------------------------------------------------
;; from modified genfparam 256
(defparameter *curve-fr256-params*
  (make-curve-params
   :pairing-text
   #>.end
type f
q 115792089237314936872688561244471742058375878355761205198700409522629664518163
r 115792089237314936872688561244471742058035595988840268584488757999429535617037
b 3
beta 76600213043964638334639432839350561620586998450651561245322304548751832163977
alpha0 82889197335545133675228720470117632986673257748779594473736828145653330099944
alpha1 66367173116409392252217737940259038242793962715127129791931788032832987594232
.end
   :g1  (make-instance 'g1-cmpr
         :pt (bev
              (make-instance 'hex
                :str "ff8f256bbd48990e94d834fba52da377b4cab2d3e2a08b6828ba6631ad4d668500")))
   :g2  (make-instance 'g2-cmpr
         :pt (bev
              (make-instance 'hex
                :str "e20543135c81c67051dc263a2bc882b838da80b05f3e1d7efa420a51f5688995e0040a12a1737c80def47c1a16a2ecc811c226c17fb61f446f3da56c420f38cc01"))))

  "Curve parameters adapted to ensure q is as large as possible within
the constraints that q < 2^256, q and r prime, q = 3 mod 4, q = 4 mod
9, and b = 3. Bitlengths of q and r are the same. Easy cube roots and
square roots.  This curve will wrap 1 in 92 trillion hash/256.
Algorithm from 'Pairing-Friendly Elliptic Curves of Prime Order' by
Barreto and Naehrig")

;; ---------------------------------------------------------------------------------------
;; from modified genfparam 255
(defparameter *curve-fr255-params*
  (make-curve-params
   :pairing-text
   #>.end
type f
q 57896044618657242796275912003089040872670005837955836828594514493287093416899
r 57896044618657242796275912003089040872429389868787834093544968723639815668573
b 3
beta 55803917036574816430082368241718705273146220885597405985627421222965120451030
alpha0 36745065291682366075254502967669756043233951020839323524243711945517092183438
alpha1 48463065091351977226261302315306999962633968949407617433093635561838515781540
.end
#|
   :g1  (make-instance 'g1-cmpr
         :pt (bev
              (make-instance 'hex
                :str "ff8f256bbd48990e94d834fba52da377b4cab2d3e2a08b6828ba6631ad4d668500")))
   :g2  (make-instance 'g2-cmpr
         :pt (bev
              (make-instance 'hex
                             :str "e20543135c81c67051dc263a2bc882b838da80b05f3e1d7efa420a51f5688995e0040a12a1737c80def47c1a16a2ecc811c226c17fb61f446f3da56c420f38cc01")))
|#
)

  "Curve parameters adapted to ensure q is as large as possible within
the constraints that q < 2^255, q and r prime, q = 3 mod 4, q = 4 mod
9, and b = 3. Bitlengths of q and r are the same. Easy cube roots and
square roots.  This curve will wrap 1 in 68 trillion hash/256,
truncated to 255 bits.  Algorithm from 'Pairing-Friendly Elliptic
Curves of Prime Order' by Barreto and Naehrig")

;; ---------------------------------------------------------------------------------------
;; from: genfparam 250
(defparameter *curve-fr250-params*
  (make-curve-params
   :pairing-text
   #>.end
type f
q 1809251394332986959257939850161114686612631097102842638593643553811817328791
r 1809251394332986959257939850161114686570095801237726254523530744446827763441
b 3
beta 1437141908251968146817076402274864795341362088762910734010474370732118688574
alpha0 536271594856618124639488944906714824552130805762113454417772030647384157770
alpha1 28425959349375493106220488310181159417866476731095721227729238529329201869
.end
#|
   :g1  (make-instance 'g1-cmpr
         :pt (bev
              (make-instance 'hex
                :str "ff8f256bbd48990e94d834fba52da377b4cab2d3e2a08b6828ba6631ad4d668500")))
   :g2  (make-instance 'g2-cmpr
         :pt (bev
              (make-instance 'hex
                             :str "e20543135c81c67051dc263a2bc882b838da80b05f3e1d7efa420a51f5688995e0040a12a1737c80def47c1a16a2ecc811c226c17fb61f446f3da56c420f38cc01")))
|#
)

  "Curve parameters adapted to ensure q is as large as possible within
the constraints that q < 2^250, q and r prime, q = 3 mod 4, q = 4 mod
9, and b = 3. Bitlengths of q and r are the same. Easy cube roots and
square roots.  This curve will wrap 1 in 23 trillion hash/256,
truncated to 250 bits.  Algorithm from 'Pairing-Friendly Elliptic
Curves of Prime Order' by Barreto and Naehrig")


;; ---------------------------------------------------------------------------------------
;; from: genfparam 248
(defparameter *curve-fr248-params*
  (make-curve-params
   :pairing-text
   #>.end
type f
q 452312848583254953884744365750739939688865847938171536927600539923801802599
r 452312848583254953884744365750739939667598200005613151790322367567049477473
b 3
beta 397249851777460990708985571352217434120562840764162776983847378519420311973
alpha0 304231671163681708906096514291872830602548274253692921780966369109574403653
alpha1 345706227803693509060549771291435615772039886420137687701437165115462066273
.end
#|
   :g1  (make-instance 'g1-cmpr
         :pt (bev
              (make-instance 'hex
                :str "ff8f256bbd48990e94d834fba52da377b4cab2d3e2a08b6828ba6631ad4d668500")))
   :g2  (make-instance 'g2-cmpr
         :pt (bev
              (make-instance 'hex
                             :str "e20543135c81c67051dc263a2bc882b838da80b05f3e1d7efa420a51f5688995e0040a12a1737c80def47c1a16a2ecc811c226c17fb61f446f3da56c420f38cc01")))
|#
)

  "Curve parameters adapted to ensure q is as large as possible within
the constraints that q < 2^248, q and r prime, q = 3 mod 4, q = 4 mod
9, and b = 3. Bitlengths of q and r are the same. Easy cube roots and
square roots.  This curve will wrap 1 in 40 trillion hash/256,
truncated to 248 bits..  Algorithm from 'Pairing-Friendly Elliptic
Curves of Prime Order' by Barreto and Naehrig")


;; ---------------------------------------------------------------------------------------
;; from: genfparam 247
(defparameter *curve-fr247-params*
  (make-curve-params
   :pairing-text
   #>.end
type f
q 226156424291628771614785038744124807180463434874498764847568283250641725631
r 226156424291628771614785038744124807165424936801498629912928666264952945481
b 3
beta 157581236914492743008411029886095484341247996949025327085235169276682226769
alpha0 102203583261777748687691191922467678782639793054014524719597751476114479682
alpha1 183476226787262761591357671373987444584218941387482623829885679422372771068
.end
#|
   :g1  (make-instance 'g1-cmpr
         :pt (bev
              (make-instance 'hex
                :str "ff8f256bbd48990e94d834fba52da377b4cab2d3e2a08b6828ba6631ad4d668500")))
   :g2  (make-instance 'g2-cmpr
         :pt (bev
              (make-instance 'hex
                             :str "e20543135c81c67051dc263a2bc882b838da80b05f3e1d7efa420a51f5688995e0040a12a1737c80def47c1a16a2ecc811c226c17fb61f446f3da56c420f38cc01")))
|#
)

  "Curve parameters adapted to ensure q is as large as possible within
the constraints that q < 2^247, q and r prime, q = 3 mod 4, q = 4 mod
9, and b = 3. Bitlengths of q and r are the same. Easy cube roots and
square roots.  This curve will wrap 1 in 51 trillion hash/256,
truncated to 247 bits.  Algorithm from 'Pairing-Friendly Elliptic
Curves of Prime Order' by Barreto and Naehrig")

;; ---------------------------------------------------------------------------------------
;; from: genfparam 256
(defparameter *curve-fr256-params-old*
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
   :g1  (make-instance 'g1-cmpr
         :pt (bev
              (make-instance 'hex
                :str "0761cf30e9ce29716b7c5b7bb25b62371b64f73bb1515487de78beeda041f98f01")))
   :g2  (make-instance 'g2-cmpr
         :pt (bev
              (make-instance 'hex
                :str "05063635c1a668e13ff75dc50e3ee70691956c1e3a7a1aa753949bfc5a2c64b1089295808a7b287851ed003e0c03de12be1ab149825c21c909f0c440e145d0b000"))))

  "Ben Lynn's quick and dirty F-type generation. This curve will wrap
3 out of 4 hash/256")

;; ---------------------------------------------------------------------------------------
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
   :g1  (make-instance 'g1-cmpr
         :pt (make-instance 'base58
              :str "BirBvAoXsqMYtZCJ66wwCSFTZFaLWrAEhS5GLFrd96DGojc9xfp7beyDPxC5jSuta3yTMXQt7BXLTpam9dj1MVf7m"))
   :g2  (make-instance 'g2-cmpr
         :pt (make-instance 'base58
              :str
"FXJJmcVJsYYG8Y89AZ9Z51kjVANBD68LQi7pD28EG92dxFoWijrcrDaVVYUgiB9yv4GazAAGg7ARg6FeDxCxetkY8")))
"Ben Lynn's favorite default - 160 bits symmetric pairing.
Unfortunately, we are a decade later than when these curves were
developed and 80-bit security is no longer sufficient. But this curve
serves as a check on our implementation with his pbc-calc for
comparison.")

;; ---------------------------------------------------------------------------------------

(defparameter *curve*      nil)

(define-symbol-macro *curve-order*   (curve-params-order   *curve*)) ;; order of all groups (G1,G2,Zr,Gt)
(define-symbol-macro *g1*            (curve-params-g1      *curve*)) ;; generator for G1
(define-symbol-macro *g2*            (curve-params-g2      *curve*)) ;; generator for G2
(define-symbol-macro *g1-size*       (curve-params-g1-len  *curve*)) ;; G1 corresponds to the h curve
(define-symbol-macro *g2-size*       (curve-params-g2-len  *curve*)) ;; G2 corresponds to the g curve for keying
(define-symbol-macro *zr-size*       (curve-params-zr-len  *curve*)) ;; Zr corresponds to the secret-key
(define-symbol-macro *gt-size*       (curve-params-gt-len  *curve*)) ;; GT corresponds to the pairings

;; -------------------------------------------------

(defun need-pairing ()
  (unless *curve*
    (init-pairing)))

;; -------------------------------------------------

(defvar *crypto-lock*  (mpcompat:make-lock)
  "Used to protect internal startup routines from multiple access")

(defun init-pairing (&key (params *curve-fr256-params* params-supplied-p)
                          force)
  (mpcompat:with-lock (*crypto-lock*)
    (when (or force
              params-supplied-p
              (null *curve*))
      (setf *curve* nil)
      (um:bind* ((:struct-accessors curve-params ((txt pairing-text)
                                                  (g1  g1)
                                                  (g2  g2)) params)
                 (ntxt   (length txt)))
        
        (cffi:with-foreign-pointer (ansbuf #.(* 4 (cffi:foreign-type-size :long)))
          (cffi:with-foreign-string (ctxt txt
                                          :encoding :ASCII)
            (assert (zerop (_init-pairing ctxt ntxt ansbuf)))
            (setf *curve* params
                  *g1-size*  (cffi:mem-aref ansbuf :long 0)
                  *g2-size*  (cffi:mem-aref ansbuf :long 1)
                  *gt-size*  (cffi:mem-aref ansbuf :long 2)
                  *zr-size*  (cffi:mem-aref ansbuf :long 3)
                  *curve-order* nil)
            (if g1
                (set-generator g1)
              (setf *g1* (get-g1)))
            (if g2
                (set-generator g2)
              (setf *g2* (get-g2)))
            (get-order)
            (values)
            ))))))

;; -------------------------------------------------
;; PBC lib expects all values as big-endian
;; We work internally with little-endian values

(defun raw-bytes (x)
  ;; used only in encrypt/decrypt where message is stored as BEV
  (bev-vec (bev x)))

(defun construct-bev (vec)
  ;; careful here... this assumes vec is UB8-VECTOR
  ;; more efficient internally when we know vec.
  (make-instance 'bev
                 :vec vec))

(defun xfer-foreign-to-lisp (fbuf nel)
  (let ((lbuf (make-ub8-vector nel)))
    (dotimes (ix nel)
      (setf (aref lbuf ix) (cffi:mem-aref fbuf :unsigned-char ix)))
    (construct-bev lbuf)))

(defun ensure-bevn (buf nel)
  (bev-vec (bevn buf nel)))

(defun xfer-lisp-to-foreign (lbuf fbuf nel)
  (let ((llbuf  (ensure-bevn lbuf nel)))
    (dotimes (ix nel)
      (setf (cffi:mem-aref fbuf :unsigned-char ix) (aref llbuf ix)))
    ))

(defmacro with-fli-buffers (buffers &body body)
  (if (endp buffers)
      `(progn
         ,@body)
    (destructuring-bind (name size &optional lisp-buf) (car buffers)
      (if lisp-buf
          `(cffi:with-foreign-pointer (,name ,size)
             (xfer-lisp-to-foreign ,lisp-buf ,name ,size)
             (with-fli-buffers ,(cdr buffers) ,@body))
        `(cffi:with-foreign-pointer (,name ,size)
           (with-fli-buffers ,(cdr buffers) ,@body))
        ))))

#+:LISPWORKS
(editor:setup-indent "with-fli-buffers" 1)

;; -------------------------------------------------
;; GET-ELEMENT -- read a (possibly compressed) element from the C
;; layer.  On first attempt, we generally don't know the size
;; involved, so a first call is made with a null buffer pointer. The C
;; world returns the actual neededd lenght to us, and stores that into
;; the :LONG count buffer. Then we proceed as normal.

(defun get-element (nb get-fn)
  (need-pairing)
  (with-fli-buffers ((buf nb))
    (assert (eql nb (funcall get-fn buf nb)))
    (xfer-foreign-to-lisp buf nb)))
              
;; -------------------------------------------------

(defun get-g1 ()
  (or *g1*
      (setf *g1*
            (make-instance 'g1-cmpr
                           :pt (get-element *g1-size* '_get-g1))
            )))

(defun get-g2 ()
  (or *g2*
      (setf *g2*
            (make-instance 'g2-cmpr
                           :pt (get-element *g2-size* '_get-g2))
            )))

(defun get-order ()
  ;; retuns an integer
  (or *curve-order*
      (setf *curve-order*
            (let ((txt (curve-params-pairing-text *curve*)))
              (read-from-string txt t nil
                                :start (+ (search "r " txt
                                                  :test 'string-equal)
                                          2)))
            )))

;; -------------------------------------------------
;; NOTE: Mapping hash values to Elliptic curves using the mapping
;; first to the finite field, then multiplying by a curve generator is
;; *COMPLETELY UNSAFE* for signature generation when the pairing is
;; symmetric. Anyone could forge a signature on any message.
;;
;; Thankfully, for security reasons, we use asymmetric BN pairing
;; curves and this insecurity is not present in our system.
;;

(defmethod mod-hash ((hash hash) nb)
  ;; the PBC library has a terrible inconsistency once the hash value
  ;; exceeds the order of the fields. At that point, instead of
  ;; wrapping, it appears to shift right by one or more bits before
  ;; being consumed into the field element. Collisions occur for
  ;; adjacent values thereafter.
  ;;
  ;; PBC also ignores any bits beyond the field size for large hashes,
  ;; effectively making it take only the MSB portion of the hash.
  ;;
  ;; We correct that by taking the hash value modulo the field size
  ;; before submitting to the PBC libraary. 
  ;;
  (let ((v  (int hash))
        (r  (get-order)))
    (if (> v r)
        (bevn (mod v r) nb)
      hash)))

(defmethod g1-from-hash ((hash hash))
  (need-pairing)
  (let ((nb  (hash-length hash)))
    (with-fli-buffers ((ptbuf  *g1-size*)
                       (hbuf   nb (mod-hash hash nb)))
      (_get-g1-from-hash ptbuf hbuf nb)
      (make-instance 'g1-cmpr
                     :pt  (xfer-foreign-to-lisp ptbuf *g1-size*))
      )))
                       
(defmethod g2-from-hash ((hash hash))
  (need-pairing)
  (let ((nb (hash-length hash)))
  (with-fli-buffers ((ptbuf  *g2-size*)
                     (hbuf   nb  (mod-hash hash nb)))
    (_get-g2-from-hash ptbuf hbuf nb)
    (make-instance 'g2-cmpr
                   :pt  (xfer-foreign-to-lisp ptbuf *g2-size*))
    )))
                       
(defmethod zr-from-hash ((hash hash))
  (need-pairing)
  (let ((nb (hash-length hash)))
    (with-fli-buffers ((zbuf   *zr-size*)
                       (hbuf   nb  (mod-hash hash nb)))
      (_get-zr-from-hash zbuf hbuf nb)
      (make-instance 'zr
                     :val  (xfer-foreign-to-lisp zbuf *zr-size*))
      )))
                       
;; -------------------------------------------------

(defmethod set-element ((x crypto-val) set-fn nb)
  ;; internal routine
  (need-pairing)
  (mpcompat:with-lock (*crypto-lock*)
    (let ((bytes (crypto-val-vec x)))
      (with-fli-buffers ((buf nb bytes))
        (funcall set-fn buf)))))

(defmethod set-generator ((g1 g1-cmpr))
  (setf *g1* g1)
  (set-element g1 '_set-g1 *g1-size*))

(defmethod set-generator ((g2 g2-cmpr))
  (setf *g2* g2)
  (set-element g2 '_set-g2 *g2-size*))

;; -------------------------------------------------

(defmethod sign-hash ((hash hash) (skey secret-key))
  ;; hash-bytes is UB8V
  (need-pairing)
  (let ((nhash (hash-length hash)))
    (with-fli-buffers ((sigbuf *g1-size*)
                       (skbuf  *zr-size* skey)
                       (hbuf nhash hash))
      (_sign-hash sigbuf skbuf hbuf nhash)
      (make-instance 'signature
                     :val (xfer-foreign-to-lisp sigbuf *g1-size*))
      )))

(defmethod check-hash ((hash hash) (sig signature) (pkey public-key))
  ;; hash-bytes is UB8V
  (let ((nhash (hash-length hash)))
    (with-fli-buffers ((sbuf *g1-size*  sig)
                       (hbuf nhash      hash)
                       (pbuf *g2-size*  pkey))
      (zerop (_check-signature sbuf hbuf nhash pbuf))
      )))

;; --------------------------------------------------------------
;; BLS Signatures on Messages - result is a triple (MSG, SIG, PKEY)

(defclass signed-message ()
  ((msg   :reader  signed-message-msg
          :initarg :msg)
   (sig   :reader  signed-message-sig
          :initarg :sig)
   (pkey  :reader  signed-message-pkey  ;; who signed it
          :initarg :pkey)
   ))

(defun sign-message (msg pkey skey)
  (make-instance 'signed-message
                 :msg  msg
                 :sig  (sign-hash (hash/256 msg) skey)
                 :pkey pkey))

(defmethod check-message ((sm signed-message))
  (check-hash (hash/256 (signed-message-msg sm))
              (signed-message-sig       sm)
              (signed-message-pkey      sm)))

;; --------------------------------------------------------------
;; Keying - generate secret and authenticated public keys

(defclass keying-triple ()
  ((pkey  :reader keying-triple-pkey
          :initarg :pkey)
   (sig   :reader keying-triple-sig
          :initarg :sig)
   (skey  :reader keying-triple-skey
          :initarg :skey)))

(defun make-key-pair (seed)
  ;; seed can be literally anything at all...
  (need-pairing)
  (multiple-value-bind (hsh hlen) (hash/256 seed)
    (with-fli-buffers ((sbuf *zr-size*)
                       (pbuf *g2-size*)
                       (hbuf hlen (mod-hash hsh hlen)))
      (_make-key-pair sbuf pbuf hbuf hlen)
      (let* ((pkey (make-instance 'public-key
                                  :val (xfer-foreign-to-lisp pbuf *g2-size*)))
             (skey (make-instance 'secret-key
                                  :val (xfer-foreign-to-lisp sbuf *zr-size*)))
             (sig  (sign-hash (hash/256 pkey) skey))) ;; signature on public key
        ;; return 3 values: public key, signature on public key, secret key
        (make-instance 'keying-triple
                       :pkey pkey
                       :sig  sig
                       :skey skey)
        ))))

(defmethod check-public-key ((pkey public-key) (psig signature))
  (check-hash (hash/256 pkey)
              psig
              pkey))

;; -----------------------------------------------------------------------
;; Sakai-Haskara Encryption

(defmethod make-public-subkey ((pkey public-key) seed)
  (multiple-value-bind (hsh hlen) (hash/256 seed)
    (with-fli-buffers ((hbuf hlen      (mod-hash hsh hlen))
                       (pbuf *g2-size* (public-key-val pkey))
                       (abuf *g2-size*))
      (_make-public-subkey abuf pbuf hbuf hlen)
      (make-instance 'public-key
                     :val (xfer-foreign-to-lisp abuf *g2-size*)))))

(defmethod make-secret-subkey ((skey secret-key) seed)
  (multiple-value-bind (hsh hlen) (hash/256 seed)
    (with-fli-buffers ((hbuf hlen      (mod-hash hsh hlen))
                       (sbuf *zr-size* (secret-key-val skey))
                       (abuf *g1-size*))
      (_make-secret-subkey abuf sbuf hbuf hlen)
      (make-instance 'secret-key
                     :val (xfer-foreign-to-lisp abuf *g1-size*)))))

;; --------------------------------------------------------------
;; SAKKE - Sakai-Kasahara Pairing Encryption

(defclass crypto-packet ()
  ((pkey   :reader  crypto-packet-pkey     ;; public key of intended recipient
           :initarg :pkey)
   (id     :reader  crypto-packet-id       ;; ID used in IBE for this message
           :initarg :id)
   (tstamp :reader  crypto-packet-tstamp   ;; timestamp of encryption
           :initarg :tstamp)
   (rval   :reader  crypto-packet-rval     ;; R value of encryption
           :initarg :rval)
   (cmsg   :reader  crypto-packet-cmsg     ;; cryptotext of encrypted message
           :initarg :cmsg)
   ))

(defmethod ibe-encrypt (msg (pkey public-key) id)
  ;; msg can be anything and of any length. (we use LOENC:ENCODE)
  ;; Asymmetric encryption is intended only for short messages, like
  ;; keying material. Use symmetric encryption for bulk message
  ;; encryption. But this will work regardless.
  (let* ((pkid      (make-public-subkey pkey id))
         (tstamp    (construct-bev (uuid:uuid-to-byte-array
                                    (uuid:make-v1-uuid))))
         (msg-bytes (loenc:encode msg))
         (nmsg      (length msg-bytes))
         (xlen      (* 32 (ceiling nmsg 32)))
         (xbytes    (if (< nmsg xlen)
                        (let ((cloaked (make-ub8-vector xlen)))
                          (replace cloaked msg-bytes)
                          (fill cloaked 0 :start nmsg)
                          cloaked)
                      ;; else
                      msg-bytes))
         (xmsg      (construct-bev xbytes))
         (rhsh      (hash/256 id tstamp xmsg)))
    (with-fli-buffers ((hbuf  32         (mod-hash rhsh 32))   ;; hash value
                       (pbuf  *gt-size*)         ;; returned pairing
                       (kbuf  *g2-size*  pkid)   ;; public key
                       (rbuf  *g2-size*))        ;; returned R value
      (_sakai-kasahara-encrypt rbuf pbuf kbuf hbuf 32)
      (let* ((pval (get-hash-nbytes xlen (xfer-foreign-to-lisp pbuf *gt-size*)))
             (cmsg (make-instance 'crypto-text
                                  :vec (construct-bev
                                        (map-into pval 'logxor pval xbytes))))
             (rval (make-instance 'g2-cmpr
                                  :pt (xfer-foreign-to-lisp rbuf *g2-size*))))
        (make-instance 'crypto-packet
                       :pkey   pkey
                       :id     id
                       :tstamp tstamp
                       :rval   rval
                       :cmsg   cmsg)
        ))))
                             
(defmethod ibe-decrypt ((cx crypto-packet) (skey secret-key))
  (with-accessors ((pkey   crypto-packet-pkey)
                   (id     crypto-packet-id)
                   (tstamp crypto-packet-tstamp)
                   (rval   crypto-packet-rval)
                   (cmsg   crypto-packet-cmsg)) cx
    (let ((skid (make-secret-subkey skey id))
          (pkey (make-public-subkey pkey id)))
      (with-fli-buffers ((pbuf  *gt-size*)
                         (kbuf  *g1-size*  skid)
                         (rbuf  *g2-size*  rval))
        (_sakai-kasahara-decrypt pbuf rbuf kbuf)
        (let* ((cmsg-bytes (raw-bytes cmsg))
               (nb         (length cmsg-bytes))
               (pval (get-hash-nbytes nb (xfer-foreign-to-lisp pbuf *gt-size*)))
               (msg  (construct-bev
                      (map-into pval 'logxor pval cmsg-bytes)))
               (hval (hash/256 id tstamp msg)))
          (with-fli-buffers ((hbuf 32        hval)
                             (kbuf *g2-size* pkey))
            (when (zerop (_sakai-kasahara-check rbuf kbuf hbuf 32))
              (loenc:decode (raw-bytes msg))))
          )))))
    
;; -----------------------------------------------

(defmethod compute-pairing ((hval g1-cmpr) (gval g2-cmpr))
  (need-pairing)
  (with-fli-buffers ((hbuf  *g1-size*  hval)
                     (gbuf  *g2-size*  gval)
                     (gtbuf *gt-size*))
    (_compute-pairing gtbuf hbuf gbuf)
    (make-instance 'pairing
                   :val (xfer-foreign-to-lisp gtbuf *gt-size*))))

;; --------------------------------------------------------
;; Curve field operations -- to match academic papers, we utilize the
;; "bent" nomenclature where curve point addition is denoted by group
;; multiplication, curve point scalar multiplication is denoted as
;; group exponentiation.

(defun binop (op a b a-siz b-siz final)
  ;; operate on operands a, b, returning in the buffer for a
  ;; it is assumed that the a-siz is also the size of result.
  (need-pairing)
  (with-fli-buffers ((a-buf  a-siz  a)
                     (b-buf  b-siz  b))
    (funcall op a-buf b-buf) ;; result returned in first arg buffer
    (funcall final (xfer-foreign-to-lisp a-buf a-siz))))

(defun make-g1-ans (ans)
  (make-instance 'g1-cmpr
                 :pt  ans))

(defun make-g2-ans (ans)
  (make-instance 'g2-cmpr
                 :pt ans))

(defun make-zr-ans (ans)
  (make-instance 'zr
                 :val ans))

;; -------------------------------

(defmethod mul-pts ((pt1 g1-cmpr) (pt2 g1-cmpr))
  ;; multiply two elements from G1 field (always the shorter field
  ;; rep)
  ;;
  ;; (should be obvious, but you can't mix G1 with G2, except by
  ;; pairing operations)
  ;;
  (cond ((zerop (int pt1)) pt2)
        ((zerop (int pt2)) pt1)
        (t 
         (binop '_mul-g1-pts pt1 pt2
                *g1-size* *g1-size* 'make-g1-ans))
        ))

(defun add-pts (pt1 pt2)
  ;; for non-bent nomenclature
  (mul-pts pt1 pt2))

(defmethod mul-pts ((pt1 g2-cmpr) (pt2 g2-cmpr))
  ;; multiply two elements from G2 field
  (cond ((zerop (int pt1)) pt2)
        ((zerop (int pt2)) pt1)
        (t 
         (binop '_mul-g2-pts pt1 pt2
                *g2-size* *g2-size* 'make-g2-ans))
        ))
        
(defmethod add-zrs ((z1 zr) (z2 zr))
  ;; add two elements from Zr ring
  (binop '_add-zr-vals z1 z2
         *zr-size* *zr-size* 'make-zr-ans))

(defmethod add-zrs ((z1 integer) z2)
  (add-zrs z2 (make-instance 'zr
                             :val (mod z1 (get-order)))))

(defmethod inv-zr ((z zr))
  ;; compute inverse of z in ring Zr
  (when (zerop (int z))
    (error "Can't invert zero"))
  (need-pairing)
  (with-fli-buffers ((z-buf  *zr-size* z))
    (_inv-zr-val z-buf)
    (make-instance 'zr
                   :val (xfer-foreign-to-lisp z-buf *zr-size*))))

(defmethod inv-zr ((z integer))
  (inv-zr (make-instance 'zr
                         :val (mod z (get-order)))))

(defvar *g1-zero*
  (make-instance 'g1-cmpr
                 :pt (bev 0)))

(defvar *g2-zero*
  (make-instance 'g2-cmpr
                 :pt (bev 0)))

(defmethod expt-pt-zr ((g1 g1-cmpr) (z zr))
  ;; exponentiate an element of G1 by element z of ring Zr
  (cond ((zerop (int z)) *g1-zero*)
        (t
         (binop '_exp-G1z g1 z
                *g1-size* *zr-size* 'make-g1-ans))
        ))

(defmethod expt-pt-zr ((g2 g2-cmpr) (z zr))
  ;; exponentiate an element of G2 by element z of ring Zr
  (cond ((zerop (int z)) *g2-zero*)
        (t 
         (binop '_exp-G2z g2 z
                *g2-size* *zr-size* 'make-g2-ans))
        ))

(defmethod expt-pt-zr (g1 (z integer))
  (expt-pt-zr g1 (make-instance 'zr
                                :val (mod z (get-order)))))

(defun mul-pt-zr (pt z)
  ;; for non-bent nomenclature
  (expt-pt-zr pt z))

;; --------------------------------------------------------
;; BLS MultiSignatures

(defmethod combine-signatures ((sm1 signed-message) (sm2 signed-message))
  ;; BLS multi-signature is the product of the G1 and G2 elements
  ;; between them
  (with-accessors ((msg1   signed-message-msg)
                   (sig1   signed-message-sig)
                   (pkey1  signed-message-pkey)) sm1
    (with-accessors ((sig2  signed-message-sig)
                     (pkey2 signed-message-pkey)) sm2
      ;; no point combining signatures unless the message was the
      ;; same for both...
      (make-instance 'signed-message
                     :msg  msg1
                     :sig  (change-class (mul-pts sig1 sig2)   'signature)
                     :pkey (change-class (mul-pts pkey1 pkey2) 'public-key))
      )))

;; ------------------------------------------------------
;; VRF - Publicly Verifiable Random Functions

(defstruct vrf
  seed x y proof)

(defmethod compute-vrf (seed (skey secret-key))
  (need-pairing)
  (let* ((x      (zr-from-hash (hash:hash/256 seed)))
         (1/x+s  (with-mod (get-order)
                   (m/ (m+ (int x) (int skey)))))
         (g1     (mul-pt-zr (get-g1) 1/x+s))
         (y      (compute-pairing g1 (get-g2))))
    (make-vrf
     :seed  seed
     :x     x
     :y     y
     :proof g1)))

(defmethod validate-vrf ((vrf vrf) (pkey public-key))
  (need-pairing)
  (let* ((x   (zr-from-hash (hash:hash/256 (vrf-seed vrf))))
         (g2  (add-pts (mul-pt-zr (get-g2) x) pkey))
         (c   (compute-pairing (vrf-proof vrf) g2))
         (y   (compute-pairing (vrf-proof vrf) (get-g2)))
         (chk (compute-pairing (get-g1) (get-g2))))
    (and (= (int c) (int chk))
         (= (int x) (int (vrf-x vrf)))
         (= (int y) (int (vrf-y vrf))))))

;; --------------------------------------------------------
;; (init-pairing *curve-default-ar160-params*)
;(init-pairing)
;; --------------------------------------------------------
#|
(init-pairing)

;; check BLS Signatures
(multiple-value-bind (pkey psig skey) (make-key-pair :dave)
  (let* ((msg  :hello-dave)
         (hash (hash/256 msg)))
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
         (hsh    (hash/256 msg))
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
         (hsh  (hash/256 msg))
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
#|
(defun tst (&optional (n 100))
  "test reentrancy of PBC lib. If the lib isn't reentrant, we should
likely see an assertion failure"
  (labels ((doit (name msg)
             (let ((k (make-key-pair name)))
               (dotimes (ix n)
                 (let ((sig (sign-message msg
                                          (keying-triple-pkey k)
                                          (keying-triple-skey k))))
                   (assert (check-message sig)))))))
    (ac:=bind (ans)
        (ac:par
          (doit :one   :okay#1)
          (doit :two   :okay#2)
          (doit :three :okay#3)
          (doit :four  :okay#4)
          (doit :five  :okay#5)
          (doit :six   :okay#6)
          (doit :seven :okay#7)
          (doit :eight :okay#8))
      (ac:pr :done ans))))
|#
