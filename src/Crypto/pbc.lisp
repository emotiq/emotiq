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

(in-package :pairing-curves)

(fli:disconnect-module :pbclib :remove t)
(fli:register-module :pbclib
                     :dlopen-flags t
                     :real-name  "/usr/local/lib64/libLispPBCIntf.dylib")
                     

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

(fli:define-foreign-function (_init-pairing "init_pairing" :source)
    ((param-text  (:pointer (:unsigned :char)))
     (ntext       :long))
  :language :ansi-c
  :module   :pbclib)

;; -------------------------------------------------

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

(fli:define-foreign-function (_get-public-key "get_public_key" :source)
    ((pbuf        (:pointer :void))
     (pnel        (:pointer :long)))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(fli:define-foreign-function (_get-secret-key "get_secret_key" :source)
    ((pbuf        (:pointer :void))
     (pnel        (:pointer :long)))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

(fli:define-foreign-function (_get-signature "get_signature" :source)
    ((pbuf        (:pointer :void))
     (pnel        (:pointer :long)))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

;; -------------------------------------------------

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

(fli:define-foreign-function (_set-public-key "set_public_key" :source)
    ((pbuf  (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_set-secret-key "set_secret_key" :source)
    ((sbuf  (:pointer (:unsigned :char))))
  :language :ansi-c
  :module   :pbclib)

;; -------------------------------------------------

(fli:define-foreign-function (_make-key-pair "make_key_pair" :source)
    ((pbuf   (:pointer (:unsigned :char)))
     (nel    :long))
  :language :ansi-c
  :module   :pbclib)

(fli:define-foreign-function (_sign-hash "sign_hash" :source)
    ((pbuf   (:pointer (:unsigned :char)))
     (nel    :long))
  :language :ansi-c
  :module   :pbclib)

;; -------------------------------------------------

(fli:define-foreign-function (_check-signature "check_signature" :source)
    ((psig   (:pointer (:unsigned :char)))
     (phash  (:pointer (:unsigned :char)))
     (nhash  :long)
     (pkey   (:pointer (:unsigned :char))))
  :result-type :long
  :language    :ansi-c
  :module      :pbclib)

;; -------------------------------------------------

(defvar *curve-fr256-params-text*
  #>.end
type f
q 16283262548997601220198008118239886027035269286659395419233331082106632227801
r 16283262548997601220198008118239886026907663399064043451383740756301306087801
b 10476541659213232777352255224319706265440471807344192411073251777589416636392
beta 2588849289436542488537732220497504302700946308066126767616133606209888506551
alpha0 15760619495780482509052463852330180194970833782280957391784969704642983647946
alpha1 3001017353864017826546717979647202832842709824816594729108687826591920660735
.end)

(defparameter *G1-size* nil) ;; G1 corresponds to the h curve
(defparameter *G2-size* nil) ;; G2 corresponds to the g curve for keying
(defparameter *Zr-size* nil) ;; Zr corresponds to the secret-key

(defvar *pairing-init*  nil)
(defvar *g2-init*       nil)
(defvar *zr-init*       nil)

;; -------------------------------------------------

(defun init-pairing (param-text)
  (fli:with-dynamic-foreign-objects ()
    (setf *G1-size* nil
          *G2-size* nil
          *Zr-size* nil
          *G2-init* nil
          *Zr-init* nil)
    (_init-pairing (fli:convert-to-dynamic-foreign-string
                    param-text
                    :external-format :ASCII)
                   (length param-text))
    (setf *pairing-init* t)
    (get-secret-key)
    (get-g)
    (get-h)
    ))

;; -------------------------------------------------

(defun get-datum (nb-sym get-fn)
  (assert *pairing-init*)
  (let ((nb (symbol-value nb-sym)))
    (fli:with-dynamic-foreign-objects ()
      (if nb
          (let ((lenbuf (fli:allocate-dynamic-foreign-object
                         :type :long))
                (buf    (fli:allocate-dynamic-foreign-object
                         :type '(:unsigned :char) :nelems nb)))
            (setf (fli:dereference lenbuf) nb)
            (funcall get-fn buf lenbuf)
            (assert (eql nb (fli:dereference lenbuf)))
            (let ((lbuf (make-array nb
                                    :element-type '(unsigned-byte 8))))
              (loop for ix from 0 below nb do
                    (setf (aref lbuf ix) (fli:dereference buf :index ix)))
              lbuf))
        ;; else - size unknown
        (let ((lenbuf (fli:allocate-dynamic-foreign-object
                       :type :long)))
          (funcall get-fn fli:*null-pointer* lenbuf)
          (setf (symbol-value nb-sym) (fli:dereference lenbuf))
          (get-datum nb-sym get-fn))
        ))))
              
;; -------------------------------------------------

(defun get-g ()
  (get-datum '*g2-size* '_get-g))

(defun get-h ()
  (get-datum '*g1-size* '_get-h))

(defun get-signature ()
  (get-datum '*g1-size* '_get-signature))

(defun get-public-key ()
  (get-datum '*g2-size* '_get-public-key))

(defun get-secret-key ()
  (get-datum '*zr-size* '_get-secret-key))

;; -------------------------------------------------

(defun set-generator (g-bytes)
  (assert *pairing-init*)
  (assert (eql (length g-bytes) *g2-size*))
  (fli:with-dynamic-foreign-objects ()
    (let ((gbuf  (fli:allocate-dynamic-foreign-object
                  :type '(:unsigned :char) :nelems (length g-bytes))))
      (loop for v across g-bytes
            for ix from 0
            do
            (setf (fli:dereference gbuf :index ix) v))
      (_set-g gbuf)
      (setf *g2-init* t)
      )))

(defun set-h (h-bytes)
  (assert *pairing-init*)
  (assert (eql (length h-bytes) *g1-size*))
  (fli:with-dynamic-foreign-objects ()
    (let ((hbuf  (fli:allocate-dynamic-foreign-object
                  :type '(:unsigned :char) :nelems (length h-bytes))))
      (loop for v across h-bytes
            for ix from 0
            do
            (setf (fli:dereference hbuf :index ix) v))
      (_set-h hbuf)
      )))

(defun set-public-key (pkey-bytes)
  (assert *pairing-init*)
  (assert (eql (length pkey-bytes) *g2-size*))
  (fli:with-dynamic-foreign-objects ()
    (let ((pbuf  (fli:allocate-dynamic-foreign-object
                  :type '(:unsigned :char) :nelems (length pkey-bytes))))
      (loop for v across pkey-bytes
            for ix from 0
            do
            (setf (fli:dereference pbuf :index ix) v))
      (_set-public-key pbuf))
    ))

(defun set-secret-key (skey-bytes)
  (assert *pairing-init*)
  (assert *g2-init*)
  (assert (eql (length skey-bytes) *zr-size*))
  (fli:with-dynamic-foreign-objects ()
    (let ((sbuf  (fli:allocate-dynamic-foreign-object
                  :type '(:unsigned :char) :nelems (length skey-bytes))))
      (loop for v across skey-bytes
            for ix from 0
            do
            (setf (fli:dereference sbuf :index ix) v))
      (_set-secret-key sbuf)
      (setf *zr-init* t
            *g2-init* t))))

;; -------------------------------------------------

(defun make-key-pair (seed)
  (assert *pairing-init*)
  (assert *g2-init*)
  (let ((hsh  (sha3/256-buffers seed)))
    (fli:with-dynamic-foreign-objects ()
      (let ((hbuf  (fli:allocate-dynamic-foreign-object
                    :type '(:unsigned :char) :nelems 32)))
        (loop for v across hsh
              for ix from 0
              do
              (setf (fli:dereference hbuf :index ix) v))
        (_make-key-pair hbuf 32)
        (setf *zr-init* t
              *g2-init* t)
        ))))

(defun sign-hash (hash-bytes)
  (assert *pairing-init*)
  (assert *zr-init*)
  (let ((nhash (length hash-bytes)))
    (fli:with-dynamic-foreign-objects ()
      (let ((hbuf  (fli:allocate-dynamic-foreign-object
                    :type '(:unsigned :char) :nelems nhash)))
        (loop for v across hash-bytes
              for ix from 0
              do
              (setf (fli:dereference hbuf :index ix) v))
        (_sign-hash hbuf nhash)
        ))))

(defun check-signature (sig-bytes hash-bytes pkey-bytes)
  (assert *pairing-init*)
  (assert *g2-init*)
  (assert (eql (length sig-bytes)  *g1-size*))
  (assert (eql (length pkey-bytes) *g2-size*))
  (let ((nhash (length hash-bytes)))
    (fli:with-dynamic-foreign-objects ()
      (let ((sbuf  (fli:allocate-dynamic-foreign-object
                    :type '(:unsigned :char) :nelems (length sig-bytes)))
            (hbuf  (fli:allocate-dynamic-foreign-object
                    :type '(:unsigned :char) :nelems nhash))
            (pbuf  (fli:allocate-dynamic-foreign-object
                    :type '(:unsigned :char) :nelems (length pkey-bytes))))
        (loop for v across sig-bytes
              for ix from 0
              do
              (setf (fli:dereference sbuf :index ix) v))
        (loop for v across hash-bytes
              for ix from 0
              do
              (setf (fli:dereference hbuf :index ix) v))
        (loop for v across pkey-bytes
              for ix from 0
              do
              (setf (fli:dereference pbuf :index ix) v))
        (_check-signature sbuf hbuf nhash pbuf)
        ))))

;; --------------------------------------------------------

#|
(init-pairing *curve-fr256-params-text*)
|#
