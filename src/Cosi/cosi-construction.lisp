;; cos-construction.lisp -- Generate simulation trees of network nodes for Cosi
;;
;; DM/Emotiq 02/18
;; -------------------------------------------------------------------------
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

(declaim (optimize (debug 3)))

;; --------------------------------------------------------------------
;; Physical network

(defvar *local-nodes*  '(("Malachite.local" . "10.0.1.6")
                         ("Dachshund.local" . "10.0.1.3")
                         ("Rambo"           . "10.0.1.13")
                         ("ChromeKote.local" . "10.0.1.36")))

(defun get-local-ipv4 (node-name)
  (cdr (assoc node-name *local-nodes*
              :test 'string-equal)))
  
(defun get-my-ipv4 ()
  (get-local-ipv4 (machine-instance)))

(defvar *real-nodes*  (mapcar 'cdr *local-nodes*))
(defvar *leader-node* (get-local-ipv4 "Dachshund.local"))
;;(defvar *leader-node* (get-local-ipv4 "ChromeKote.local"))

(defvar *top-node*   nil) ;; current leader node
(defvar *my-node*    nil) ;; which node my machine is on

;; default-timeout-period needs to be made smarter, based on node height in tree
(defparameter *default-timeout-period*   ;; good for 1600 nodes on single machine
  #+:LISPWORKS   10
  #+:ALLEGRO     70
  #+:CLOZURE     70
  #-(or :LISPWORKS :ALLEGRO :CLOZURE)
  70)

;; ----------------------------------------------------------------------
;; Network Tree Nodes

(defvar *bins-per-node* 9) ;; prolly needs to be >3 for BFT

(defun make-subs ()
  (make-array *bins-per-node*
              :initial-element nil))

(defclass node ()
  ((ip       :accessor node-ip       ;; IPv4 string for this node
             :initarg  :ip)
   (uuid     :accessor node-uuid     ;; UUID for this node
             :initarg  :uuid)
   (pkeyzkp  :accessor node-pkeyzkp  ;; public key + ZKP
             :initarg  :pkeyzkp)
   (skey     :accessor node-skey     ;; private key
             :initarg  :skey)
   (pkey     :accessor node-pkey     ;; cached ECC point for public key
             :initarg  :pkey)
   (parent   :accessor node-parent   ;; points to node of group parent
             :initarg  :parent
             :initform nil)
   (subs     :accessor node-subs     ;; list of group members beyond self
             :initarg  :subs
             :initform (make-subs))
   (bit      :accessor node-bit      ;; bit position in bitmap
             :initform 0)
   ;; -------------------------------------
   (real-ip  :accessor node-real-ip  ;; real node for this node
             :initarg  :real-ip)
   (ok       :accessor node-ok       ;; t if we participated in round 1
             :initform nil)
   (v        :accessor node-v        ;; first round random seed
             :initform nil)
   (seq      :accessor node-seq      ;; first round ID
             :initform nil)
   (byz      :accessor node-byz      ;; Byzantine misbehav type
             :initform nil)
   (parts    :accessor node-parts    ;; group participants in first round commitment
             :initform nil)
   (bad      :accessor node-bad      ;; if true then node was corrupted
             :initform nil)
   (load     :accessor node-load     ;; cpu loading of group for this node
             :initform 1)
   (self     :accessor node-self     ;; ptr to Actor handler
             :initarg  :self)
   ))

(defmethod node-bitmap ((node node))
  (ash 1 (node-bit node)))

(defmethod node-realnode ((node node))
  (string-equal (node-ip node) (node-real-ip node)))

(defmethod iteri-subs ((node node) fn)
  (loop for sub across (node-subs node)
        for ix from 0
        when sub
        do (funcall fn ix sub)))

(defmethod iter-subs ((node node) fn)
  (iteri-subs node (lambda (ix sub)
                     (declare (ignore ix))
                     (funcall fn sub))))

(defmethod set-node-load (node)
  (setf (node-load node)
        (1+ (loop for sub across (node-subs node)
                  when sub
                  sum  (node-load sub)))))

;; --------------------------------------------------------------------
;; For now, 4 different ways to specify a node:
;;   1. node structure pointer
;;   2. IPv4 address (in dotted string notation)
;;   3. UUID (needs uuid-to-integer for keying *uuid-node* table)
;;   4. PKEY (compressed public key ECC point)

;; XREF from IPv4 address to Tree Node
(defvar *ip-node-tbl*   (make-hash-table :test 'equal)) ;; IPv4 string as key
(defvar *uuid-node-tbl* (make-hash-table))              ;; UUID integer as key
(defvar *pkey-node-tbl* (make-hash-table))              ;; compressed ECC pt integer as key

(defvar *pkey-skey-tbl* (make-hash-table))              ;; commpressed ECC pt integer as key

;; -------------------------------------------------------------------

(defvar *comm-ip*  nil)

(defun make-node (ipstr uuid pkeyzkp parent)
  (let* ((cmpr-pkey (third pkeyzkp))
         (node (make-instance 'node
                              :ip      ipstr
                              :uuid    uuid
                              :skey    (gethash cmpr-pkey *pkey-skey-tbl*)
                              :pkey    (edec:ed-decompress-pt cmpr-pkey)
                              :pkeyzkp pkeyzkp
                              :parent  parent
                              :real-ip *comm-ip*
                              )))
    (setf (node-self node) (make-node-dispatcher node)
          (gethash ipstr *ip-node-tbl*)              node
          (gethash (node-uuid node) *uuid-node-tbl*) node
          (gethash cmpr-pkey *pkey-node-tbl*)        node)))

(defun need-to-specify (&rest args)
  (declare (ignore args))
  (error "Need to specify..."))

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
    (ironclad:produce-digest dig)))

(defun convert-pt-to-v (pt)
  (let ((nb (ceiling (1+ (integer-length *ed-q*)) 8)))
    (convert-int-to-nbytesv (ed-compress-pt pt) nb)))
  
(defun hash-pt-pt (p1 p2)
  (convert-bytes-to-int
   (sha3-buffers (convert-pt-to-v p1)
                 (convert-pt-to-v p2))))

(defun hash-pt-msg (pt msg) 
  ;; We hash an ECC point by first converting to compressed form, then
  ;; to a UB8 vector. A message is converted to a UB8 vector by calling
  ;; on LOENC:ENCODE.
  ;;
  ;; Max compressed point size is 1 bit more than the integer-length
  ;; of the underlying curve field prime modulus.
  ;;
  ;; We return the SHA3 hash as a big-integer
  ;;
  ;; This is callled with the *ed-curve* binding in effect. Client
  ;; functions should call this function from within a WITH-ED-CURVE.
  ;;
  (let ((v  (convert-pt-to-v pt))
        (mv (loenc:encode msg)))
    (convert-bytes-to-int (sha3-buffers v mv))))

;; --------------------------------------------------------------

(defun compute-pkey-zkp (skey pkey)
  ;; from private skey and public ECC pt pkey, compute the pkey ZKP
  (multiple-value-bind (v vpt) (ed-random-pair)
    (let* ((c     (hash-pt-pt vpt pkey)) ;; Fiat-Shamir NIZKP challenge
           (r     (with-mod *ed-r*
                    (m- v (m* skey c))))
           (pcmpr (ed-compress-pt pkey)))
      (list r c pcmpr)))) ;; NIZKP and public key

(defun check-pkey (zkp)
  ;; verify pkey zkp, return decompressed pkey ECC point
  (destructuring-bind (r c pcmpr) zkp
    (let* ((pt   (ed-decompress-pt pcmpr)) ;; node's public key
           (vpt  (ed-add (ed-nth-pt r)    ;; validate with NIZKP 
                         (ed-mul pt c))))
      (values pt
              (= c (hash-pt-pt vpt pt)))
      )))

#+:ALLEGRO
(defun allegro-dotted-to-integer (string)
  (multiple-value-bind (start end starts ends)
      (#~m/^([0-9]+).([0-9]+).([0-9]+).([0-9]+)$/ string)
    (declare (ignore start end))
    (reduce (lambda (ans pair)
              (destructuring-bind (start . end) pair
                (logior (ash ans 8)
                        (parse-integer string :start start :end end))))
            (nreverse (pairlis (coerce starts 'list)
                               (coerce ends   'list)))
            :initial-value 0)))

#+:ALLEGRO
(defun allegro-integer-to-dotted (val)
  (let ((parts (um:nlet-tail iter ((n   4)
                                   (pos 0)
                                   (ans nil))
                 (if (zerop n)
                     ans
                   (iter (1- n) (+ pos 8) (cons (ldb (byte 8 pos) val) ans)))
                 )))
    (format nil "~{~d~^.~}" parts)))

(defun dotted-string-to-integer (string)
  #+:LISPWORKS (comm:string-ip-address string)
  #+:OPENMCL   (ccl::dotted-to-ipaddr string)
  #+:ALLEGRO   (allegro-dotted-to-integer string)
  #-(or :LISPWORKS :ALLEGRO :CLOZURE)
  (usocket:host-byte-order string))

(defun integer-to-dotted-string (val)
  #+:LISPWORKS (comm:ip-address-string val)
  #+:OPENMCL (CCL::ipaddr-to-dotted val)
  #+:ALLEGRO (allegro-integer-to-dotted val)
  #-(or :LISPWORKS :ALLEGRO :CLOZURE)
  (usocket:hbo-to-dotted-quad val))

(defun gen-uuid-int ()
  (uuid:uuid-to-integer (uuid:make-v1-uuid)))

(defun gen-node-id (ip)
  (if (consp ip)
      (values-list ip)
    (multiple-value-bind (skey pkey) (edec:ed-random-pair)
      (let ((zkp (compute-pkey-zkp skey pkey)))
        (setf (gethash (third zkp) *pkey-skey-tbl*) skey)
        (values ip
                (gen-uuid-int)
                zkp)
        ))))

;; -------------------------------------------------------------

;; the *NODE-BIT-TBL* is really an ordered vector of all nodes in the
;; tree, ordered by UUID magnitude

(defvar *node-bit-tbl* #())

(defun assign-bits ()
  ;; assign bit positions to each node
  (let ((collected
         (um:accum acc
           (maphash (lambda (k node)
                      (declare (ignore k))
                      (acc node))
                    *ip-node-tbl*))))
    (setf collected (sort collected '< :key 'node-uuid))
    (loop for node in collected
          for ix from 0 do
          (setf (node-bit node) ix))
    (setf *node-bit-tbl*
          (coerce collected 'vector))
    ))

;; -------------------------------------------------------------------
;; Node construction

(defun partition (node ip-list &key (key 'identity))
  (let* ((bins  (make-subs))
         (nbins (length bins))
         (vnode (dotted-string-to-integer (node-ip node))))
    (mapc (lambda (ip-arg)
            (let* ((vip (dotted-string-to-integer (funcall key ip-arg)))
                   (ix  (mod (logxor vnode vip) nbins)))
              (push ip-arg (aref bins ix))))
          ip-list)
    (setf (node-subs node) bins)))

(defun inner-make-node-tree (ip ip-list &optional parent)
  (multiple-value-bind (ipstr uuid pkeyzkp)
      (gen-node-id ip)
    (let ((node (make-node ipstr uuid pkeyzkp parent)))
      (when ip-list
        (let ((bins (partition node ip-list
                               :key (lambda (ip-arg)
                                      (if (consp ip-arg)
                                          (car ip-arg)
                                        ip-arg)))))
          (iteri-subs node
                      (lambda (ix subs)
                        (setf (aref bins ix)
                              (inner-make-node-tree (car subs)
                                                    (cdr subs)
                                                    node))))
          (set-node-load node)))
      node)))

(defun make-node-tree (ip vlist)
  ;; main entry point - captures IPv4 of arg ip for use as real-ip in
  ;; succeeding nodes
  (multiple-value-bind (ipstr uuid pkeyzp)
      (gen-node-id ip)
    (let ((*comm-ip*  ipstr))
      (inner-make-node-tree (list ipstr uuid pkeyzp) vlist))))

;; --------------------------------------------------------------------
;; for visual debugging...

#+:LISPWORKS
(progn
  (defmethod children (x layout)
    nil)
  
  (defmethod children ((node node) layout)
    (remove nil (coerce (node-subs node) 'list)))

  (defun split-to-octets (val)
    (um:nlet-tail iter ((n   4)
                        (val val)
                        (lst nil))
      (if (zerop n)
          lst
        (iter (1- n) (ash val -8) (cons (ldb (byte 8 0) val) lst)))
      ))

  (defclass red-text (capi:item-pinboard-object)
    ())

  (defclass black-text (capi:item-pinboard-object)
    ())

  (defmethod capi:draw-pinboard-object :around (pinboard (self red-text) &key &allow-other-keys)
    (gp:with-graphics-state (pinboard
                             :foreground :red)
      (call-next-method)))
  
  (defmethod make-node-pane (graphics-port (node node))
    (declare (ignore graphics-port))
    (let ((txt (node-ip node)))
      (make-instance (if (node-realnode node)
                         'red-text
                       'black-text)
                     :text txt)))

  (defmethod view-tree ((tree node) &key (layout :left-right))
    (capi:contain
     (make-instance 'capi:graph-pane
                    :layout-function layout
                    :roots (list tree)
                    :node-pane-function 'make-node-pane
                    :children-function (lambda (node)
                                         (children node layout))
                    ))))

;; --------------------------------------------------------------------
;; Initial Tree Generation and Persistence

(defvar *default-data-file* (asdf:system-relative-pathname :cosi "config/cosi-nodes.txt"))
(defvar *default-key-file*  (asdf:system-relative-pathname :cosi "config/cosi-keying.txt"))

(defun generate-ip ()
  ;; generate a unique random IPv4 address
  (let ((ip (integer-to-dotted-string (random #.(expt 2 32)))))
    (if (gethash ip *ip-node-tbl*)
        (generate-ip) ;; should be unlikely, would be 50% at around 2^16 nodes
      (setf (gethash ip *ip-node-tbl*) ip))))

(defmethod pair-ip-uuid ((node node))
  (list (node-ip node)
        (node-uuid node)
        (node-pkeyzkp node)))

(defmethod pair-ip-uuid ((ip string))
  (pair-ip-uuid (gethash ip *ip-node-tbl*)))

(defun gen-main-tree (leader real-nodes grps)
  (let* ((trees     (mapcar 'make-node-tree real-nodes grps))
         (main-tree (find leader trees
                          :test 'string=
                          :key  'node-ip)))
    ;; attach the non-leader real nodes to the leader node
    (let ((all-but (remove main-tree trees)))
      (setf (node-subs main-tree) (concatenate 'vector
                                               (node-subs main-tree)
                                               (apply 'vector all-but)))
      (dolist (tree all-but)
        (setf (node-parent tree) main-tree)))
    main-tree))

;; --------------------------------------------------------------
;; Generate Tree / Keying and save to startup init files

(defun generate-tree (&key datafile keyfile (nel 1000))
  (let* ((leader     *leader-node*)
         (real-nodes  (remove-duplicates *real-nodes*
                                         :test 'string=)))
    
    ;; ensure leader is in the real-nodes collection
    (assert (member leader real-nodes :test 'string=))
    
    ;; pre-populate hash table with real-nodes
    (clrhash *ip-node-tbl*)
    (clrhash *uuid-node-tbl*)
    (clrhash *pkey-node-tbl*)
    (clrhash *pkey-skey-tbl*)
    (dolist (ip real-nodes)
      (setf (gethash ip *ip-node-tbl*) ip))

    ;; build the trees
    (let* ((nreal       (length real-nodes))
           (nel/grp     (ceiling nel nreal))
           (grps        (loop repeat nreal collect
                              (loop repeat nel/grp collect
                                    (generate-ip))))
           (main-tree   (gen-main-tree leader real-nodes grps)))

      ;; save nodes as a text file for later
      (ensure-directories-exist *default-key-file*  :verbose t)
      (ensure-directories-exist *default-data-file* :verbose t)
      
      (with-open-file (f (or datafile *default-data-file*)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :rename)
        (with-standard-io-syntax
          (let ((*print-readably*     t) ;; to get readable UUID's
                (*print-right-margin* 128))
            (pprint `(:leader     ,leader
                      :real-nodes ,(mapcar 'pair-ip-uuid real-nodes)
                      :groups     ,(mapcar (lambda (grp)
                                             (mapcar 'pair-ip-uuid grp))
                                           grps))
                    f))))
      
      ;; write the pkey/skey associations
      (with-open-file (f (or keyfile *default-key-file*)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :rename)
        (let ((keys nil))
          (maphash (lambda (k v)
                     (push (cons k v) keys))
                   *pkey-skey-tbl*)
          (with-standard-io-syntax
            (pprint keys f))))
      (assign-bits)
      #+:LISPWORKS (view-tree main-tree)
      (setf *my-node*  main-tree
            *top-node* main-tree)
      )))

;; ---------------------------------------------------------------
;; Tree reconstruction from startup init files

(defun read-data-file (path)
  (with-open-file (f path
                     :direction :input)
    (read f)))
      
(defun reconstruct-tree (&key datafile keyfile)
  ;; read the keying file
  (let* ((key-path  (or keyfile 
                     *default-key-file*))
         (keys       (read-data-file key-path))
         (data-path   (or datafile
                          *default-data-file*))
         (data        (read-data-file data-path))
         (leader      (getf data :leader))
         (real-nodes  (getf data :real-nodes))
         (grps        (getf data :groups)))
    
    ;; sanity checking
    (assert (member leader real-nodes
                    :test 'string=
                    :key  'car))
    (labels ((no-dups (lst)
               (dolist (ip lst)
                 (destructuring-bind (ipstr uuid zkp) ip
                   (assert (null (gethash ipstr *ip-node-tbl*)))
                   (assert (null (gethash uuid  *uuid-node-tbl*)))
                   (destructuring-bind (r c pcmpr) zkp
                     (declare (ignore r c))
                     (assert (null (gethash pcmpr *pkey-node-tbl*)))
                     (check-pkey zkp)
                     (setf (gethash ipstr *ip-node-tbl*)   ip
                           (gethash uuid  *uuid-node-tbl*) ip
                           (gethash pcmpr *pkey-node-tbl*) ip)
                     )))))
      (clrhash *ip-node-tbl*)
      (clrhash *uuid-node-tbl*)
      (clrhash *pkey-node-tbl*)
      (no-dups real-nodes)
      (mapc #'no-dups grps))
    
    ;; reconstruct keying info
    (clrhash *pkey-skey-tbl*)
    (mapc (lambda (pair)
            (destructuring-bind (k . v) pair
              ;; k is integer, compressed pkey ECC pt
              ;; v is skey integer
              ;; decompression checks for valid ECC pt
              (assert (ed-pt= (ed-nth-pt v) (ed-decompress-pt k)))
              (setf (gethash k *pkey-skey-tbl*) v)))
          keys)
    
    ;; reconstruct the trees
    (let ((main-tree  (gen-main-tree leader real-nodes grps)))
      (assign-bits)
      #+:LISPWORKS (view-tree main-tree)
      (setf *top-node* main-tree
            *my-node*  (gethash (get-my-ipv4) *ip-node-tbl*)))))

