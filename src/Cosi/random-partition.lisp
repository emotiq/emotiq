;; random-partition.lisp -- Generate simulation trees of network nodes for Cosi
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


(defpackage :cosi-simgen
  (:use :common-lisp :cosi :crypto-mod-math)
  (:import-from :edwards-ecc
   :ed-add 
   :ed-sub 
   :ed-mul 
   :ed-div 
   :ed-affine
   :ed-nth-pt
   :*ed-r*
   :*ed-q*
   :ed-neutral-point
   :ed-pt=
   :with-ed-curve
   :ed-compress-pt
   :ed-decompress-pt
   :ed-validate-point
   :ed-hash
   :ed-random-pair)
  (:import-from :ecc-crypto-b571
   :convert-int-to-nbytesv
   :convert-bytes-to-int)
  (:import-from :actors
   :=bind
   :=values
   :=defun
   :=lambda
   :=funcall
   :=apply
   :pmapcar
   :spawn
   :current-actor
   :recv
   :become
   :do-nothing
   :make-actor
   :set-executive-pool
   :with-borrowed-mailbox
   :pr)
  (:export
   :generate-tree
   :reconstruct-tree
   :forwarding))

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
  #+:CLOZURE     70)

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
   (realnode :accessor node-realnode ;; t/f - t when this is a real IPv4 node, nil for fake
             :initform nil)
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
  #+:ALLEGRO   (allegro-dotted-to-integer string))

(defun integer-to-dotted-string (val)
  #+:LISPWORKS (comm:ip-address-string val)
  #+:OPENMCL (CCL::ipaddr-to-dotted val)
  #+:ALLEGRO (allegro-integer-to-dotted val))

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

(defvar *node-bit-tbl* #())

(defun assign-bits ()
  ;; assign bit positions to each node
  (let ((collected
         (um:accum acc (maphash (lambda (k node) (declare (ignore k)) (acc node)) *ip-node-tbl*))))
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

(defvar *default-data-file* "cosi-nodes.txt")
(defvar *default-key-file*  "cosi-keying.txt")

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
    ;; mark the real nodes as special
    (mapc (lambda (tree)
            (setf (node-realnode tree) t))
          trees)
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

(defun generate-tree (&key fname (nel 1000))
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
      (with-open-file (f (merge-pathnames
                          #+:LISPWORKS
                          (sys:get-folder-path :documents)
                          #+(OR :ALLEGRO :OPENMCL)
                          "~/Documents/"
                          (or fname *default-data-file*))
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
      (with-open-file (f (merge-pathnames
                          #+:LISPWORKS
                          (sys:get-folder-path :documents)
                          #+(OR :ALLEGRO :OPENMCL)
                          "~/Documents/"
                          *default-key-file*)
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
      
(defun reconstruct-tree (&key fname)
  ;; read the keying file
  (let* ((key-path  (merge-pathnames
                     #+:LISPWORKS
                     (sys:get-folder-path :documents)
                     #+(OR :ALLEGRO :OPENMCL)
                     "~/Documents/"
                     *default-key-file*))
         (keys       (read-data-file key-path))
         (data-path   (merge-pathnames
                       #+:LISPWORKS
                       (sys:get-folder-path :documents)
                       #+(OR :ALLEGRO :OPENMCL)
                       "~/Documents/"
                       (or fname *default-data-file*)))
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

;; ---------------------------------------------------------------
;; New leader node election... tree rearrangement

(defun notify-real-descendents (node &rest msg)
  (labels ((recurse (sub-node)
             (if (node-realnode sub-node)
                 (apply 'send sub-node msg)
               (iter-subs sub-node #'recurse))))
    (iter-subs node #'recurse)))

(defun all-nodes-except (node)
  (delete node
          (um:accum acc
            (maphash (um:compose #'acc 'um:snd) *ip-node-tbl*))))

(defun node-model-rebuild-tree (parent node nlist)
  (let ((bins (partition node nlist
                         :key 'node-ip)))
    (iteri-subs node
                (lambda (ix subs)
                  (setf (aref bins ix)
                        (node-model-rebuild-tree node
                                                 (car subs)
                                                 (cdr subs)))))
    (setf (node-parent node) parent)
    (set-node-load node)
    node))

(defun node-elect-new-leader (new-leader-ip)
  (let ((new-top-node (gethash new-leader-ip *ip-node-tbl*)))
    ;; Maybe... ready for prime time?
    (cond ((null new-top-node)
           (error "Not a valid leader node: ~A" new-leader-ip))
          ((eq new-top-node *top-node*)
           ;; nothing to do here...
           )
          (t
           (setf *top-node* new-top-node)
           (node-model-rebuild-tree nil new-top-node
                                    (all-nodes-except new-top-node))
           ;;
           ;; The following broadcast will cause us to get another
           ;; notification, but by then the *top-node* will already
           ;; have been set to new-leader-ip, and so no endless loop
           ;; will occur.
           ;;
           (notify-real-descendents new-top-node :election new-leader-ip))
          )))

;; ---------------------------------------------------------
;; Node insertion/change

(defun bin-for-ip (node ip)
  (let ((vnode  (dotted-string-to-integer (node-ip node)))
        (vip    (dotted-string-to-integer ip)))
    (mod (logxor vnode vip) (length (node-subs node)))))

(defun increase-loading (parent-node)
  (when parent-node
    (incf (node-load parent-node))
    (increase-loading (node-parent parent-node))))

(defun node-model-insert-node (node new-node-info)
  ;; info is (ipv4 UUID pkeyzkp)
  (destructuring-bind (ipstr uuid pkeyzkp) new-node-info
    (let* ((ix       (bin-for-ip node ipstr))
           (bins     (node-subs node))
           (sub-node (aref bins ix)))
      (if sub-node
          ;; continue in parallel with our copy of tree
          (node-model-insert-node sub-node new-node-info)
        ;; else
        (let ((new-node (make-node ipstr uuid pkeyzkp node)))
          (setf (node-real-ip new-node)  ipstr
                (node-realnode new-node) t
                (node-skey new-node)     nil
                (aref bins ix)           new-node)
          (incf (node-load node))
          (increase-loading (node-parent node)))
        ))))

(defun node-insert-node (node new-node-info)
  (destructuring-bind (ipstr uuid pkeyzkp) new-node-info
    (let ((new-node (gethash ipstr *ip-node-tbl*)))
      (if new-node ;; already present in tree?
          ;; maybe caller just wants to change UUID or keying
          ;; won't be able to sign unless it know skey
          (multiple-value-bind (pkey ok) (check-pkey pkeyzkp)
            (when ok
              (setf (node-uuid new-node)     uuid
                    (node-pkeyzkp new-node)  pkeyzkp
                    (node-pkey new-node)     pkey ;; cache the decompressed key
                    (node-realnode new-node) t
                    (node-real-ip new-node)  ipstr)))
        ;; else - not already present
        (node-model-insert-node *top-node* new-node-info))))
  (notify-real-descendents node :insert-node new-node-info))

;; ---------------------------------------------------------

(defun node-model-remove-node (gone-node)
  (remhash (node-ip gone-node)   *ip-node-tbl*)
  (remhash (node-uuid gone-node) *uuid-node-tbl*)
  (let ((pcmpr (third (node-pkeyzkp gone-node))))
    (remhash pcmpr *pkey-node-tbl*)
    (remhash pcmpr *pkey-skey-tbl*)))

(defun node-remove-node (node gone-node-ipv4)
  (let ((gone-node (gethash gone-node-ipv4 *ip-node-tbl*)))
    (when gone-node
      (node-model-remove-node gone-node)
      ;; must rebuild tree to absorb node's subnodes
      (node-model-rebuild-tree nil *top-node*
                               (all-nodes-except *top-node*))
      (when (eq node *top-node*)
        (notify-real-descendents node :remove-node gone-node-ipv4)))))
  
;; -----------------------------------------------------------------

(defun NYI (&rest args)
  (error "Not yet implemented: ~A" args))

;; -------------------------------------------------------

(defun make-node-dispatcher (node)
  ;; use indirection to node-dispatcher for while we are debugging and
  ;; extending the dispatcher. Saves reconstructing the tree every
  ;; time the dispatching chanages.
  (ac:make-actor
   ;; one of these closures is stored in the SELF slot of every node
   (lambda (&rest msg)
     (apply 'node-dispatcher node msg))))

(defun crash-recovery ()
  (maphash (lambda (k node)
             (declare (ignore k))
             (setf (node-self node) (make-node-dispatcher node)))
           *ip-node-tbl*))


(defun node-dispatcher (node &rest msg)
   (um:dcase msg
     ;; ----------------------------
     ;; user accessible entry points - directed to leader node
     
     (:cosi (reply-to msg)
      (node-compute-cosi node reply-to msg))

     (:validate (reply-to msg sig)
      (reply reply-to :validation (node-validate-cosi node msg sig)))
          
     (:public-key (reply-to)
      (reply reply-to :pkey+zkp (node-pkeyzkp node)))

     (:add/change-node (new-node-info)
      (node-insert-node node new-node-info))

     (:remove-node (node-ip)
      (node-remove-node node node-ip))
     
     (:election (new-leader-ip)
      (node-elect-new-leader new-leader-ip))

     ;; -------------------------------
     ;; internal comms between Cosi nodes
     
     (:commitment (reply-to msg seq)
      (node-cosi-commitment node reply-to msg seq))

     (:signing (reply-to c c1 seq)
      (node-cosi-signing node reply-to c c1 seq))

     ;; -----------------------------------
     ;; for sim and debug
     
     (:answer (&rest msg)
      ;; for round-trip testing
      (ac:pr msg))

     (:reset ()
      (node-reset-nodes node))
     
     (t (&rest msg)
        (error "Unknown message: ~A~%Node: ~A" msg (node-ip node)))
     ))

;; ------------------------------------------------------------------
;; GET-PUBLIC-KEY - This is here primarily for testing the network.
;; Not really an RPC call, and results will show in the output
;; browser, not returned to caller.
;;
;; RPC violates the basic premise of the point-and-shoot design.
;; Responders are free to ignore, be deaf, or send us garbage at any
;; time.
;;
;; The design of the system is aimed at coexistence in a Byzantine network.
;;
(defun get-public-key (uuid)
  (let ((node (gethash uuid *uuid-node-tbl*)))
    (when node
      (spawn (lambda ()
               (let ((ret (make-return-addr (node-real-ip *my-node*))))
                 (send node :public-key ret)
                 (recv
                   (msg
                    (unregister-return-addr ret)
                    (pr msg))
                   )))
             ))))

#|
(send *top-node* :public-key (make-node-ref *my-node*))
==> see results in output window
(:PKEY+ZKP (849707610687761353988031598913888011454228809522136330182685594047565816483 77424688591828692687552806917061506619936267795838123291694715575735109065947 2463653704506470449709613051914446331689964762794940591210756129064889348739))

COSI-SIMGEN 23 > (send (gethash "10.0.1.6" *ip-node-tbl*) :public-key (make-node-ref *my-node*))

Connecting to #$(NODE "10.0.1.6" 65000)
(FORWARDING "10.0.1.6" (QUOTE ((:PUBLIC-KEY #<NODE-REF 40200014C3>) 601290835549702797100992963662352678603116278028765925372703953633797770499 56627041402452754830116071111198944351637771601751353481660603190062587211624 23801716726735741425848558528841292842)))
==> output window
(:PKEY+ZKP (855676091672863312136583105058123818001884231695959658747310415728976873583 19894104797779289660345137228823739121774277312822467740314566093297448396984 2080524722754689845098528285145820902670538507089109456806581872878115260191))
|#
;; ----------------------------

(defvar *cosi-port*         65001)

(defclass return-addr ()
  ((ip       :accessor return-addr-ip   ;; the real IPv4 for returns
             :initarg  :ip)
   (port     :accessor return-addr-port ;; the real IPv4 port for returns
             :initarg  :port)
   (aid      :accessor return-addr-aid  ;; actor id for returns
             :initarg  :aid)))

(defvar *aid-tbl*
  ;; assoc between Actors and AID's
  #+:LISPWORKS (make-hash-table
                :weak-kind :value)
  #+:ALLEGRO   (make-hash-table
                :values :weak)
  #+:CLOZURE   (make-hash-table :weak :value)
  )

(defmethod unregister-return-addr ((ret return-addr))
  (remhash (return-addr-aid ret) *aid-tbl*)
  (become 'do-nothing))

(defmethod make-return-addr ((ipv4 string) &optional (port *cosi-port*))
  ;; can only be called from within an Actor body
  (let ((aid  (gen-uuid-int))
        (self (current-actor)))
    (unless self
      (error "MAKE-RETURN-ADDR can only be performed by an Actor"))
    (setf (gethash aid *aid-tbl*) self)
    (make-instance 'return-addr
                   :ip   ipv4
                   :port port
                   :aid  aid)))

;; -----------------------------------------------------------

(defmethod send ((node node) &rest msg)
  (unless (node-byz node)
    (socket-send (node-ip node) (node-real-ip node) *cosi-port* msg)))

(defmethod send ((ref return-addr) &rest msg)
  (socket-send ref (return-addr-ip ref) (return-addr-port ref) msg))

(defmethod send ((node null) &rest msg)
  (ac:pr :sent-to-null msg)
  msg)

(defmethod send (dest &rest msg)
  (apply 'ac:send dest msg))

;; -----------------

(defun reply (reply-to &rest msg)
  (apply 'send reply-to :answer msg))

;; -----------------------------------------------------
;; THE SOCKET INTERFACE...
;; -----------------------------------------------------

(defvar *max-buffer-length* 65500)

(defun cosi-service-handler (buf)
  (multiple-value-bind (ans err)
      (ignore-errors
        (loenc:decode buf))
    (unless err
      (multiple-value-bind (packet t/f) (verify-hmac ans)
        (when t/f
          (destructuring-bind (dest msg-verb &rest msg-args) packet
            (cond ((eql :SHUTDOWN-SERVER msg-verb)
                   :SHUTDOWN-SERVER)
                  (t
                   (let ((true-dest (dest-ip dest)))
                     ;; for debug... -------------------
                     (when (eq true-dest (node-self *my-node*))
                       (pr (format nil "forwarding-to-me: ~A" (cons msg-verb msg-args))))
                     ;; ------------------
                     (apply 'send true-dest msg-verb msg-args)
                     t))
                  )))
        ))))

(defun shutdown-server (&optional (port *cosi-port*))
  (when *my-node*
    (let ((me (node-ip *my-node*)))
      (socket-send me me port '(:SHUTDOWN-SERVER)))))

(defun socket-send (ip real-ip real-port msg)
  (let* ((quad     (make-hmac (list* ip msg)
                              (node-skey *my-node*)
                              (node-uuid *my-node*)))
         (packet   (loenc:encode quad))
         (nb       (length packet)))
    (internal-send-socket real-ip real-port packet nb)))

;; ------------------------------------------------------------------

(defmethod dest-ip ((ip string))
  (let ((node (gethash ip *ip-node-tbl*)))
    (when node
      (node-self node))))

(defmethod dest-ip ((ret return-addr))
  (gethash (return-addr-aid ret) *aid-tbl*))

;; ------------------------------------------------------------------
;; deprecated...

#+:xLISPWORKS
(progn ;; TCP/IP stream over Butterfly
  (defun socket-send (ip real-ip msg)
    ;; replace this with USOCKETS protocol
    (let* ((quad     (make-hmac msg
                                (node-skey *my-node*)
                                (node-uuid *my-node*)))
           (agent-ip (format nil "eval@~A" real-ip)))
      ;; (format t "~%SOCKET-SEND: ~A ~A ~A" ip real-ip msg)
      #+:LISPWORKS
      (bfly:! agent-ip `(forwarding ,ip ',quad))))
  
  (defun forwarding (dest quad)
    ;; (format t "~%FORWARDING: ~A ~A" dest quad)
    (multiple-value-bind (msg t/f) (verify-hmac quad)
      ;; might want to log incomings that fail the HMAC
      ;; just dropping on floor here...
      (when t/f
        (let ((true-dest (dest-ip dest)))
          (when true-dest
            (when (equal true-dest (node-self *my-node*))
              (ac:pr
               (format nil "forwarding-to-me: ~A" msg)))
            (apply 'send true-dest msg))))
      )))

;; ------------------------------------------------------------------

#-:LISPWORKS
(progn ;; USOCKET interface for ACL

  (defun #1=serve-cosi-port (socket)
    (let ((maxbuf (make-array *max-buffer-length*
                              :element-type '(unsigned-byte 8))))
      ;; (pr :server-starting-up)
      (unwind-protect
          (loop
	    (multiple-value-bind (buf buf-len rem-ip rem-port)
		(usocket:socket-receive socket maxbuf (length maxbuf))
	      (declare (ignore rem-ip rem-port))
	      ;; (pr :sock-read buf-len rem-ip rem-port (loenc:decode buf))
              (when (eql :SHUTDOWN-SERVER (cosi-service-handler buf))
                (return-from #1#))))
        ;; unwinding
        (usocket:socket-close socket)
        ;; (pr :server-stopped)
        )))
  
  (defun start-ephemeral-server (&optional (port 0))
    (let* ((my-ip  (node-real-ip *my-node*))
	   (socket (usocket:socket-connect nil nil
					   :protocol :datagram
					   :local-host my-ip
					   :local-port port
					   )))
      (mpcompat:process-run-function "UDP Cosi Server" nil
                                     'serve-cosi-port socket)
      (usocket:get-local-port socket)))
       
  (defun start-server ()
    (start-ephemeral-server *cosi-port*))

  (defun internal-send-socket (ip port packet nb)
    (let ((socket (usocket:socket-connect ip port
                                          :protocol :datagram)))
      ;(pr :sock-send (length packet) real-ip packet)
      (unless (eql nb (usocket:socket-send socket packet nb))
        (pr :socket-send-error ip packet))
      (usocket:socket-close socket)
      )))

;; ------------------------------------------------------------------

#+:LISPWORKS
(progn ;; LW UDP Async interface
  (defvar *udp-wait-collection* nil)

  (defun ensure-udp-wait-state-collection ()
    (or *udp-wait-collection*
        (let ((new (comm:create-and-run-wait-state-collection "UDP Cosi Service")))
        (if (sys:compare-and-swap *udp-wait-collection* nil new)
            new
          (progn ; Another process just set it.
            (comm:wait-state-collection-stop-loop new)
            *udp-wait-collection*)))))

  ;; -------------------------
  ;; Server side
  
  (defun #1=udp-cosi-server-process-request (async-io-state string bytes-num ip-address port-num)
    (declare (ignore bytes-num ip-address port-num))
    (let ((status (comm:async-io-state-read-status async-io-state)))
      (when status ;; something went wrong
        (pr (format nil "UDP example server: got error ~s, restarting" status))
        (comm:close-async-io-state async-io-state)
        (start-server)
        (return-from #1#))

      (if (eql :SHUTDOWN-SERVER (cosi-service-handler string))
          (comm:close-async-io-state async-io-state)
        (udp-cosi-server-receive-next async-io-state))))
  
  (defun udp-cosi-server-receive-next (async-io-state )
    (comm:async-io-state-receive-message async-io-state
                                         (make-array *max-buffer-length*
                                                     :element-type '(unsigned-byte 8))
                                         'udp-cosi-server-process-request :needs-address t))
  
  (defun start-ephemeral-server (&optional (port 0))
    (let ((async-io-state (comm:create-async-io-state-and-udp-socket 
                           (ensure-udp-wait-state-collection)
                           :name "UDP Cosi server socket"
                           :local-port port)))
      (udp-cosi-server-receive-next async-io-state)
      (multiple-value-bind (ip-addr ip-port)
          (comm:async-io-state-address async-io-state)
        (declare (ignore ip-addr))
        ip-port)))
      
  (defun start-server ()
    (start-ephemeral-server *cosi-port*))

  ;; -----------------
  ;; Client side
  
  (defun internal-udp-cosi-client-send-request (callback ip-address ip-port packet)
    (let* ((collection     (ensure-udp-wait-state-collection))
           (async-io-state (comm:create-async-io-state-and-udp-socket collection)))
      (comm:async-io-state-send-message-to-address async-io-state
                                                   ip-address
                                                   ip-port
                                                   packet
                                                   callback)
      async-io-state))
  
  (defun internal-send-socket (ip port packet nb)
    (declare (ignore nb))
    (internal-udp-cosi-client-send-request 'comm:close-async-io-state
                                           ip port packet)
    ))

#|
(defun ptst ()
  ;; test requesting a public key
  (spawn
   (lambda ()
     (let* ((port (start-ephemeral-server))
            (ret  (make-return-addr (node-real-ip *my-node*) port)))
       (labels
           ((exit ()
              (become 'do-nothing)
              (unregister-return-addr ret)
              (shutdown-server port)))
         (pr :my-port port)
         #+:LISPWORKS (inspect ret)
         (send *my-node* :public-key ret)
         (recv
           (msg
            (pr :I-got... msg)
            (exit))
           :TIMEOUT 2
           :ON-TIMEOUT
           (progn
             (pr :I-timed-out...)
             (exit))
           ))))
   ))

(defun stst (msg)
  ;; test getting a signature & verifying it
  (spawn
   (lambda ()
     (let* ((port (start-ephemeral-server))
            (ret  (make-return-addr (node-real-ip *my-node*) port)))
       (labels
           ((exit ()
              (become 'do-nothing)
              (unregister-return-addr ret)
              (shutdown-server port)))
         (pr :my-port port)
         #+:LISPWORKS (inspect ret)
         (send *top-node* :cosi ret msg)
         (recv
           ((list :answer (and packet
                               (list :signature xmsg sig)))
            (pr :I-got... packet)
            (pr (format nil "Witnesses: ~A" (logcount (um:last1 sig))))
            (send *my-node* :validate ret msg sig)
            (recv
              (ansv
               (pr :Validation ansv)
               (exit))
              :TIMEOUT 1
              :ON-TIMEOUT
              (pr :timed-out-on-signature-verification)
              (exit)))
           
           (xmsg
            (pr :what!? xmsg)
            (exit))
           
           :TIMEOUT 15
           :ON-TIMEOUT
           (progn
             (pr :I-timed-out...)
             (exit))
           ))))
   ))
|#         

;; --------------------------------------------------------------

(defun make-hmac (msg skey uuid)
  ;; Every packet sent to another node is accompanied by an HMAC that
  ;; is unforgeable. If a MITM attack occurs, the receiving node will
  ;; fail HMAC verification and just drop the incoming packet on the
  ;; floor. So MITM modifications become tantamount to a DOS attack.
  (multiple-value-bind (v vpt) (ed-random-pair)
    (let* ((c   (hash-pt-msg vpt msg))
           (r   (sub-mod *ed-r* v
                         (mult-mod *ed-r* c skey))))
      (list msg r c uuid))))

(defun verify-hmac (quad)
  ;; Every incoming packet is scrutinized for a valid HMAC. If it
  ;; checks out then the packet is dispatched to an operation.
  ;; Otherwise it is just dropped on the floor.
  (when (and (consp quad)
             (= 4 (length quad)))
    (destructuring-bind (msg r c uuid) quad
      (let* ((node (gethash uuid *uuid-node-tbl*))
             (pkey (node-pkey node))
             (vpt  (ed-add (ed-nth-pt r)
                           (ed-mul pkey c)))
             (cc   (hash-pt-msg vpt msg)))
        (values msg (= cc c))
        ))))

;; --------------------------------------------------------------------
;; Message handlers for verifier nodes

(defun node-validate-cosi (node msg sig)
  ;; toplevel entry for Cosi signature validation checking
  (declare (ignore node)) ;; not needed here...
  (destructuring-bind (c r ids) sig
    (let* ((tkey  (reduce (lambda (ans node)
                            (if (and node
                                     (logbitp (node-bit node) ids))
                                (ed-add ans (node-pkey node))
                              ans))
                          *node-bit-tbl*
                          :initial-value (ed-neutral-point)))
           (vpt  (ed-add (ed-nth-pt r)
                         (ed-mul tkey c)))
           (h    (hash-pt-msg vpt msg)))
      (= h c))
    ))

;; -----------------------------------------------------------------------

#-:LISPWORKS
(defparameter *dly-instr*
  (ac:make-actor
   (lambda (&rest args)
     (declare (ignore args))
     t)))

#+:LISPWORKS
(defparameter *dly-instr*
  (ac:make-actor
   (let ((data   nil)
         (pltsym :plt))
     (um:dlambda
       (:incr (dly)
        #+:LISPWORKS
        (push dly data))
       (:clr ()
        (setf data nil))
       (:pltwin (sym)
        (setf pltsym sym))
       (:plt ()
        #+:LISPWORKS
        (plt:histogram pltsym data
                       :clear  t
                       :ylog   t
                       :xrange '(0 1.2)
                       :thick  2
                       ;; :cum    t
                       :norm   nil
                       :title  "Measured Delay Ratios"
                       :xtitle "Delay-Ratio"
                       :ytitle "Counts")
        (plt:plot pltsym '(1 1) '(0.1 1e6)
                  :color :red))
       ))))

;; -----------------------------------------------------------------------

(defun msg-ok (msg node)
  (declare (ignore msg))
  (not (node-byz node))) ;; for now... should look at node-byz to see how to mess it up

(defun mark-node-no-response (node sub)
  (declare (ignore node sub)) ;; for now...
  nil)

(defun mark-node-corrupted (node sub)
  (declare (ignore node)) ;; for now...
  (setf (node-bad sub) t)
  nil)

;; -----------------------

(defun clear-bad ()
  (send-real-nodes :reset))

(defun node-reset-nodes (node)
  (declare (ignore node))
  (loop for node across *node-bit-tbl* do
        (setf (node-bad node) nil)))

;; ---------------

(defun send-subs (node &rest msg)
  (iter-subs node (lambda (sub)
                    (apply 'send sub msg))))

(defun group-subs (node)
  (um:accum acc
    (iter-subs node #'acc)))

(defun send-real-nodes (&rest msg)
  (loop for ip in *real-nodes* do
        (apply 'send (gethash ip *ip-node-tbl*) msg)))

;; -----------------------------------------------------------------------

(defun sub-commitment (my-ip msg seq-id)
  (=lambda (node)
    (let ((start    (get-universal-time))
          (timeout  10
                    ;; (* (node-load node) *default-timeout-period*)
                    )
          (ret-addr (make-return-addr my-ip)))
      (send node :commitment ret-addr msg seq-id)
      (labels
          ((!dly ()
             #+:LISPWORKS
             (send *dly-instr* :incr
                   (/ (- (get-universal-time) start)
                      timeout)))

           (=return (val)
             (!dly)
             (unregister-return-addr ret-addr)
             (=values val))
               
           (wait ()
             (recv
               ((list* :commit sub-seq ans)
                (if (eql sub-seq seq-id)
                    (=return ans)
                  (wait)))
               
               (_
                (wait))
                 
               :TIMEOUT timeout
               :ON-TIMEOUT
               (progn
                 (pr (format nil "SubCommitment timeout waiting for ~A" (node-ip node)))
                 (=return nil))
               )))
        (wait))
      )))

(defun node-cosi-commitment (node reply-to msg seq-id)
  ;;
  ;; First phase of Cosi:
  ;;   Generate a fresh random ECC pair: (v, v*G)
  ;;
  ;;   Decide if msg warrants a commitment. If so add our contribution
  ;;   to the random challenge, hold on to the secret seed, v. Collect
  ;;   contributions from group members and add to the random
  ;;   challenge ECC pt.
  ;;
  ;;   Compute local validity challenges for all the group members.
  ;; 
  ;;   Return both the accumulated random point and our particular
  ;;   point for use in the local validity challenge.
  ;;
  (multiple-value-bind (v vpt) (ed-random-pair)
    (let* ((subs   (remove-if 'node-bad (group-subs node)))
           (ok     (msg-ok msg node))
           (bits   (if ok (node-bitmap node) 0))
           (vsum   (if ok vpt (ed-neutral-point))))
      (setf (node-seq   node) seq-id
            (node-parts node) nil
            (node-v     node) v
            (node-ok    node) ok) ;; indicate our participation in phase 1
      (=bind (lst)
          (pmapcar (sub-commitment (node-real-ip node) msg seq-id) subs)
        (labels
            ((fold-answer (ans sub)
               (cond
                ((null ans)
                 (pr (format nil "No commitmemt: ~A" (node-ip sub)))
                 (mark-node-no-response node sub))
                
                (t
                 (destructuring-bind (sub-ptsum sub-pt sub-bits) ans
                   ;; fold in the subtree answer
                   (setf bits (logior bits sub-bits)
                         vsum (ed-add vsum (ed-decompress-pt sub-ptsum)))
                   ;; compute a random challenge for node validity checking
                   (let ((chk (hash-pt-msg (ed-decompress-pt sub-pt) msg)))
                     ;; accumulate participants for phase 2
                     (push (list sub msg chk) (node-parts node))
                     )))
                )))
          (mapc #'fold-answer lst subs)
          (send reply-to :commit seq-id (ed-compress-pt vsum) (ed-compress-pt vpt) bits)
          )))))

;; ------------------------------

(defun sub-signing (my-ip c seq-id)
  (=lambda (node chk)
    (let ((start    (get-universal-time))
          (timeout  10
                    ;; (* (node-load node) *default-timeout-period*)
                    )
          (ret-addr (make-return-addr my-ip)))
      (send node :signing ret-addr c chk seq-id)
      (labels
          ((!dly ()
                 #+:LISPWORKS
                 (send *dly-instr* :incr
                       (/ (- (get-universal-time) start)
                          timeout)))

               (=return (val)
                 (!dly)
                 (unregister-return-addr ret-addr)
                 (=values val))
               
               (wait ()
                 (recv
                   ((list* :signed sub-seq ans)
                    (if (eql sub-seq seq-id)
                        (=return ans)
                      ;; else
                      (wait)))
                   
                   ((list (or :missing-node
                              :invalid-commitment) sub-seq)
                    (if (eql sub-seq seq-id)
                        (=return nil)
                      ;; else
                      (wait)))
                   
                   (_
                    (wait))
                   
                   :TIMEOUT timeout
                   :ON-TIMEOUT
                   (progn
                     (pr (format nil "SubSigning timeout waiting for ~A" (node-ip node)))
                     (=return nil))
                   )))
        (wait))
      )))
  
(defun node-cosi-signing (node reply-to c c1 seq-id)
  ;;
  ;; Second phase of Cosi:
  ;;   Given challenge value c, compute the signature value
  ;;     r = v - c * skey.
  ;;   If we decided against signing in the first phase,
  ;;   then we shouldn't even be called
  ;;
  (cond
   ((and (integerp c)  ;; valid setup for phase 2?
         (integerp c1)
         (eql seq-id (node-seq node)))
    (labels
        ((compute-signage (challenge)
           (sub-mod *ed-r* (node-v node)
                    (mult-mod *ed-r* challenge (node-skey node)))))
      
      (let* ((ok      (node-ok node)) ;; did we participate in phase 1?
             (subs    (mapcar 'first (node-parts node)))
             (chks    (mapcar 'third (node-parts node)))
             (rsum    (if ok (compute-signage c) 0))
             (r1      (compute-signage c1))
             (missing nil))
        (setf (node-v   node) nil ;; done with these
              (node-seq node) nil)
        (=bind (r-lst)
            (pmapcar (sub-signing (node-real-ip node) c seq-id) subs chks)
          (labels
              ((fold-answer (sub-rs sub-chk)
                 (destructuring-bind (sub msg chk) sub-chk
                   (cond
                    ((null sub-rs)
                     ;; no response from node, or bad subtree
                     (pr (format nil "No signing: ~A" sub))
                     (mark-node-no-response node sub)
                     (setf missing t))
                    
                    (t
                     (destructuring-bind (sub-r sub-r1) sub-rs
                       ;; first validate the sub
                       (if (node-validate-cosi node msg (list chk sub-r1 (node-bitmap sub)))
                           (unless missing
                             ;; sub was ok, but if we had some missing
                             ;; subs, don't waste time computing
                             ;; anything
                             (setf rsum (add-mod *ed-r* rsum sub-r)))
                         (progn
                           ;; sub gave a corrupt answer on the local challenge
                           (pr (format nil "Corrupt node: ~A" (node-ip sub)))
                           (mark-node-corrupted node sub)
                           (setf missing t))
                         )))
                    ))))
            (mapc #'fold-answer r-lst (node-parts node))
            (if missing
                (send reply-to :missing-node seq-id)
              (send reply-to :signed seq-id rsum r1))
            )))))
   
   (t ;; else -- bad args
      (send reply-to :invalid-commitment seq-id)) ;; request restart
   ))

;; -----------------------------------------------------------

(defun node-compute-cosi (node reply-to msg)
  ;; top-level entry for Cosi signature creation
  ;; assume for now that leader cannot be corrupted...
  (let ((sess (gen-uuid-int))
        (self (current-actor)))
    (ac:self-call :commitment self msg sess)
    (labels
        ((unknown-message (msg)
           (error "Unknown message: ~A" msg))
         
         (wait-commitment ()
           (recv
             ((list :commit seq vpt vsubpt bits)
              (cond
               ((eql seq sess)
                ;; compute global challenge                         
                (let ((c  (hash-pt-msg (ed-decompress-pt vpt)    msg))
                      (c1 (hash-pt-msg (ed-decompress-pt vsubpt) msg)))
                  (ac:self-call :signing self c c1 sess)
                  (labels
                      ((wait-signing ()
                         (recv
                           ((list :signed seq r r1)
                            (declare (ignore r1))
                            (cond
                             ((eql seq sess)
                              (let ((sig (list c r bits)))
                                (if (node-validate-cosi node msg sig)
                                    ;; we completed successfully
                                    (reply reply-to
                                           (list :signature msg sig))
                                  ;; bad signature, try again
                                  (reply reply-to :corrupt-cosi-network)
                                  )))

                             (t ;; seq mismatch
                              ;; must have been a late arrival
                              (wait-signing))
                             ))
                                
                           ((list :missing-node seq)
                            (cond
                             ((eql seq sess)
                              ;; retry from start
                              (pr "Witness dropout, signing restart")
                              (node-compute-cosi node reply-to msg))

                             (t ;; seq mismatch
                              ;; must have been a late arrival
                              (wait-signing))
                             ))
                           
                           ((list :invalid-commitment seq)
                            (cond
                             ((eql seq sess)
                              (pr "Invalid commitment, signing restart")
                              (node-compute-cosi node reply-to msg))
                             
                             (t ;; seq mismatch
                              ;; must have been a late arrival
                              (wait-signing))
                             ))
                           
                           (msg ;; other messages during signing phase
                            (unknown-message msg))
                           )))
                    (wait-signing))
                  )) ;; end of big COND clause
               ;; ------------------------------------
               (t ;; seq mismatch
                ;; must have been a late arrival
                (wait-commitment))
               )) ;; end of message pattern
             ;; ---------------------------------
             (msg ;; other messages during commitment phase
              (unknown-message msg))
             )))
      (wait-commitment))))

#|
;; FOR TESTING!!!

(setup-server)

(set-executive-pool 1)

(setf *real-nodes* (list *leader-node*))

(setf *real-nodes* (remove "10.0.1.13" *real-nodes*
                           :test 'string-equal))

(generate-tree :nel 100)

(reconstruct-tree)
|#

(defun tst ()
  (spawn
   (lambda ()
     (send *dly-instr* :clr)
     (send *dly-instr* :pltwin :histo-4)
     (let ((ret   (make-return-addr (node-real-ip *my-node*)))
           (start (get-universal-time)))
       (labels
           ((exit ()
              (unregister-return-addr ret)))
         (send *top-node* :cosi ret "This is a test message!")
         (recv
           ((list :answer
                  (and msg
                       (list :signature txt
                             (and sig (list _ _ bits)))))
            (send *dly-instr* :plt)
            (ac:pr
             (format nil "Total Witnesses: ~D" (logcount bits))
             msg
             (format nil "Duration = ~A" (- (get-universal-time) start)))
            
            (send *my-node* :validate ret txt sig)
            (recv
              ((list :answer :validation t/f)
               (if t/f
                   (ac:pr :valid-signature)
                 (ac:pr :invalid-signature))
               (exit))
              
              (msg
               (error "ValHuh?: ~A" msg)
               (exit))
              ))
           
           (msg
            (error "Huh? ~A" msg)
            (exit))
           ))))))

;; -------------------------------------------------------------

(defvar *dachshund*  "10.0.1.3")
(defvar *malachite*  "10.0.1.6")
(defvar *rambo*      "10.0.1.13")

(defmethod damage ((ip string) t/f)
  (damage (gethash ip *ip-node-tbl*) t/f))

(defmethod damage ((node node) t/f)
  (setf (node-byz node) t/f))

(defun init-sim ()
  (shutdown-server)
  (reconstruct-tree)
  (start-server))
