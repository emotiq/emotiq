
(in-package :cosi-simgen)

;; ----------------
;; ANCIENT -- Ancient code that needs revisiting, currently unused... DBM 06/29/18

(defmethod node-dispatcher ((msg-sym (eql :election)) &key new-leader-pkey)
  (emotiq/tracker:track :election)
  (node-elect-new-leader new-leader-pkey))

(defmethod node-dispatcher ((msg-sym (eql :public-key)) &key reply-to)
  (reply reply-to :pkey+zkp (node-pkeyzkp (current-node))))

(defmethod node-dispatcher ((msg-sym (eql :add/change-node)) &key new-node-info)
  (node-insert-node new-node-info))

(defmethod node-dispatcher ((msg-sym (eql :remove-node)) &key node-pkey)
  (node-remove-node node-pkey))

;; -- END of ANCIENT CODE

;; ----------------------------------------------------------------
;; ANCIENT -- Ancient code that needs revisiting, currently unused - DBM 06/29/18

(defun crash-recovery ()
  ;; just in case we need to re-make the Actors for the network
  (maphash (lambda (k node)
             (declare (ignore k))
             (setf (node-self node) (make-node-dispatcher node)))
           *ip-node-tbl*))

;; -------------------------------------------------------
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

(defun node-elect-new-leader (new-leader-pkey)
  (let ((new-top-node (gethash (int new-leader-pkey) *pkey-node-tbl*)))
    ;; Maybe... ready for prime time?
    (cond ((null new-top-node)
           (error "Not a valid leader node: ~A" new-leader-pkey))
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
           (notify-real-descendents new-top-node :election new-leader-pkey))
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
  ;; info is (ipv4 port pkeyzkp)
  (destructuring-bind (ipstr pkeyzkp) new-node-info
    (let* ((ix       (bin-for-ip node ipstr))
           (bins     (node-subs node))
           (sub-node (aref bins ix)))
      (if sub-node
          ;; continue in parallel with our copy of tree
          (node-model-insert-node sub-node new-node-info)
        ;; else
        (let ((new-node (make-node ipstr pkeyzkp node)))
          (setf (node-real-ip new-node)  ipstr
                (node-skey new-node)     nil
                (aref bins ix)           new-node)
          (incf (node-load node))
          (increase-loading (node-parent node)))
        ))))

(defun node-insert-node (new-node-info)
  (destructuring-bind (ipstr port pkeyzkp) new-node-info
    (declare (ignore port)) ;; for now...
    (let* ((node (current-node))
           (new-node (gethash ipstr *ip-node-tbl*)))
      (if new-node ;; already present in tree?
          ;; maybe caller just wants to change keying
          ;; won't be able to sign unless it know skey
          (multiple-value-bind (pkey ok) (check-pkey pkeyzkp)
            (when ok
              (setf (node-pkeyzkp new-node)  pkeyzkp
                    (node-pkey new-node)     pkey ;; cache the decompressed key
                    (node-real-ip new-node)  ipstr)))
        ;; else - not already present
        (node-model-insert-node *top-node* new-node-info))
      (notify-real-descendents node :insert-node new-node-info))))

;; ---------------------------------------------------------

(defun node-model-remove-node (gone-node)
  (remhash (node-ip gone-node) *ip-node-tbl*)
  (let ((pcmpr (keyval (first (node-pkeyzkp gone-node)))))
    (remhash pcmpr *pkey-node-tbl*)
    (remhash pcmpr *pkey-skey-tbl*)))

(defun node-remove-node (gone-node-pkey)
  (let* ((node (current-node))
         (gone-node (gethash (int gone-node-pkey) *pkey-node-tbl*)))
    (when gone-node
      (node-model-remove-node gone-node)
      ;; must rebuild tree to absorb node's subnodes
      (node-model-rebuild-tree nil *top-node*
                               (all-nodes-except *top-node*))
      (when (eq node *top-node*)
        (notify-real-descendents node :remove-node gone-node-pkey)))))
  
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
#|
(defun ptst ()
  ;; test requesting a public key
  (spawn
   (lambda ()
     (let* ((my-ip    (node-real-ip *my-node*))
            (my-port  (start-ephemeral-server))
            (ret      (current-actor)))
         (labels
             ((exit ()
                (become 'do-nothing)
                (shutdown-server my-port)))
           (pr :my-port my-port)
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
     (let* ((my-ip    (node-real-ip *my-node*))
            (my-port  (start-ephemeral-server))
            (ret      (current-actor)))
       (labels
           ((exit ()
              (become 'do-nothing)
              (shutdown-server my-port)))
         (pr :my-port my-port)
         #+:LISPWORKS (inspect ret)
         (send *top-node* :cosi-sign ret msg)
         (recv
           ((list :answer (and packet
                               (list :signature _ sig)))
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
;; -- END OF ANCIENT CODE...

;; ----------------------------------------------------------------------
;; ANCIENT -- ancient code that probably needs revising if ever needed again... DBM 06/29/18
;; -----------------------------------------------------------------------------------
;; Test block assembly and verification...

(defvar *trans1* nil)
(defvar *trans2* nil)
(defvar *genesis* nil)

(defun tst-blk ()
  (reset-system)
  (spawn
   (lambda ()
     (labels
         ((bcast-msg (&rest msg)
            (map nil (lambda (node)
                       (apply 'send (node-pkey node) msg))
                 *node-bit-tbl*))
          (send-tx-to-all (tx)
            (bcast-msg :new-transaction :trn tx))
          (send-genesis-to-all (utxo)
            (bcast-msg :genesis-utxo :utxo utxo))
          (become-witness ()
            (bcast-msg :become-witness)))
       
       (become-witness)
       ;; -------------------------------------------------------------
       ;; manufacture two transactions and send to all nodes
       (if *trans1*
           (progn
             (send-genesis-to-all *genesis*)
             (send-tx-to-all *trans1*)
             (send-tx-to-all *trans2*))
         ;; else
         (let* ((k     (pbc:make-key-pair :dave)) ;; genesis keying
                (pkey  (pbc:keying-triple-pkey k))
                (skey  (pbc:keying-triple-skey k))
                
                (km    (pbc:make-key-pair :mary)) ;; Mary keying
                (pkeym (pbc:keying-triple-pkey km))
                (skeym (pbc:keying-triple-skey km)))
           
           (pr "Construct Genesis transaction")
           (multiple-value-bind (utxog secrg)
               (make-cloaked-txout 1000 pkey)
             (declare (ignore secrg))
             (send-genesis-to-all (setf *genesis* utxog))

             (let* ((minfo (decrypt-txout-info utxog skey))
                    (trans (make-transaction :ins `((:kind :cloaked
                                                     :amount ,(txout-secr-amt minfo)
                                                     :gamma  ,(txout-secr-gamma minfo)
                                                     :pkey   ,pkey
                                                     :skey   ,skey))
                                             :outs `((:kind :cloaked
                                                      :amount 750
                                                      :pkey   ,pkeym)
                                                     (:kind :cloaked
                                                      :amount 240
                                                      :pkey   ,pkey))
                                             :fee 10)))
               
               ;; send TX to all nodes
               (send-tx-to-all (setf *trans1* trans))
               
               (pr "Find UTX for Mary")
               (let* ((utxm   (find-txout-for-pkey-hash (hash:hash/256 pkeym) trans))
                      (minfo  (decrypt-txout-info utxm skeym)))
                 
                 (pr "Construct 2nd transaction")
                 (let ((trans (make-transaction :ins `((:kind :cloaked
                                                        :amount ,(txout-secr-amt minfo)
                                                        :gamma  ,(txout-secr-gamma minfo)
                                                        :pkey   ,pkeym
                                                        :skey   ,skeym))
                                                :outs `((:kind :cloaked
                                                         :amount 250
                                                         :pkey   ,pkeym)
                                                        (:kind :cloaked
                                                         :amount 490
                                                         :pkey  ,pkey))
                                                :fee 10)))
                   ;; send TX to all nodes
                   (send-tx-to-all (setf *trans2* trans))
                   ))))))
       ;; ------------------------------------------------------------------------
       (sleep 10)
       (map nil (lambda (node)
                  (setf (node-current-leader node) (node-pkey *top-node*))
                  (send (node-pkey node) :answer
                        (format nil "Ready-to-run: ~A" (short-id node))))
            *node-bit-tbl*)
       (send *top-node* :become-leader)
       (send *top-node* :make-block)
       ))))

;; -------------------------------------------------------------
;; Test with uncloaked transactions...

(defun tst-ublk ()
  (reset-system)
  (spawn
   (lambda ()
     (labels
         ((bcast-msg (&rest msg)
            (map nil (lambda (node)
                       (apply 'send (node-pkey node) msg))
                 *node-bit-tbl*))
          (send-tx-to-all (tx)
            (bcast-msg :new-transaction :trn tx))
          (send-genesis-to-all (utxo)
            (bcast-msg :genesis-utxo :utxo utxo))
          (become-witness ()
            (bcast-msg :become-witness)))
       
       (become-witness)
       ;; -------------------------------------------------------------
       ;; manufacture two transactions and send to all nodes
       (let* ((k     (pbc:make-key-pair :dave)) ;; genesis keying
              (pkey  (pbc:keying-triple-pkey k))
              (skey  (pbc:keying-triple-skey k))
              
              (km    (pbc:make-key-pair :mary)) ;; Mary keying
              (pkeym (pbc:keying-triple-pkey km))
              (skeym (pbc:keying-triple-skey km)))
         
         (pr "Construct Genesis transaction")
         (multiple-value-bind (utxog secrg)
             (make-uncloaked-txout 1000 pkey)
           (declare (ignore secrg))
           (send-genesis-to-all utxog)
           
           (let* ((amt   (uncloaked-txout-amt utxog))
                  (gamma (uncloaked-txout-gamma utxog))
                  (trans (make-transaction :ins `((:kind :uncloaked
                                                   :amount ,amt
                                                   :gamma  ,gamma
                                                   :pkey   ,pkey
                                                   :skey   ,skey))
                                           :outs `((:kind :uncloaked
                                                    :amount 750
                                                    :pkey   ,pkeym)
                                                   (:kind :uncloaked
                                                    :amount 240
                                                    :pkey   ,pkey))
                                           :fee 10)))
             
             ;; send TX to all nodes
             (send-tx-to-all trans)
             
             (pr "Find UTX for Mary")
             (let* ((utxm   (find-txout-for-pkey-hash (hash:hash/256 pkeym) trans))
                    (amt    (uncloaked-txout-amt utxm))
                    (gamma  (uncloaked-txout-gamma utxm)))
               
               (pr "Construct 2nd transaction")
               (let ((trans (make-transaction :ins `((:kind :uncloaked
                                                      :amount ,amt
                                                      :gamma  ,gamma
                                                      :pkey   ,pkeym
                                                      :skey   ,skeym))
                                              :outs `((:kind :uncloaked
                                                       :amount 250
                                                       :pkey   ,pkeym)
                                                      (:kind :uncloaked
                                                       :amount 490
                                                       :pkey  ,pkey))
                                              :fee 10)))
                 ;; send TX to all nodes
                 (send-tx-to-all trans)
                 )))))
       ;; ------------------------------------------------------------------------
       (sleep 10)
       (map nil (lambda (node)
                  (setf (node-current-leader node) (node-pkey *top-node*))
                  (send (node-pkey node) :answer
                        (format nil "Ready-to-run: ~A" (short-id node))))
            *node-bit-tbl*)
       (send *top-node* :become-leader)
       (send *top-node* :make-block)
       ))))

;; -------------------------------------------------------------

(defvar *arroyo*     "10.0.1.2")
(defvar *dachshund*  "10.0.1.3")
(defvar *malachite*  "10.0.1.6")
(defvar *rambo*      "10.0.1.13")

(defmethod damage ((ip string) t/f)
  (damage (gethash ip *ip-node-tbl*) t/f))

(defmethod damage ((node node) t/f)
  (setf (node-byz node) t/f))

;; -- END of ANCIENT CODE
