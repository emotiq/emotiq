;; cosi-handlers.lisp -- Handlers for various Cosi operations
;;
;; DM/Emotiq  02/18
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

(in-package :cosi-simgen)
;; ---------------------------------------------------------------

(defun NYI (&rest args)
  (error "Not yet implemented: ~A" args))

;; -------------------------------------------------------

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

     (:signing (reply-to c seq)
      (node-cosi-signing node reply-to c seq))

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
#|
(defun ptst ()
  ;; test requesting a public key
  (spawn
   (lambda ()
     (let* ((my-ip    (node-real-ip *my-node*))
            (my-port  (start-ephemeral-server))
            (ret      (make-return-addr my-ip my-port)))
         (labels
             ((exit ()
                (become 'do-nothing)
                (unregister-return-addr ret)
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
            (ret      (make-return-addr my-ip my-port)))
       (labels
           ((exit ()
              (become 'do-nothing)
              (unregister-return-addr ret)
              (shutdown-server my-port)))
         (pr :my-port my-port)
         #+:LISPWORKS (inspect ret)
         (send *top-node* :cosi ret msg)
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

#-(AND :COM.RAL :LISPWORKS)
(defparameter *dly-instr*
  (ac:make-actor
   (lambda (&rest args)
     (declare (ignore args))
     t)))

#+(AND :COM.RAL :LISPWORKS)
(defparameter *dly-instr*
  ;; Very useful for timeout tuning. If timeouts are properly set,
  ;; then histogram will be entirely to left of red 1.0 Ratio, but not
  ;; too far left
  (ac:make-actor
   (let ((data   nil)
         (pltsym :plt))
     (um:dlambda
       (:incr (dly)
        (push dly data))
       (:clr ()
        (setf data nil))
       (:pltwin (sym)
        (setf pltsym sym))
       (:plt ()
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
                 (destructuring-bind (sub-ptsum sub-bits) ans ;; partial V_sun, and V_i
                   ;; fold in the subtree answer
                   (setf bits (logior bits sub-bits)
                         vsum (ed-add vsum (ed-decompress-pt sub-ptsum)))
                   ;; accumulate participants for phase 2
                   (push (list sub sub-ptsum sub-bits) (node-parts node))
                   ))
                )))
          (mapc #'fold-answer lst subs)
          ;; return partial V_sum and V_i for local checking
          (send reply-to :commit seq-id (ed-compress-pt vsum) bits)
          )))))

;; ------------------------------

(defun sub-signing (my-ip c seq-id)
  (=lambda (node)
    (let ((start    (get-universal-time))
          (timeout  10
                    ;; (* (node-load node) *default-timeout-period*)
                    )
          (ret-addr (make-return-addr my-ip)))
      (send node :signing ret-addr c seq-id)
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
                   ((list :signed sub-seq ans)
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

(defun get-summed-pkey (bits)
  (let ((pksum (ed-neutral-point)))
    (loop for node across *node-bit-tbl*
          for bit from 0
          do
          (when (logbitp bit bits)
            (setf pksum (ed-add pksum (node-pkey node)))))
    pksum))

(defun node-validate-member (sub-vpt c sub-r sub-bits)
  (ed-pt= (ed-decompress-pt sub-vpt)
          (ed-add (ed-nth-pt sub-r)
                  (ed-mul (get-summed-pkey sub-bits) c))))

(defun node-cosi-signing (node reply-to c seq-id)
  ;;
  ;; Second phase of Cosi:
  ;;   Given challenge value c, compute the signature value
  ;;     r = v - c * skey.
  ;;   If we decided against signing in the first phase,
  ;;   then we shouldn't even be called
  ;;
  (cond
   ((and (integerp c)  ;; valid setup for phase 2?
         (eql seq-id (node-seq node)))
    (labels
        ((compute-signage (challenge)
           (with-mod *ed-r*
             (m- (node-v node)
                 (m* challenge (node-skey node))))))
      
      (let* ((ok      (node-ok node)) ;; did we participate in phase 1?
             (subs    (mapcar 'first (node-parts node)))
             (rsum    (if ok (compute-signage c) 0))
             (missing nil))
        (setf (node-v   node) nil ;; done with these
              (node-seq node) nil)
        (=bind (r-lst)
            (pmapcar (sub-signing (node-real-ip node) c seq-id) subs)
          (labels
              ((fold-answer (sub-r sub-chk)
                 (destructuring-bind (sub sub-vpt sub-bits) sub-chk
                   (cond
                    ((null sub-r)
                     ;; no response from node, or bad subtree
                     (pr (format nil "No signing: ~A" sub))
                     (mark-node-no-response node sub)
                     (setf missing t))
                    
                    (t
                     ;; first validate the sub using msg and its c_i, r_i
                     (if (node-validate-member sub-vpt c sub-r sub-bits)
                         (unless missing
                           ;; sub was ok, but if we had some missing
                           ;; subs, don't waste time computing
                           ;; anything
                           (with-mod *ed-r*
                             (setf rsum (m+ rsum sub-r))))
                       (progn
                         ;; sub gave a corrupt answer on the local challenge
                         (pr (format nil "Corrupt node: ~A" (node-ip sub)))
                         (mark-node-corrupted node sub)
                         (setf missing t))
                       ))
                    ))))
            (mapc #'fold-answer r-lst (node-parts node))
            (if missing
                (send reply-to :missing-node seq-id)
              (send reply-to :signed seq-id rsum)) ;; return partial r_sum, and our r_i
            )))))
   
   (t ;; else -- bad args
      (send reply-to :invalid-commitment seq-id)) ;; request restart
   ))

;; -----------------------------------------------------------

(defun node-compute-cosi (node reply-to msg)
  ;; top-level entry for Cosi signature creation
  ;; assume for now that leader cannot be corrupted...
  (let ((sess (gen-uuid-int)) ;; strictly increasing sequence of integers
        (self (current-actor)))
    (ac:self-call :commitment self msg sess)
    (labels
        ((unknown-message (msg)
           (error "Unknown message: ~A" msg))
         
         (wait-commitment ()
           (recv
             ((list :commit seq vpt bits)
              (cond
               ((eql seq sess)
                ;; compute global challenge                         
                (let ((c  (hash-pt-msg (ed-decompress-pt vpt) msg)))
                  (ac:self-call :signing self c sess)
                  (labels
                      ((wait-signing ()
                         (recv
                           ((list :signed seq r)
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
