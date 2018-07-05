;; rh-server.lisp -- Randhound Server
;;
;; DM/Emotiq  03/18
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

(in-package :randhound)
;; ---------------------------------------------------------------

(defun poly (q coffs x)
  (with-mod q
    (um:nlet-tail iter ((cs  coffs)
                        (ans 0))
      (if (endp cs)
          ans
        (iter (cdr cs)
              (m+ (car cs)
                  (m* x ans)))))
    ))

(defun invwt (q n xj)
  (with-mod q
    (um:nlet-tail iter ((ix   1)
                        (prod 1))
      (if (> ix n)
          prod
        (iter (1+ ix)
              (if (= xj ix)
                  prod
                (m* prod (m- xj ix))))
        ))))

(defun lagrange-wt (q n xj)
  (with-mod q
    (m/ xj
        (invwt q n xj))))

(defun dotprod-g1-zr (pts zrs)
  (um:nlet-tail iter ((pts  pts)
                      (zrs  zrs)
                      (ans  nil))
    (if (endp pts)
        ans
      (iter (cdr pts) (cdr zrs)
            (let ((p*z  (mul-pt-zr (car pts) (car zrs))))
              (if ans
                  (add-pts ans p*z)
                p*z)))
      )))

;; ------------------------------------------------------------------

(defstruct rh-group-info
  session     ;; session ID for this round
  group       ;; list of pkeys in subgroup
  leader      ;; group leader of group (= first in group)
  super       ;; supervisor of group leader (group leader only)
  graph       ;; Gossip group ID for subgroup
  thresh      ;; BFT threshold group
  share-thr   ;; sharing threshold of group
  commits     ;; list of commits seen 
  decr-shares ;; 2D array of decrypted shares, indexed as (owner, witness)
  rands       ;; accumulating subgroup randomness
  ;; -----------
  groups      ;; Beacon has the list of groups
  grp-rands)  ;; and accumulates group randomness values

(defun establish-broadcast-group (pkeys &key graphID)
  (cond (*use-real-gossip*
         (or :UBER ;; for now...
             (gossip:establish-broadcast-group pkeys
                                               :graphID graphID)))

        (t  pkeys)))

(defun broadcast-grp+me (msg &key graphID)
  (cond (*use-real-gossip*
         (gossip:singlecast msg :graphID nil)
         (gossip:broadcast  msg :graphID graphID))
        
        (t
         (let ((me (node-pkey (current-node))))
           (dolist (pkey (cons me (remove me graphID :test 'int=)))
             (apply 'send pkey msg))))
        ))

;; ------------------------------------------------------------------
;; Start up a Randhound round - called from election central when node is *BEACON*

(defun start-randhound-round ()
  ;; take the list of witness nodes, and partition into a 2-level
  ;; graph with *BEACON* at the top
  (let* ((node (current-node))
         (me   (node-pkey node)))

    (when (int= me *beacon*)
      (let* ((witnesses (get-witness-list))
             (session   (hash/256 *local-epoch* (uuid:make-v1-uuid) *beacon*))
             (grpids    (mapcar (lambda (wit)
                                  (list (first wit) (int (hash/256 session wit))))
                                witnesses))
             (sorted    (mapcar 'first
                                (sort grpids '<
                                      :key 'second)))
             (twit      (min 1600 (length sorted)))
             (tsqrt     (min   40 (isqrt twit)))
             (swits     (subseq sorted 0 twit))
             (ngrp      (if (< tsqrt 6)
                            twit
                          tsqrt))
             (grps      (nreverse (um:group swits ngrp))))

        (when (> (length grps) 1)
          ;; absorb short group into last group if fewer than 6 members
          (let ((short-grp (first grps)))
            (when (< (length short-grp) 6)
              (setf grps (nconc short-grp (second grps) (cdddr grps))))))
        
        (broadcast+me (make-signed-start-message session *local-epoch* me grps
                                                 (node-skey node)))
        ))))

;; ----------------------------------------------------------------------------------

(defun make-start-message-skeleton (session epoch pkey groups)
  `(:randhound :start
    :session ,session
    :epoch   ,epoch
    :from    ,pkey
    :groups  ,groups
    :sig))

(defun make-signed-start-message (session epoch pkey groups skey)
  (let ((skel (make-start-message-skeleton session epoch pkey groups)))
    (um:append1 skel (pbc:sign-hash skel skey))))

(defun validate-signed-start-message (session epoch pkey groups sig)
  (let ((skel (make-start-message-skeleton session epoch pkey groups)))
    (pbc:check-hash skel sig pkey)))


(defstruct subgroup-commit
  encr-shares ;; list of encrypted shares
  proofs      ;; list of share proofs
  chks        ;; list of checks on proofs
  rval)       ;; grand decryption check value

(defmethod rh-dispatcher ((msg-sym (eql :start)) &key session epoch from groups sig)
  (when (and (= epoch *local-epoch*)
             (int= from *beacon*)
             (validate-signed-start-message session epoch from groups sig))

    (let* ((node        (current-node))
           (me          (node-pkey node))
           (my-group    (find-if (lambda (grp)
                                   (find me grp :test 'int=))
                                 groups))
           (graph-name   (format nil "beacon-~A" (1+ (position my-group groups))))
           (graph        (establish-broadcast-group my-group :graphID graph-name))
           (group-leader (first my-group))
           (ngrp         (length my-group))
           (byz-frac     (floor (1- ngrp) 3))
           (bft-thresh   (- ngrp byz-frac))
           (kcoffs       (1+ byz-frac)))
      
      (setf *rh-state* (make-rh-group-info
                        :session     session
                        :group       my-group
                        :leader      group-leader
                        :thresh      bft-thresh
                        :share-thr   kcoffs
                        :super       (when (int= me group-leader)
                                       *beacon*)
                        :graph       graph
                        :decr-shares (make-array `(,ngrp ,ngrp))
                        :groups  (when (int= me *beacon*)
                                   groups)))
              
      (let* ((xvals      (um:range 1 (1+ ngrp)))
             (q          (pbc:get-order))
             (coffs      (loop repeat kcoffs collect (random-between 1 q)))
             (krand      (random-between 1 q))
             (shares     (mapcar (um:curry 'poly q coffs) xvals))
             (proofs     (mapcar (um:compose
                                  (um:curry 'mul-pt-zr (pbc:get-g1))
                                  (let ((sf (with-mod q
                                              (m/ krand (int (node-skey node))))))
                                    (lambda (share)
                                      (with-mod q
                                        (m* sf share)))))
                                 shares))
             (chks       (mapcar (um:compose
                                  (um:curry 'mul-pt-zr (pbc:get-g2))
                                  (lambda (share)
                                    (with-mod q
                                      (m* krand share))))
                                 shares))
             (enc-shares (mapcar 'mul-pt-zr my-group shares))
             (rval       (mul-pt-zr (pbc:get-g1) krand))
             (msg        `(:randhound :subgroup-commit
                           :session ,session
                           :from    ,me
                           :commit  ,(make-subgroup-commit
                                      :encr-shares enc-shares
                                      :proofs      proofs
                                      :chks        chks
                                      :rval        rval))))

        (broadcast-grp+me msg :graphID graph)
        ))))

;; ----------------------------------------------------------------------------

(defun chk-proofs (proofs chks pkey)
  (every (lambda (proof chk)
           (let ((p1  (compute-pairing proof pkey))
                 (p2  (compute-pairing (get-g1) chk)))
             (int= p1 p2)))
         proofs chks))

(defmethod rh-dispatcher ((msg-sym (eql :subgroup-commit)) &key session from commit)
  (with-accessors ((my-session  rh-group-info-session)
                   (my-group    rh-group-info-group)
                   (my-commits  rh-group-info-commits)
                   (my-shares   rh-group-info-decr-shares)
                   (my-graph    rh-group-info-graph)) *rh-state*
    (with-accessors ((proofs       subgroup-commit-proofs)
                     (chks         subgroup-commit-chks)
                     (encr-shares  subgroup-commit-encr-shares)
                     (rval         subgroup-commit-rval)) commit
      (let* ((node  (current-node))
             (me    (node-pkey node)))
            
        (when (and
               (int= session my-session)        ;; correct session?
               (find from my-group :test 'int=) ;; from someone in my group?
               (not (find from my-commits          ;; not-seen yet
                          :test 'int=
                          :key  'first))
               (chk-proofs proofs chks from)) ;; valid collection of proofs?
          
          (let* ((q        (pbc:get-order))
                 (ngrp     (length my-group))
                 (byz-frac (floor (1- ngrp) 3))
                 (kcheck   (- ngrp byz-frac 1))
                 (xvals    (um:range 1 (1+ ngrp)))
                 (coffs    (loop repeat kcheck collect (random-between 1 q)))
                 (rschks   (mapcar (um:curry 'poly q coffs) xvals))
                 (invwts   (mapcar (um:curry 'invwt q ngrp) xvals))
                 (rschkv   (with-mod q
                             (mapcar 'm/ rschks invwts)))
                 (rschk    (dotprod-g1-zr proofs rschkv)))
            
            (when (zerop (int rschk))
              (push (list from commit) my-commits)
              (let* ((my-index     (position me my-group
                                             :test 'int=))
                     (my-share     (elt encr-shares my-index))
                     (skey         (node-skey (current-node)))
                     (decr-share   (compute-pairing
                                    (mul-pt-zr (pbc:get-g1)
                                               (inv-zr (int skey)))
                                    my-share))
                     (decr-chkl    (compute-pairing
                                    (mul-pt-zr rval (with-mod (get-order)
                                                      (m/ (int skey))))
                                    my-share))
                     (decr-chkr    (compute-pairing (get-g1) (elt chks my-index))))
                (when (int= decr-chkl decr-chkr)
                  (broadcast-grp+me (make-signed-decr-share-message
                                     session me from decr-share skey)
                                    :graphID my-graph))
                ))))))))

;; ----------------------------------------------------------------------------

(defun make-decr-share-message-skeleton (session from for decr-share)
  `(:randhound :subgroup-decrypted-share
    :from       ,from
    :for        ,for
    :session    ,session
    :decr-share ,decr-share
    :sig))

(defun make-signed-decr-share-message (session from for decr-share skey)
  (let ((skel (make-decr-share-message-skeleton session from for decr-share)))
    (um:append1 skel (pbc:sign-hash skel skey))))

(defun validate-signed-decr-share-message (session from for decr-share sig)
  (let ((skel (make-decr-share-message-skeleton session from for decr-share)))
    (pbc:check-hash skel sig from)))

(defmethod rh-dispatcher ((msg-sym (eql :subgroup-decrypted-share)) &key session from for decr-share sig)
  (with-accessors ((my-shares       rh-group-info-decr-shares)
                   (my-group-leader rh-group-info-leader)
                   (my-group        rh-group-info-group)
                   (my-session      rh-group-info-session)
                   (thresh          rh-group-info-thresh)
                   (share-thresh    rh-group-info-share-thr)
                   (my-commits      rh-group-info-commits)) *rh-state*
    (when (and
           (int= session my-session)              ;; correct session?
           (find from my-group :test 'int=)       ;; from member of my group?
           (find for  my-group :test 'int=)       ;; for member of my group?
           (validate-signed-decr-share-message session from for decr-share sig))
      
      (let* ((from-index   (position from my-group :test 'int=))
             (for-index    (position for my-group :test 'int=))
             (for-count    (loop for ix from 0 below (length my-group) count
                                 (aref my-shares for-index ix))))

        (when (and (< for-count thresh)
                   (null (aref my-shares for-index from-index)))
          (setf (aref my-shares for-index from-index) decr-share)
          (when (= (1+ for-count) thresh)
            (let* ((ngrp  (length my-group))
                   (q     (pbc:get-order))
                   (rand  nil))
              (loop for ix from 0 below ngrp do
                    (let ((yval (aref my-shares for-index ix)))
                      (when yval
                        (let ((yvexpt  (pbc:expt-gt-zr yval (lagrange-wt q ngrp ix))))
                          (setf rand (if rand
                                         (pbc:mul-gts rand yvexpt)
                                       yvexpt))))))
              (apply 'send my-group-leader
                     (make-signed-subgroup-randomness-message session for from rand
                                                              (node-skey (current-node))))
              )))
        ))))

;; ----------------------------------------------------------------------------

(defun make-subgroup-randomness-message-skeleton (session for from rand)
  `(:randhound :subgroup-randomness
    :session ,session
    :for     ,for
    :from    ,from
    :rand    ,rand
    :sig))

(defun make-signed-subgroup-randomness-message (session for from rand skey)
  (let ((skel (make-subgroup-randomness-message-skeleton session for from rand)))
    (um:append1 skel (pbc:sign-hash skel skey))))

(defun validate-signed-subgroup-randomness-message (session for from rand sig)
  (let ((skel (make-subgroup-randomness-message-skeleton session for from rand)))
    (pbc:check-hash skel sig from)))

(defmethod rh-dispatcher ((msg-sym (eql :subgroup-randomness)) &key session for from rand sig)
  (with-accessors ((my-shares       rh-group-info-decr-shares)
                   (my-super        rh-group-info-super)
                   (my-group        rh-group-info-group)
                   (my-session      rh-group-info-session)
                   (my-commits      rh-group-info-commits)
                   (my-rands        rh-group-info-rands)
                   (my-bft-thresh   rh-group-info-thresh)) *rh-state*
    (let* ((me  (node-pkey (current-node))))
      (when (and
             my-super
             (not (find for my-rands :key 'first :test 'int=))
             (int= session my-session)
             (find for my-group :test 'int=)
             (find from my-group :test 'int=)
             (validate-signed-subgroup-randomness-message session for from rand sig))

        (push (list for rand) my-rands)
        (when (= (length my-rands) my-bft-thresh)
          (let ((group-rand  nil))
            (loop for pair in my-rands do
                  (let ((rand  (second pair)))
                    (setf group-rand (if group-rand
                                         (pbc:mul-gts rand group-rand)
                                       rand))))
            (apply 'send my-super (make-signed-group-randomness-message session me group-rand
                                                                        (node-skey (current-node))))
            ))))))

;; ------------------------------------------------------------------

(defun make-group-randomness-message-skeleton (session from rand)
  `(:randhound :group-randomness
    :session ,session
    :from    ,from
    :rand    ,rand
    :sig))

(defun make-signed-group-randomness-message (session from rand skey)
  (let ((skel (make-group-randomness-message-skeleton session from rand)))
    (um:append1 skel (pbc:sign-hash skel skey))))

(defun validate-signed-group-randomness-message (session from rand sig)
  (let ((skel (make-group-randomness-message-skeleton session from rand)))
    (pbc:check-hash skel sig from)))

(defmethod rh-dispatcher ((msg-sym (eql :group-randomness)) &key session from rand sig)
  ;; this message should only arrive at *BEACON*, as group leaders
  ;; forward their composite randomness
  (with-accessors ((my-groups  rh-group-info-groups)
                   (my-rands   rh-group-info-grp-rands)
                   (my-session rh-group-info-session)) *rh-state*
    (let* ((ngrps      (length my-groups))
           (byz-frac   (floor (1- ngrps) 3))
           (bft-thresh (- ngrps byz-frac)))
      
      (when (and
             my-groups  ;; only *BEACON* should have this
             (int= session my-session)
             (find from (mapcar 'first my-groups) :test 'int=) ;; should only arrive from group leaders
             (< (length my-rands) bft-thresh)
             (validate-signed-group-randomness-message session from rand sig))

        (push rand my-rands)
        (when (= (length my-rands) bft-thresh)
          (let* ((trand (reduce 'mul-gts (cdr my-rands)
                               :initial-value (car my-rands)))
                 (seed  (float (/ (int (hash/256 trand))
                                  #.(ash 1 256))
                               1d0)))
            ;; (broadcast+me (make-signed-election-message *beacon* seed (node-skey (current-node))))
            (pr (format nil "~%Hold election from RandHound: ~A" seed))
            ))))))

;; ------------------------------------------------------------------
