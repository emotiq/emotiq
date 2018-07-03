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

(in-package :randhound/server)
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
  session group leader super graph
  thresh
  commits     
  decr-shares
  rands
  entropy
  groups
  grp-rands)

(defun establish-broadcast-group (pkeys &key graphID)
  (cond (*use-real-gossip*
         (or :UBER ;; for now...
             (gossip:establish-broadcast-group pkeys
                                               :graphID graphID)))

        (t  pkeys)))

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
                                (sort grpids <
                                      :key 'second)))
             (twit      (min 1600 (length sorted)))
             (tsqrt     (min   40 (isqrt twit)))
             (swits     (subseq sorted twit))
             (ngrp      (if (< tsqrt 6)
                            twit
                          tsqrt))
             (grps      (nreverse (um:group swits ngrp))))

        (when (> (length grps) 1)
          ;; absorb short group into last group if fewer than 6 members
          (let ((short-grp (first grps)))
            (when (< (length short-grp) 6)
              (setf grps (nconc short-grp (second grps) (cdddr grps))))))
        
        (broadcast+me (make-signed-start-message session epoch pkey groups
                                                 (node-skey node)))
        ))))

;; ----------------------------------------------------------------------------------

(defun make-start-message-skeleton (session epoch pkey groups)
  `(:randhound :start
    :session session
    :epoch   epoch
    :from    pkey
    :groups  groups
    :sig))

(defun make-signed-start-message (session epoch pkey skey)
  (let ((skel (make-start-message-skeleton session epoch pkey groups)))
    (um:conc1 skel (pbc:sign-hash skel skey))))

(defun validate-signed-start-message (session epoch pkey sig)
  (let ((skel (make-start-message-skeleton session epoch pkey groups)))
    (pbc:check-hash skel sig pkey)))


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
           (bft-thresh   (- ngrp byz-frac)))
      
      (setf *rh-state* (make-rh-group-info
                        :session session
                        :group   my-group
                        :leader  group-leader
                        :thresh  bft-thresh
                        :super   (when (int= me group-leader)
                                   *beacon*)
                        :graph   graph
                        :decr-shares (make-array `(,ngrp ,ngrp))
                        :groups  (when (int= me *beacon*)
                                   groups)))
              
      (let* ((xvals      (um:range 1 (1+ ngrp)))
             (kcoffs     (1+ byz-frac))
             (q          (pbc:get-order))
             (coffs      (loop repeat kcoffs collect (random-between 1 q)))
             (krand      (random-between 1 q))
             (shares     (mapcar (um:curry 'poly q coffs) xvals))
             (proofs     (mapcar (um:compose
                                  (um:curry 'mul-pt-zr (pbc:get-g1))
                                  (let ((sf (with-mod q
                                              (m/ krand (node-skey node)))))
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
             (enc-shares (mapcar 'mul-pt-zr grp shares))
             (rval       (mul-pt-zr (pbc:get-g1) krand))
             (msg        `(:randhound :subgroup-commit
                           :session ,session
                           :from    ,me
                           :commit  ,(make-subgroup-commit
                                      :thresh      kcoffs
                                      :encr-shares enc-shares
                                      :proofs      proofs
                                      :chks        chks
                                      :rval        rval))))

        (broadcast-grp+me msg :graphID graph)
        ))))

;; ----------------------------------------------------------------------------

(defun chk-proofs (proofs chks pkeys)
  (every (lambda (proof chk pkey)
           (let ((p1  (compute-pairing proof pkey))
                 (p2  (compute-pairing (get-g1) chk)))
             (int= p1 p2)))
         proofs chks pkeys))

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
    (when (and
           (int= session my-session)        ;; correct session?
           (find from my-group :test 'int=) ;; from someone in my group?
           (not (find from my-commits          ;; not-seen yet
                      :test 'int=
                      :key  'first))
           (chk-proofs proofs chks my-group)) ;; valid collection of proofs?
      
      (let* ((q        (pbc:get-order))
             (ngrp     (length my-group))
             (byz-frac (floor (1- ngrp) 3))
             (kcheck   (- ngrp byz-frac 1))
             (xvals    (um:range 1 (1+ ngrp)))
             (coffs    (loop repeat kcheck collect (random-between 1 q)))
             (chks     (mapcar (um:curry 'poly q coffs) xvals))
             (invwts   (mapcar (um:curry 'invwt q ngrp) xvals))
             (chkv     (with-mod q
                         (mapcar 'm/ chks invwts)))
             (chk      (dotprod-g1-zr proofs chkv)))

        (when (zerop (int chk))
          (push (list from commit) my-commits)
          (let* ((node         (current-node))
                 (my-pkey      (node-pkey node))
                 (my-index     (position my-pkey my-group
                                       :test 'int=))
                 (sender-index (position from my-group
                                         :test 'int=))
                 (my-share     (elt encr-shares my-index))
                 (skey         (node-skey (current-node)))
                 (decr-share   (compute-pairing
                                (mul-pt-zr (pbc:get-g1)
                                           (inv-zr skey))
                                my-share))
                 (decr-chkl    (compute-pairing
                                (mul-pt-zr rval (with-mod (get-order)
                                                  (m/ skey)))
                                my-share))
                 (decr-chkr    (compute-pairing (get-g1) (elt-chks my-index))))
            (when (int= decr-chkl decr-chkr)
              (broadcast-grp+me (make-signed-decr-share-message
                                 session me from decr-share skey)
                                :graphID my-graph))
            )))))))

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
    (um:conc1 skel (pbc:sign-hash skel skey))))

(defun validate-signed-decr-share-message (session from for decr-share sig)
  (let ((skel (make-decr-share-message-skeleton session from for decr-share)))
    (pbc:check-hash skel sig from)))

(defmethod rh-dispatcher ((msg-sym (eql :subgroup-decrypted-share)) &key session from for decr-share sig)
  (with-accessors ((my-shares       rh-group-info-decr-shares)
                   (my-group-leader rh-group-info-leader)
                   (my-group        rh-group-info-grp)
                   (my-session      rh-group-info-session)
                   (my-commits      rh-group-info-commits)) *rh-state*
    (let* ((me  (node-pkey (current-node))))
      (when (and
             (int= session my-session)              ;; correct session?
             (find from my-group :test 'int=)       ;; from member of my group?
             (find for  my-group :test 'int=)       ;; for member of my group?
             (validate-signed-decr-share-message session from for decr-share sig))

        (let* ((from-index   (position from my-group :test 'int=))
               (for-index    (position for my-group :test 'int=))
               (for-count    (loop for ix from 0 below (length my-group) count
                                   (aref my-shares for-index ix)))
               (commit       (find for my-commits :test 'int= :key 'first))
               (share-thresh (subgroup-commit-thresh commit)))

          (unless (and (< for-count share-thresh)
                       (null (aref my-shares for-index from-index)))
            (setf (aref my-shares for-index from-index) decr-share)
            (when (= (1+ for-count) share-thresh)
              (let* ((ngrp  (length my-group))
                     (lwt   (lambda (ix)
                              (with-mod (pbc:get-order)
                                (m/ ix
                                    (let ((ans 1))
                                      (um:nlet-tail iter ((jx 1))
                                        (if (> jx ngrp)
                                            ans
                                          (progn
                                            (unless (= ix jx)
                                              (setf ans (m* ans (- ix jx))))
                                            (iter (1+ jx))))))
                                    ))))
                     (rand   nil))
                (loop for ix from 0 below ngrp do
                      (let ((yval (aref my-commits for-index ix)))
                        (when yval
                          (let ((yvexpt  (pbc:expt-pairing-zr yval (funcall lwt ix))))
                            (setf rand (if rand
                                           (pbc:mult-pairings rand yvexpt)
                                         yvexpt))))))
                (apply 'send my-group-leader
                       (make-signed-subgroup-randomness-message session for from rand))
                )))
          )))))

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
    (um:conc1 skel (pbc:sign-hash skel skey))))

(defun validate-signed-subgroup-randomness-message (session for from rand sig)
  (let ((skel (make-subgroup-randomness-message-skeleton session for from rand)))
    (pbc:check-hash skel sig from)))

(defmethod rh-dispatcher ((msg-sym (eql :subgroup-randomness)) &key session for from rand sig)
  (with-accessors ((my-shares       rh-group-info-decr-shares)
                   (my-super        rh-group-info-super)
                   (my-group        rh-group-info-grp)
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
                                         (pbc:mult-pairings rand group-rand)
                                       rand))))
            (apply 'send my-super (make-signed-group-randomness-message session me group-rand))
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
    (um:conc1 skel (pbc:sign-hash skel skey))))

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
          (let ((seed  (float (/ (hash/256 (reduce 'logxor my-rands))
                                 #.(ash 1 256))
                              1d0)))
            (broadcast+me (make-signed-election-message *beacon* seed (node-skey (current-node))))
            ))))))

;; ------------------------------------------------------------------
#|
(defstruct rh-server-state
  byz-frac grp reply-to config
  (commits      (make-hash-table))
  (decr-shares  (make-hash-table))
  entropy
  grp-entropy)

(defun rh-serve-validate-commit (node reply-to commit)
  (when (and *rh-state*
             (not (gethash (int reply-to) (rh-server-state-commits *rh-state*))))
    (when (chk-proofs (subgroup-commit-proofs commit)
                      (subgroup-commit-encr-shares commit)
                      (rh-server-state-grp *rh-state*))
      (let* ((q       (get-order))
             (ngrp    (length (rh-server-state-grp *rh-state*)))
             (kcheck  (- ngrp (rh-server-state-byz-frac *rh-state*) 1))
             (xvals   (um:range 1 (1+ ngrp)))
             (coffs   (loop repeat kcheck collect (random-between 1 q)))
             (chks    (mapcar (um:curry 'poly q coffs) xvals))
             (invwts  (mapcar (um:curry 'invwt q ngrp) xvals))
             (chkv    (with-mod q
                                (mapcar 'm/ chks invwts)))
             (chk     (dotprod-g1-zr (subgroup-commit-proofs commit)
                                     chkv)))
        (when (zerop (int chk))
          (setf (gethash (int reply-to) (rh-server-state-commits *rh-state*))
                commit)
          (let* ((my-pkey   (node-assoc-pkey node))
                 (my-grp    (rh-server-state-grp *rh-state*))
                 (my-index  (position (int my-pkey) my-grp
                                      :key 'int))
                 (my-share   (aref (coerce (subgroup-commit-encr-shares commit) 'vector)
                                   my-index))
                 (decr-share (mul-pt-zr my-share
                                        (inv-zr (node-secret-key (node-assoc-pkey node))))))
            (let* ((msg   (list :subgroup-decrypted-share
                                reply-to      ;; pkey of shares generator node
                                my-pkey       ;; who decrypted this share
                                decr-share))) ;; decrypted share (G2 point)
              (broadcast-message msg my-grp)
              )))))))
    
(defun position-in-group (pkey grp)
  (position (int pkey) grp
            :key 'int))

(defun lagrange-interp (q pairs)
  (let* ((xs  (mapcar 'first pairs))
         (pts (mapcar 'rest  pairs)))
    (with-mod q
      (um:nlet-tail iter ((pts  pts)
                          (xs   xs)
                          (ans  nil))
        (if (endp pts)
            ans
          (let* ((xj  (car xs))
                 (pt  (car pts))
                 (den (um:nlet-tail iter ((xis xs)
                                          (ans 1))
                        (if (endp xis)
                            ans
                          (iter (cdr xis)
                                (let ((xi (car xis)))
                                  (if (= xi xj)
                                      ans
                                    (m* ans (m- xj xi)))))
                          )))
                 (pt*z  (mul-pt-zr pt (m/ xj den))))
            (iter (cdr pts) (cdr xs)
                  (if ans
                      (add-pts ans pt*z)
                    pt*z)))
          )))))
              
(defun rh-serve-record-decypted-share (node owner-pkey pkey share)
  (declare (ignore node))
  (let ((commit nil))
    (when (and *rh-state*
               (setf commit (gethash (int owner-pkey) (rh-server-state-commits *rh-state*))))
      (let* ((ipkey   (int pkey))
             (proof   (um:nlet-tail iter ((pkeys  (rh-server-state-grp *rh-state*))
                                          (proofs (subgroup-commit-proofs commit)))
                        (unless (endp pkeys)
                          (if (= (int (car pkeys)) ipkey)
                              (car proofs)
                            (iter (cdr pkeys) (cdr proofs))))
                        )))
        (let ((p1  (compute-pairing proof (get-g2)))
              (p2  (compute-pairing (get-g1) share)))
          (when (= (int p1) (int p2))
            (let* ((decr-shares (rh-server-state-decr-shares *rh-state*))
                   (shares      (cons (cons pkey share) (gethash commit decr-shares)))
                   (thresh      (subgroup-commit-thresh commit)))
              (setf (gethash commit decr-shares) shares)
              (when (>= (length shares) thresh)
                (let* ((grp     (rh-server-state-grp *rh-state*))
                       (pairs   (um:nlet-tail iter ((shs shares)
                                                    (lst nil))
                                  (if (endp shs)
                                      lst
                                    (destructuring-bind (pkey share) (car shs)
                                      (let ((ix (1+ (position-in-group pkey grp))))
                                        (iter (cdr shs) (cons (cons ix share) lst))))
                                    )))
                       (q         (get-order))
                       (entropy   (lagrange-interp q pairs))
                       (byz-frac  (rh-server-state-byz-frac *rh-state*))
                       (thresh    (1+ (* 2 byz-frac))))
                  (push entropy (rh-server-state-entropy *rh-state*))
                  (when (>= (length (rh-server-state-entropy *rh-state*)) thresh)
                    (let ((total-entropy (reduce 'add-pts (rh-server-state-entropy *rh-state*))))
                      (setf (rh-server-state-grp-entropy *rh-state*) total-entropy)
                      (gossip-entropy total-entropy))
                    )))
              )))))))
|#

;; --------------------------------------

  
