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
;; (pushnew :rh-testing *features*)

;; ---------------------------------------------------------------

(defun max-byz-fails (ngrp)
  ;; offer up answers in one place so all are on same footing...
  ;; too easy to goof up if you have to recompute them
  (floor (1- ngrp) 2))

;; ----------------------------------------------------------------
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

(defun lagrange-wt (q ns xj)
  (with-mod q
    (let ((num  1)
          (den  1))
      (loop for ix in ns do
            (unless (= ix xj)
              (setf num (m* num ix)
                    den (m* den (- ix xj)))))
      (m/ num den))))

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
;; FOR SIM... Get associations between Node PKeys and their Short PKeys

(defun get-witness-short-keys ()
  (let* ((nodes  (coerce cosi-simgen:*node-bit-tbl* 'list)))
    (mapcar (lambda (node)
              (list (node-pkey node)
                    (node-short-pkey node)))
            nodes)))
                              
;; ------------------------------------------------------------------

(defstruct rh-group-info
  session     ;; session ID for this round
  group       ;; list of pkeys in subgroup
  leader      ;; group leader of group (= first in group)
  super       ;; supervisor of group leader (group leader only)
  graph       ;; Gossip group ID for subgroup
  share-thr   ;; sharing threshold of group
  commits     ;; list of commits seen 
  decr-shares ;; 2D array of decrypted shares, indexed as (owner, witness)
  ;; ----------
  ;; in group leader nodes
  rands       ;; accumulating subgroup randomness
  entropy     ;; list of random vals for each commitment
  ;; -----------
  ;; in beacon node
  beacon-thr  ;; threshold for Beacon reaping
  tentropy    ;; list of entropy from group leaders
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

(defvar *rh-start*  nil) ;; record time of start so we can report run times.

(defun start-randhound-round ()
  ;; The Actor running the election calls this function to start up a
  ;; Randhound session.
  ;;
  ;; Take the list of witness nodes, and partition into a 2-level
  ;; graph with *BEACON* at the top.
  ;;
  ;; All witness nodes are potential participants, including BEACON
  ;; and LEADER just elected. But we limit the number of participats
  ;; to 1600 nodes or fewer.
  ;;
  ;; If fewer than 36 nodes in the network, then make only one group.
  ;; Otherwise, make Sqrt number of groups, of size Sqrt number of
  ;; nodes.
  ;;
  (setf *rh-start* (get-universal-time)) ;; record start time for timings
  
  (let* ((node (current-node))
         (me   (node-pkey node)))

    (when (int= me *beacon*)
      (let* ((witnesses  (get-witness-list))
             (short-keys (get-witness-short-keys))
             (session    (hash/256 *local-epoch* (uuid:make-v1-uuid) *beacon*))
             (grpids     (mapcar (lambda (wit)
                                   (list (first wit) (int (hash/256 session wit))))
                                 witnesses))
             (sorted     (mapcar 'first
                                 (sort grpids '<
                                       :key 'second)))
             (twit       (min 1600 (length sorted)))  ;; total witness nodes
             (tsqrt      (min   40 (isqrt twit)))     ;; their square root
             (swits      (subseq sorted 0 twit))      ;; select out no more than 1600 nodes
             (ngrp       (if (< tsqrt 6)  ;; nbr nodes per group
                             twit
                           tsqrt))
             (grps       (nreverse (um:group swits ngrp)))) ;; actual groups

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
  ;; Every node runs this startup code.
  ;;
  ;; Construct Randhound state for ourselves, compute a shared secret
  ;; randomness along with ZKP proofs on the sharing polynomial
  ;; coefficients and proofs of the computed shares. Send all this
  ;; information to all other nodes in our group.
  ;;
  (when (and (= epoch *local-epoch*)
             (int= from *beacon*)
             (validate-signed-start-message session epoch from groups sig))

    (let* ((node           (current-node))
           (me             (node-pkey node))
           (my-group       (find-if (lambda (grp)
                                      (find me grp :test 'int=))
                                    groups))
           (graph-name     (format nil "beacon-~A" (1+ (position my-group groups))))
           (graph          (establish-broadcast-group my-group :graphID graph-name))
           (group-leader   (first my-group))
           (group-leader-p (int= me group-leader))
           (ngrp           (length my-group)))
      (let* ((byz-fails    (max-byz-fails ngrp))
             (share-thresh (1+ byz-fails)))
        (setf *rh-state* (make-rh-group-info
                          :session     session
                          :group       my-group
                          :leader      group-leader
                          :share-thr   share-thresh
                          :super       (when group-leader-p
                                         *beacon*)
                          :graph       graph
                          :decr-shares (make-hash-table)
                          :rands       (when group-leader-p
                                         (make-hash-table))))

        (when (int= me *beacon*)
          (let* ((tnodes        (loop for grp in groups sum (length grp)))
                 (tfails        (floor (1- tnodes) 3))
                 (grp-fails     (um:nlet-tail iter ((grps   groups)
                                                    (fails  tfails)
                                                    (gfails 0))
                                  
                                  (let* ((grp       (car grps))
                                         (byz-fails (1+ (max-byz-fails (length grp)))))
                                    (if (>= fails byz-fails)
                                        (iter (cdr grps) (- fails byz-fails) (1+ gfails))
                                      gfails)))
                                ))
            (setf (rh-group-info-groups     *rh-state*) groups
                  (rh-group-info-beacon-thr *rh-state*) (1+ grp-fails))
            ))

        (let* ((xvals      (um:range 1 (1+ ngrp)))
               (q          (pbc:get-order))
               (coffs      (loop repeat share-thresh collect (random-between 1 q)))
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
          )))))

;; ----------------------------------------------------------------------------

(defun chk-proofs (proofs chks pkey)
  (every (lambda (proof chk)
           (let ((p1  (compute-pairing proof pkey))
                 (p2  (compute-pairing (get-g1) chk)))
             (int= p1 p2)))
         proofs chks))

(defmethod rh-dispatcher ((msg-sym (eql :subgroup-commit)) &key session from commit)
  ;; Every node in a group produces a randomness commitment along with
  ;; a ZKP on the polynomial coefficients and the values of the secret
  ;; shares provided to all other group members.
  ;;
  ;; This is the code that receives the commitments from other nodes
  ;; and validates them, and then decrypts its particular share of the
  ;; secret. We forward that decrypted share to all other group
  ;; members.
  ;;
  (with-accessors ((my-session  rh-group-info-session)
                   (my-group    rh-group-info-group)
                   (my-commits  rh-group-info-commits)
                   (my-graph    rh-group-info-graph)) *rh-state*
    (with-accessors ((proofs       subgroup-commit-proofs)
                     (chks         subgroup-commit-chks)
                     (encr-shares  subgroup-commit-encr-shares)
                     (rval         subgroup-commit-rval)) commit
      (let* ((node  (current-node))
             (me    (node-pkey node))
             (ngrp  (length my-group)))
            
        (when (and
               (int= session my-session)        ;; correct session?
               (find from my-group :test 'int=) ;; from someone in my group?
               (not (find from my-commits       ;; not-seen yet
                          :test 'int=
                          :key  'first))
               (chk-proofs proofs chks from))   ;; valid collection of proofs?

          (push (list from commit) my-commits)
          (let* ((byz-fails (max-byz-fails ngrp))
                 (share-thr (1+ byz-fails))
                 (q         (pbc:get-order))
                 (kcheck    (- ngrp share-thr))
                 (xvals     (um:range 1 (1+ ngrp)))
                 (coffs     (loop repeat kcheck collect (random-between 1 q)))
                 (rschks    (mapcar (um:curry 'poly q coffs) xvals))
                 (invwts    (mapcar (um:curry 'invwt q ngrp) xvals))
                 (rschkv    (with-mod q
                                      (mapcar 'm/ rschks invwts)))
                 (rschk     (dotprod-g1-zr proofs rschkv)))
            
            #+:rh-testing
            (assert (zerop (int rschk)))
            
            (when (zerop (int rschk))
              (let* ((my-index     (position me my-group
                                             :test 'int=))
                     (my-share     (elt encr-shares my-index))
                     (skey         (node-skey node))
                     (decr-share   (compute-pairing
                                    (mul-pt-zr (pbc:get-g1)
                                               (inv-zr (int skey)))
                                    my-share))
                     (decr-chkl    (compute-pairing
                                    (mul-pt-zr rval (with-mod (get-order)
                                                              (m/ (int skey))))
                                    my-share))
                     (decr-chkr    (compute-pairing (get-g1) (elt chks my-index))))
                ;; ----------------------------------------------------------
                #+:rh-testing
                (assert (int= decr-chkl decr-chkr))
                ;; ----------------------------------------------------------
                
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


(defun reduce-lagrange-interpolate (xy-pairs q)
  ;; xy-pairs is a list of (x y) pairs, where x's are cardinal indexes 1, 2, ...
  ;; and y is a GT field subgroup value computed from some Tate pairing operation.
  ;;
  ;; We combine these pairs by expnentiating each y GT-field value by the Lagrange
  ;; interpolation weight for the x value, and multiply them all
  ;; together to derive a final randomness value.
  ;;;
  (let ((xs (mapcar 'first xy-pairs)))
    (labels ((rand-gt (x y)
               (expt-gt-zr y (lagrange-wt q xs x)))
             (rand-gt-pair (pair)
               (apply #'rand-gt pair)))
      (reduce (lambda (prod pair)
                (mul-gts prod (rand-gt-pair pair)))
              (cdr xy-pairs)
              :initial-value (rand-gt-pair (car xy-pairs))
              ))))

(defmethod rh-dispatcher ((msg-sym (eql :subgroup-decrypted-share)) &key session from for decr-share sig)
  ;; Each node gets this message from other nodes in the group, and
  ;; from themself. Collect the decrypted shares into lists per the
  ;; "for" node which constructed the corresponding commitment.
  ;;
  ;; After a BFT threshold number of decrypted shares for any one
  ;; commitment, combine them using Lagrange interpolation to find the
  ;; secret random value.
  ;;
  (with-accessors ((my-shares       rh-group-info-decr-shares)
                   (my-group-leader rh-group-info-leader)
                   (my-group        rh-group-info-group)
                   (my-session      rh-group-info-session)
                   (share-thresh    rh-group-info-share-thr)) *rh-state*
    (when (and
           (int= session my-session)              ;; correct session?
           (find from my-group :test 'int=)       ;; from member of my group?
           (find for  my-group :test 'int=)       ;; for member of my group?
           (validate-signed-decr-share-message session from for decr-share sig))
      
      (let* ((node         (current-node))
             (me           (node-pkey node))
             (for-key      (int for))
             (for-shares   (gethash for-key my-shares))
             (for-count    (length for-shares)))
        (when (and (< for-count share-thresh)
                   (not (find from for-shares
                              :key  'first
                              :test 'int=)))
          (let ((from-index (1+ (position from my-group :test 'int=))))
            (push (list from from-index decr-share) for-shares)
            (if (= (1+ for-count) share-thresh)
                (let* ((q       (pbc:get-order))
                       ;; since share-thresh = byz-fail + 1, any
                       ;; collection of share-thresh values will
                       ;; include at least one honest random
                       ;; contribution. So just take the first of them
                       ;; in the list.
                       (xypairs (mapcar 'cdr for-shares))
                       (rand    (reduce-lagrange-interpolate xypairs q)))
                  (apply 'send my-group-leader
                         (make-signed-subgroup-randomness-message session for me rand
                                                                  (node-skey node))))
              ;; else
              (setf (gethash for-key my-shares) for-shares))
            ))
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

(defstruct rand-entry
  froms  ;; holds list of contributor pkeys
  rand)  ;; holds accumulating randomness

(defmethod rh-dispatcher ((msg-sym (eql :subgroup-randomness)) &key session for from rand sig)
  ;; Group leaders perform this code.
  ;;
  ;; Collect a BFT Threshold number of decoded secret random values
  ;; for each commitment. We combine that incoming randomness as an
  ;; accumulating product of randomness.
  ;;
  ;; Once a BFT threshold number have been received and accumulated,
  ;; we accumulate that product randomness into another accumulating
  ;; group randomness.  And once a BFT threshold of contributions has
  ;; been made to the group randomness, we send that off to the Beacon
  ;; node.
  ;;
  (with-accessors ((my-super        rh-group-info-super)
                   (my-group        rh-group-info-group)
                   (my-session      rh-group-info-session)
                   (my-rands        rh-group-info-rands)
                   (my-entropy      rh-group-info-entropy)
                   (share-thresh    rh-group-info-share-thr)) *rh-state*
    (let* ((node  (current-node))
           (me    (node-pkey node)))
      (when (and
             my-super                                          ;; I'm a group leader?
             (int= session my-session)                         ;; correct session?
             (find for my-group :test 'int=)                   ;; for commitment in my group
             (find from my-group :test 'int=)                  ;; from witness in my group
             (validate-signed-subgroup-randomness-message session for from rand sig)) ;; valid message?

        (let* ((for-key   (int for))
               (for-rands (gethash for-key my-rands)))
          (cond (for-rands
                 (with-accessors ((for-froms  rand-entry-froms)
                                  (for-rand   rand-entry-rand)) for-rands
                   (let ((for-count (length for-froms)))
                     (unless (or (find from for-froms :test 'int=) ;; ignore duplicates
                                 (>= for-count share-thresh))
                       (push from for-froms)
                       (setf for-rand (mul-gts rand for-rand))
                       (when (= (1+ for-count) share-thresh)
                         (cond (my-entropy
                                (with-accessors ((entropy-froms  rand-entry-froms)
                                                 (entropy-rand   rand-entry-rand)) my-entropy
                                  (let ((entropy-count (length entropy-froms)))
                                    (when (< entropy-count share-thresh)
                                      (push for entropy-froms)
                                      (setf entropy-rand (mul-gts entropy-rand for-rand))
                                      (when (= (1+ entropy-count) share-thresh)
                                        (apply 'send my-super   ;; send to beacon
                                               (make-signed-group-randomness-message session me entropy-rand
                                                                                     (node-skey node))))
                                      ))))
                               (t
                                (setf my-entropy
                                      (make-rand-entry
                                       :froms (list for)
                                       :rand  for-rand)))
                               ))))))
                
                (t
                 (setf (gethash for-key my-rands)
                       (make-rand-entry
                        :froms (list from)
                        :rand  rand)))
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
  ;; forward their composite group randomness
  ;;
  ;; Collectc a BFT Threshold number of group-random values from the
  ;; group-leaders, then fold them into one grand randomness by
  ;; multiplication.
  ;;
  ;; Convert the grand randomness by hash to a shorter value, then
  ;; normalize to a double-precision float value to be used as an
  ;; election seed. Call for an election among all witnesses with that
  ;; seed.
  ;;
  (with-accessors ((groups      rh-group-info-groups)
                   (beacon-thr  rh-group-info-beacon-thr)
                   (tentropy    rh-group-info-tentropy)
                   (my-session  rh-group-info-session)) *rh-state*
    (with-accessors ((entropy-froms  rand-entry-froms)
                     (entropy-rand   rand-entry-rand)) tentropy
      
      (let* ((entropy-count (if tentropy
                                (length entropy-froms)
                              0)))
        (when (and
               groups                            ;; only *BEACON* should have this
               (< entropy-count beacon-thr)      ;; still awaiting contributions?
               (int= session my-session)                         ;; correct session?
               (find from (mapcar 'first groups) :test 'int=) ;; should only arrive from group leaders
               (not (and tentropy                             ;; not already seen?
                         (find from entropy-froms :test 'int=)))
               (validate-signed-group-randomness-message session from rand sig)) ;; valid message?

          (if tentropy
              (progn
                (push from entropy-froms)
                (setf entropy-rand (mul-gts rand entropy-rand)))
            ;; else
            (setf tentropy
                  (make-rand-entry
                   :froms (list from)
                   :rand  rand)))

          (when (= (1+ entropy-count) beacon-thr)
            (let* ((seed  (float (/ (int (hash/256 (rand-entry-rand tentropy)))
                                    #.(ash 1 256))
                                 1d0)))
              ;; ------------------------------------------------------------------
              ;; while debugging, don't run actual elections, uncomment for prodution
              ;; 
              ;; (broadcast+me (make-signed-election-message *beacon* seed (node-skey (current-node))))
              (pr (format nil "~%Hold election from RandHound: ~A" seed))
              (pr (format nil "~%Elapsed Time = ~A" (- (get-universal-time) *rh-start*)))
              ;; ------------------------------------------------------------------
              )))))))
  
;; ------------------------------------------------------------------
