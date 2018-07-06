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
  ;; ----------
  ;; in group leader nodes
  rands       ;; accumulating subgroup randomness
  entropy     ;; list of random vals for each commitment
  ;; -----------
  ;; in beacon node
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
;; (defvar *check-rh* (make-hash-table)) ;; holds values of secret randomness for cheching its recovery

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
  ;; (clrhash *check-rh*)
  
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
             (twit      (min 1600 (length sorted)))  ;; total witness nodes
             (tsqrt     (min   40 (isqrt twit)))     ;; their square root
             (swits     (subseq sorted 0 twit))      ;; select out no more than 1600 nodes
             (ngrp      (if (< tsqrt 6)  ;; nbr nodes per group
                            twit
                          tsqrt))
             (grps      (nreverse (um:group swits ngrp)))) ;; actual groups

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
           (ngrp           (length my-group))
           (byz-frac       (floor (1- ngrp) 3))
           (bft-thresh     (- ngrp byz-frac))
           (kcoffs         (1+ byz-frac)))
      
      (setf *rh-state* (make-rh-group-info
                        :session     session
                        :group       my-group
                        :leader      group-leader
                        :thresh      bft-thresh
                        :share-thr   kcoffs
                        :super       (when group-leader-p
                                       *beacon*)
                        :graph       graph
                        :decr-shares (make-hash-table)
                        :rands       (when group-leader-p
                                       (make-hash-table))
                        :groups      (when (int= me *beacon*)
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

        ;; -----------------------------------------------------------
        ;; for checking debugging
        ;; (let ((mykey (int me)))
        ;;   (setf (gethash mykey *check-rh*)
        ;;         (compute-pairing (mul-pt-zr (get-g1) (poly q coffs 0))
        ;;                          (get-g2))))
        ;; ------------------------------------------------------------
        ))))

#|
(let* ((q     (get-order))
       (coffs '(115 32 13))
       (ps    (mapcar (um:curry 'poly  q coffs) '(0 1 2 3)))
       (shs   (mapcar (lambda (p)
                        (compute-pairing (mul-pt-zr (get-g1) p)
                                         (get-g2)))
                      ps))
       (xs    '(1 2 3))
       (pairs (mapcar 'list xs (cdr shs)))
       (chk   (reduce-lagrange-interpolate pairs q)))
  (int= (car shs) chk))

|#

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
                ;; (assert (int= decr-chkl decr-chkr))
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


(defun reduce-lagrange-interpolate (combo q)
  (with-mod q
    (let ((prod nil)
          (ns   (mapcar 'first combo))
          (ys   (mapcar 'second combo)))
      (loop for jx in ns
            for y  in ys
            do
            (let* ((expon (lagrange-wt q ns jx))
                   (yrand (expt-gt-zr y expon)))
              (setf prod (if prod
                             (mul-gts prod yrand)
                           yrand))))
      prod)))


(defun generate-new-combo (xy-pairs npairs ptries)
  (um:nlet-tail again ()
    (um:nlet-tail iter ((pairs xy-pairs)
                        (index 0)
                        (ans   nil))
      (if (>= index npairs)
          (let* ((sorted (sort ans '<
                               :key 'first))
                 (key    (mapcar 'first sorted)))
            (if (find key (car ptries) :test 'equal)
                (again)
              (progn
                (push key (car ptries))
                sorted)))
        ;; else - perform a selection
        (let* ((ix   (random (length pairs)))
               (pair (nth ix pairs)))
          (iter (remove pair pairs)
                (1+ index)
                (cons pair ans)))
        ))
    ))

#|
;; ----------------------------------------------------
;; for debug checking that we got the right answer
(defun check-rh (for rand)
  (let* ((forkey  (int for))
         (chkval  (gethash forkey *check-rh*)))
    (assert (int= chkval rand))))
;; ----------------------------------------------------
|#

(defmethod rh-dispatcher ((msg-sym (eql :subgroup-decrypted-share)) &key session from for decr-share sig)
  ;; Each node gets this message from other nodes in the group, and
  ;; from themself. Collect the decrypted shares into lists per the
  ;; "for" node which constructed the corresponding commitment.
  ;;
  ;; After a BFT threshold number of decrypted shares for any one
  ;; commitment, combine them using Lagrange interpolation to find the
  ;; secret random value.

  ;; But since some decrypts may be bogus, we need to repeat the
  ;; secret extraction over different unique subgroups of the group,
  ;; till we obtain a BFT consensus on the value of the secret. Once
  ;; that consensus is reached, send the secret randomness to the
  ;; group leader.
  ;;
  ;; We use a probabilistic search of the combination space of
  ;; subgroupings - BFT Thresh number of decrypts, taken Sharing
  ;; Thresh number at a time.
  ;;
  ;; The probabilistic search operates by randomly selecting node
  ;; responses, until Sharing Thresh number are obtained, and this
  ;; sorted grouping has not been seen before.
  ;;
  ;; We do this until either a BFT threshold number of such subgroups
  ;; produce the same secret random value, or until we have tried
  ;; twice the BFT threshold number of times. If we exceed the limit,
  ;; then just give up as though BFT consensus could not be reached.
  ;;
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
      
      (let* ((node         (current-node))
             (me           (node-pkey node))
             (for-key      (int for))
             (for-shares   (gethash for-key my-shares))
             (for-count    (length for-shares)))
        (when (and (< for-count thresh)
                   (not (find from for-shares
                              :key  'first
                              :test 'int=)))
          (let ((from-index (1+ (position from my-group :test 'int=))))
            (push (list from from-index decr-share) for-shares)
            (setf (gethash for-key my-shares) for-shares)
            (when (= (1+ for-count) thresh)
              (let* ((q       (pbc:get-order))
                     (xypairs (mapcar 'cdr for-shares))
                     (rands   (make-hash-table))
                     (ptries  (list  nil)))
                (um:nlet-tail iter ((trial 0))
                  (if (> trial (* 2 thresh))
                      (setf ptries nil) ;; give up...
                    ;; else
                    (let* ((combo (generate-new-combo xypairs share-thresh ptries))
                           (rand  (reduce-lagrange-interpolate combo q))
                           (rkey  (int rand))
                           (count (1+ (gethash rkey rands 0))))
                      ;; -----------------------------------
                      ;; for debug checking that we got the right answer
                      ;; (when (int= me (car my-group))
                      ;;   (check-rh for rand))
                      ;; -----------------------------------
                      
                      (setf (gethash rkey rands) count)
                      (if (= count thresh)
                          (setf ptries rand) ;; achieved BFT randomness
                        ;; else - one more time
                        (iter (1+ trial))))
                    ))
                (when ptries
                  (apply 'send my-group-leader
                         (make-signed-subgroup-randomness-message session for me ptries
                                                                  (node-skey node)))
                  ;; ----------------------------------------------------
                  ;; for debug checking that we got the right answer
                  ;; (when (int= me (car my-group))
                  ;;   (check-rh for ptries))
                  ;; ----------------------------------------------------
                  ))))
          )))))

#|
;; ------------------------------------------------------
;; To Prove: (We only need to worry when small numbers are in use)
;;   N Nodes,
;;   MaxFail F = Floor((N-1)/3),
;;   Sharing thresh K = F + 1
;;   BFT Threshold T = N - F
;;   Nbr combos for BFT sharing:
;;      C(T, K) = T! / (K! * (T-K)!)
;;              = C(N-F, F+1)
;;              = (N-F)! / ((F+1)! * (N-2*F-1)!)
;;
;; Look for N where C(T,K) >= 2*T
;;
;; We use probabilistic unique groupings until BFT Threshold
;; derivations of randomness agree with each other, or else a maximum
;; of 2*BFT Threshold number of trials.
;;
;; So look for what conditions assure that number of combos available
;; equal or exceed twice the threshold. Only need to worry for small
;; configurations. By the time we reach 40 nodes in a group, we have
;; more than 20M possible combos.
;;
;; Looks like the minimum configuration is 6 nodes.

(defun factorial (n &optional (nstop 1))
  (declare (integer n nstop))
  (if (zerop n)
      1
    (um:nlet-tail iter ((n n)
                        (ans 1))
      (if (<= n nstop)
          ans
        (iter (1- n) (* n ans))))))
  
(defun combinations (n m)
  ;; combinations of n items, taken m at a time
  (declare (integer n m))
  (assert (>= n m))
  (let ((excess (- n m)))
    (/ (factorial n (max m excess))
       (factorial (min m excess)))))

(loop for n from 1 to 40 do
      (let* ((nfail  (floor (1- n) 3))
             (kshare (1+ nfail))
             (thresh (- n nfail))
             (combos (combinations thresh kshare)))
        (with-standard-io-syntax
          (pprint
           (list :nodes  n
                 :nfail  nfail
                 :kshare kshare
                 :thresh thresh
                 :combos combos)))))

;; ------------------------------------------------------

(let* ((ptries (list nil))
       (pairs  '((1 a) (2 b) (3 c) (4 d))))
  (loop repeat 6 collect
        (generate-new-combo pairs 2 ptries))
  (pprint ptries))

;; ------------------------------------------------------
|#

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
  froms rands)

(defmethod rh-dispatcher ((msg-sym (eql :subgroup-randomness)) &key session for from rand sig)
  ;; Group leaders perform this code.
  ;;
  ;; Collect a BFT Threshold number of decoded secret random values
  ;; for each commitment which agree in value.
  ;;
  ;; After than, assuming a BFT number of commitments have seen a BFT
  ;; threshold number of secrets that agree in value, combine these
  ;; secrets from each commitment into one group-random value and send
  ;; along to Beacon node. We combine by multiplication in the pairing
  ;; group.
  ;;
  (with-accessors ((my-super        rh-group-info-super)
                   (my-group        rh-group-info-group)
                   (my-session      rh-group-info-session)
                   (my-rands        rh-group-info-rands)
                   (my-entropy      rh-group-info-entropy)
                   (my-bft-thresh   rh-group-info-thresh)) *rh-state*
    (let* ((node  (current-node))
           (me    (node-pkey node)))
      (when (and
             my-super                                          ;; I'm a group leader?
             (int= session my-session)                         ;; correct session?
             (find for my-group :test 'int=)                   ;; for commitment in my group
             (find from my-group :test 'int=)                  ;; from witness in my group
             (validate-signed-subgroup-randomness-message session for from rand sig)) ;; valid message?

        (let* ((for-key   (int for))
               (for-rands (gethash for-key my-rands))
               (rkey      (int rand)))
          (cond (for-rands
                 (with-accessors ((for-froms  rand-entry-froms)
                                  (for-counts rand-entry-rands)) for-rands
                   (unless (find from for-froms :test 'int=) ;; ignore duplicates
                     (let* ((count (1+ (gethash rkey for-counts 0)))) ;; incr count for this value
                       (push from for-froms)                          ;; retain record of who submitted it
                       (setf (gethash rkey for-counts) count)
                       (when (= count my-bft-thresh) ;; when one rand values has BFT count
                         (push rand my-entropy)      ;; copy that rand value into entropy list
                         (when (= (length my-entropy) my-bft-thresh) ;; when entropy list has BFT count
                           (let ((group-rand  nil))  ;; compute group entropy
                             (dolist (rand my-entropy)
                               (setf group-rand (if group-rand
                                                    (pbc:mul-gts rand group-rand)
                                                  rand)))
                             (apply 'send my-super   ;; send to beacon
                                    (make-signed-group-randomness-message session me group-rand
                                                                          (node-skey node)))
                             )))))))

                (t
                 (let ((tbl  (make-hash-table)))
                   (setf (gethash rkey tbl)  1
                         (gethash for-key my-rands)
                         (make-rand-entry
                          :froms (list from)
                          :rands tbl))))
                ))
        ))))

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
  (with-accessors ((my-groups   rh-group-info-groups)
                   (my-tentropy rh-group-info-tentropy)
                   (my-session  rh-group-info-session)) *rh-state*
    (let* ((ngrps      (length my-groups))
           (byz-frac   (floor (1- ngrps) 3))
           (bft-thresh (- ngrps byz-frac)))
      
      (when (and
             my-groups  ;; only *BEACON* should have this
             (int= session my-session)                             ;; correct session?
             (find from (mapcar 'first my-groups) :test 'int=)     ;; should only arrive from group leaders
             (not (find from my-tentropy :key 'first :test 'int=)) ;; have not seen this group leader yet?
             (< (length my-tentropy) bft-thresh)                   ;; still awaiting a BFT thresh
             (validate-signed-group-randomness-message session from rand sig)) ;; valid message?

        (push (list from rand) my-tentropy)
        (when (= (length my-tentropy) bft-thresh)
          (let* ((trand (reduce (um:compose 'mul-gts 'second) (cdr my-tentropy)
                                :initial-value (second (car my-tentropy))))
                 (seed  (float (/ (int (hash/256 trand))
                                  #.(ash 1 256))
                               1d0)))
            ;; ------------------------------------------------------------------
            ;; while debugging, don't run actual elections, uncomment for prodution
            ;; 
            ;; (broadcast+me (make-signed-election-message *beacon* seed (node-skey (current-node))))
            (pr (format nil "~%Hold election from RandHound: ~A" seed))
            (pr (format nil "~%Elapsed Time = ~A" (- (get-universal-time) *rh-start*)))
            ;; ------------------------------------------------------------------
            ))))))

;; ------------------------------------------------------------------
