;; rh-server.lisp -- Randhound Server
;;
;; DM/Emotiq  03/18
;; DM/Emotiq  07/18 - significant upgrade and rewrite based on SHAKE
;;
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

(defvar *randhound-curve*  :curve-ar160)

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
  ;; NOTE: It is important that the list of witness keys be kept in
  ;; consistent order among all nodes. Each node has its own local
  ;; copy of this list. But unless the ordering is consistent between
  ;; them, the election outcomes will be seen as different at each
  ;; node.
  (map 'list
       (lambda (node)
         (list (node-pkey node)
               (node-short-pkey node)))
       cosi-simgen:*node-bit-tbl*)) ;; <-- This is the table that should be in consistent sort order
                              
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
  rschkv      ;; precomputed Reed-Solomon check vector
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
;; Debug Instrumentation
(defvar *watcher*
  (make-actor
   (let ((counts (make-hash-table)))
     (lambda (&rest msg)
       (um:dcase msg
         (:reset ()
          (clrhash counts))
         (:read ()
          (um:accum acc
            (maphash (lambda (k v)
                       (acc (list k v)))
                     counts)))
         (:tally (kwsym)
          (let ((ct (gethash kwsym counts 0)))
            (setf (gethash kwsym counts) (1+ ct))))
         )))))

(defun clear-counters ()
  (send *watcher* :reset))

(defun read-counters ()
  (ask *watcher* :read))

(defun tally (kwsym)
  (send *watcher* :tally kwsym))

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
  (clear-counters)
  
  (let* ((node (current-node))
         (me   (node-pkey node)))

    (when (int= me *beacon*)
      (let* ((witnesses  (get-witness-short-keys))
             (session    (hash/256 *local-epoch* (uuid:make-v1-uuid) *beacon*))
             (grpids     (mapcar (lambda (wit)
                                   ;; prepend random lottery ticket to (pkeyLong, pkeyShort)
                                   (cons (int (hash/256 session wit)) wit))
                                 witnesses))
             ;; sort witnesses by lottery ticket, then discard ticket
             (sorted     (mapcar 'rest
                                 (sort grpids '<
                                       :key 'first)))
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

        (tally :start)
        (broadcast+me (make-signed-start-message session *local-epoch* me grps))
        ))))

;; ----------------------------------------------------------------------------------

(defun make-start-message-skeleton (session epoch pkey groups)
  `(:randhound :start
    :session ,session
    :epoch   ,epoch
    :from    ,pkey
    :groups  ,groups))

(defun make-signed-message (msg)
  (append msg (list :sig (pbc:sign-hash msg (node-skey (current-node))))))

(defun make-signed-start-message (session epoch pkey groups)
  (let ((skel (make-start-message-skeleton session epoch pkey groups)))
    (make-signed-message skel)))

(defun validate-signed-start-message (session epoch pkey groups sig)
  (let ((skel (make-start-message-skeleton session epoch pkey groups)))
    (pbc:check-hash skel sig pkey)))


(defstruct subgroup-commit
  encr-shares ;; list of encrypted shares
  proofs      ;; list of share proofs
  rpt)        ;; random generator


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
                                      (find me grp :test 'int= :key 'first))
                                    groups))
           (graph-name     (format nil "beacon-~A" (1+ (position my-group groups))))
           (pkeys          (mapcar 'first my-group))
           (graph          (establish-broadcast-group pkeys :graphID graph-name))
           (group-leader   (caar my-group))
           (group-leader-p (int= me group-leader))
           (ngrp           (length my-group))
           (byz-fails      (max-byz-fails ngrp))
           (share-thresh   (1+ byz-fails)))
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

      (let* ((commit (pbc:with-curve *randhound-curve*
                       (let* ((q          (pbc:get-order))
                              (xvals      (um:range 1 (1+ ngrp)))
                              (coffs      (loop repeat share-thresh collect (random-between 1 q)))
                              (krand      (random-between 1 q))
                              (rpt        (mul-pt-zr (pbc:get-g1) krand))
                              (shares     (mapcar (um:curry 'poly q coffs) xvals))
                              (proofs     (mapcar (um:curry 'mul-pt-zr rpt) shares))
                              (pkeys      (mapcar 'second my-group))
                              (enc-shares (mapcar 'mul-pt-zr pkeys shares))

                              ;; construct the Reed-Solomon check vector
                              (kcheck    (- ngrp share-thresh))
                              (coffs     (loop repeat kcheck collect (random-between 1 q)))
                              (rschks    (mapcar (um:curry 'poly q coffs) xvals))
                              (invwts    (mapcar (um:curry 'invwt q ngrp) xvals)))
                         
                         (setf (rh-group-info-rschkv *rh-state*)
                               (with-mod q
                                 (mapcar 'm/ rschks invwts)))
                         
                         (make-subgroup-commit
                          :encr-shares enc-shares
                          :proofs      proofs
                          :rpt         rpt))))

             (msg (make-signed-subgroup-commit-message session me commit)))

        (tally :subgroup-commit)
        (broadcast-grp+me msg :graphID graph)
        ))))

;; ----------------------------------------------------------------------------

(defun make-subgroup-commit-message-skeleton (session from commit)
  `(:randhound :subgroup-commit
    :from       ,from
    :session    ,session
    :commit     ,commit))

(defun make-signed-subgroup-commit-message (session from commit)
  (let ((skel (make-subgroup-commit-message-skeleton session from commit)))
    (make-signed-message skel)))

(defun validate-signed-subgroup-commit-message (session from commit sig)
  (let ((skel (make-subgroup-commit-message-skeleton session from commit)))
    (pbc:check-hash skel sig from)))


(defun chk-proofs (commit pkeys rschkv)
  (let ((rpt         (subgroup-commit-rpt         commit))
        (eshares     (subgroup-commit-encr-shares commit))
        (proofs      (subgroup-commit-proofs      commit))
        (short-pkeys (mapcar 'second pkeys)))
    (pbc:with-curve *randhound-curve*
      (and (every (lambda (eshare proof pkey)
                    ;; check pairing relations between encrypted shares and proofs
                    (let ((p1  (compute-pairing proof pkey))
                          (p2  (compute-pairing rpt   eshare)))
                      (int= p1 p2)))
                  eshares proofs short-pkeys)
           ;; check that we don't have a contstant share setxs
           (not (every (um:curry 'int= (car proofs)) (cdr proofs)))
           ;; verify consistency of polynomial shares
           (let* ((rschk  (dotprod-g1-zr proofs rschkv)))
             (when (zerop (int rschk))
               ;; provide decrypted-share
               (let ((my-index (position (node-pkey (current-node)) pkeys
                                         :test 'int=
                                         :key  'first)))
                 (list (1+ my-index)
                       #|
                       (compute-pairing (mul-pt-zr (get-g1)
                                                   (with-mod (get-order)
                                                     (m/ (int (node-short-skey (current-node))))))
                                        (elt eshares my-index))
                       |#
                       (mul-pt-zr (elt eshares my-index)
                                  (with-mod (get-order)
                                    (m/ (int (node-short-skey (current-node))))))
                       (elt proofs my-index)
                       rpt
                       )
                 )))
           ))))

(defmethod rh-dispatcher ((msg-sym (eql :subgroup-commit)) &key session from commit sig)
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
                   (rschkv      rh-group-info-rschkv)
                   (my-graph    rh-group-info-graph)) *rh-state*
    (let* ((node        (current-node))
           (me          (node-pkey node))
           (ncomms      (length my-commits))
           (ngrp        (length my-group))
           (barrier-thr (- ngrp (floor (1- ngrp) 3)))
           (decr-share (and
                        (< ncomms barrier-thr)     ;; still awaiting commits?
                        (int= session my-session)  ;; correct session?
                        (find from my-group        ;; from someone in my group?
                              :test 'int=
                              :key  'first)
                        (not (find from my-commits ;; not-seen yet
                                   :test 'int=
                                   :key  'first))
                        (validate-signed-subgroup-commit-message session from commit sig) ;; valid message?
                        (chk-proofs commit my-group rschkv))))  ;; valid collection of proofs?
      
      (when decr-share
        (push (list from decr-share) my-commits)
        (when (= (1+ ncomms) barrier-thr)
          (tally :subgroup-decrypted-share)
          ;; send my decrypted share to all group members
          (broadcast-grp+me (make-signed-decr-share-message
                             session me my-commits)
                            :graphID my-graph)))
      )))

;; ----------------------------------------------------------------------------

(defun make-decr-share-message-skeleton (session from shares)
  `(:randhound :subgroup-decrypted-share
    :from       ,from
    :session    ,session
    :shares     ,shares))

(defun make-signed-decr-share-message (session from shares)
  (let ((skel (make-decr-share-message-skeleton session from shares)))
    (make-signed-message skel)))

(defun validate-signed-decr-share-message (session from shares sig)
  (let ((skel (make-decr-share-message-skeleton session from shares)))
    (pbc:check-hash skel sig from)))


(defun reduce-lagrange-interpolate (xy-pairs)
  ;; xy-pairs is a list of (x y) pairs, where x's are cardinal indexes 1, 2, ...
  ;; and y is a GT field subgroup value computed from some Tate pairing operation.
  ;;
  ;; We combine these pairs by expnentiating each y GT-field value by the Lagrange
  ;; interpolation weight for the x value, and multiply them all
  ;; together to derive a final randomness value.
  ;;;
  (let ((xs (mapcar 'first xy-pairs))
        (q  (get-order)))
    (labels ((rand-gt (x y &rest ignored)
               (declare (ignore ignored))
               ;; (expt-gt-zr y (lagrange-wt q xs x))
               (mul-pt-zr y (lagrange-wt q xs x)))
             (rand-gt-pair (pair)
               (apply #'rand-gt pair)))
      (reduce (lambda (prod pair)
                ;; (mul-gts prod (rand-gt-pair pair))
                (add-pts prod (rand-gt-pair pair)))
              (cdr xy-pairs)
              :initial-value (rand-gt-pair (car xy-pairs))
              ))))

(defun validate-share (share)
  (destructuring-bind (x y w r) share
    (declare (ignore x))
    (let ((p1 (compute-pairing w (get-g2)))
          (p2 (compute-pairing r y)))
      (int= p1 p2))))
    
(defmethod rh-dispatcher ((msg-sym (eql :subgroup-decrypted-share)) &key session from shares sig)
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
                   (share-thr       rh-group-info-share-thr)
                   (my-session      rh-group-info-session)
                   (share-thresh    rh-group-info-share-thr)) *rh-state*
    (when (and
           (int= session my-session)              ;; correct session?
           (find from my-group :test 'int= :key 'first)       ;; from member of my group?
           (validate-signed-decr-share-message session from shares sig))
      
      (let* ((node         (current-node))
             (me           (node-pkey node))
             (rands        nil))
        (pbc:with-curve *randhound-curve*
          (loop for (poly share) in shares do
                (when (validate-share share)
                  (let* ((key  (int poly))
                         (rec  (gethash key my-shares))
                         (nel  (length rec)))
                    (when (< nel share-thr)
                      (setf (gethash key my-shares) (cons share rec))
                      (when (= (1+ nel) share-thr)
                        (push (list poly (reduce-lagrange-interpolate rec)) rands)
                        ))))))
        (when rands
          (tally :subgroup-randomness)
          ;; send decoded randomness to group leader
          (apply 'send my-group-leader
                 (make-signed-subgroup-randomness-message session me rands)))
        ))))

;; ----------------------------------------------------------------------------

(defun make-subgroup-randomness-message-skeleton (session from rands)
  `(:randhound :subgroup-randomness
    :session ,session
    :from    ,from
    :rands   ,rands))

(defun make-signed-subgroup-randomness-message (session from rands)
  (let ((skel (make-subgroup-randomness-message-skeleton session from rands)))
    (make-signed-message skel)))

(defun validate-signed-subgroup-randomness-message (session from rands sig)
  (let ((skel (make-subgroup-randomness-message-skeleton session from rands)))
    (pbc:check-hash skel sig from)))

(defstruct rand-entry
  froms  ;; holds list of contributor pkeys
  rand)  ;; holds accumulating randomness

(defmethod rh-dispatcher ((msg-sym (eql :subgroup-randomness)) &key session from rands sig)
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
                   (my-commits      rh-group-info-commits)
                   (share-thresh    rh-group-info-share-thr)) *rh-state*
    (let* ((node     (current-node))
           (me       (node-pkey node))
           (finished (and my-entropy
                          (>= (length (rand-entry-froms my-entropy))
                              share-thresh))))
      (when (and
             my-super                                          ;; I'm a group leader?
             (not finished)
             (int= session my-session)                         ;; correct session?
             (find from my-group :test 'int= :key 'first)      ;; from witness in my group
             (validate-signed-subgroup-randomness-message session from rands sig)) ;; valid message?

        (loop for (poly rand) in rands do
              
              (let* ((key   (int poly))
                     (rec   (gethash key my-rands)))
                (cond (rec
                       
                       (assert (not (find from (rand-entry-froms rec)
                                          :test 'int=)))

                       (let ((npoly (length (rand-entry-froms rec))))
                         (when (< npoly share-thresh)
                           (tally :incr-polynomial)
                           (push from (rand-entry-froms rec))
                           (with-curve *randhound-curve*
                             (setf (rand-entry-rand rec)
                                   ;; (mul-gts (rand-entry-rand rec) rand)
                                   (add-pts (rand-entry-rand rec) rand)))
                           (when (= (1+ npoly) share-thresh)
                             (if my-entropy
                                 (let ((ngrpr (length (rand-entry-froms my-entropy))))
                                   
                                   (assert (not (find poly (rand-entry-froms my-entropy)
                                                      :test 'int=)))

                                   (when (< ngrpr share-thresh)
                                     (tally :incr-group)
                                     (push poly (rand-entry-froms my-entropy))
                                     (with-curve *randhound-curve*
                                       (setf (rand-entry-rand my-entropy)
                                             ;; (mul-gts (rand-entry-rand my-entropy)
                                             ;;          (rand-entry-rand rec))
                                             (add-pts (rand-entry-rand my-entropy)
                                                      (rand-entry-rand rec))))
                                     (when (= (1+ ngrpr) share-thresh)
                                       (tally :send-to-beacon)
                                       (apply 'send my-super   ;; send to beacon
                                              (make-signed-group-randomness-message session me
                                                                                    (rand-entry-rand my-entropy))))
                                     ))
                               ;; haven't started group entropy accum yet
                               (progn
                                 (tally :incr-group)
                                 (setf my-entropy
                                       (make-rand-entry
                                        :froms (list poly)
                                        :rand  (rand-entry-rand rec))))
                               )))))
                      
                      (t
                       ;; haven't seen anything for this polynomial yet
                       (tally :incr-polynomial)
                       (setf (gethash key my-rands)
                             (make-rand-entry
                              :froms (list from)
                              :rand  rand)))
                      )))))))

;; ------------------------------------------------------------------

(defun make-group-randomness-message-skeleton (session from rand)
  `(:randhound :group-randomness
    :session ,session
    :from    ,from
    :rand    ,rand))

(defun make-signed-group-randomness-message (session from rand)
  (let ((skel (make-group-randomness-message-skeleton session from rand)))
    (make-signed-message skel)))

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
               (find from (mapcar 'caar groups) :test 'int=) ;; should only arrive from group leaders
               (not (and tentropy                             ;; not already seen?
                         (find from entropy-froms :test 'int=)))
               (validate-signed-group-randomness-message session from rand sig)) ;; valid message?

          (if tentropy
              (progn
                (push from entropy-froms)
                (pbc:with-curve *randhound-curve*
                  (setf entropy-rand
                        ;; (mul-gts rand entropy-rand)
                        (add-pts rand entropy-rand))))
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
