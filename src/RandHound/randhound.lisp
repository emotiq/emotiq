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

(defvar *randhound-pairing*  :curve-ar160) ;; fast 160-bit symmetric pairing

(defun max-byz-fails (ngrp)
  ;; offer up answers in one place so all are on same footing...
  ;; too easy to goof up if you have to recompute manually
  (floor (1- ngrp) 2))

;; ----------------------------------------------------------------
(defun poly (q coffs x)
  ;; Horner's method for computing polynomials
  ;; Note: coffs in descending order
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
  ;; used in computing Reed-Solomon check vectors
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
  ;; used in performing Lagrange interpolation of shares
  (with-mod q
    (let ((num  1)
          (den  1))
      (loop for ix in ns do
            (unless (= ix xj)
              (setf num (m* num ix)
                    den (m* den (- ix xj)))))
      (m/ num den))))

(defun dotprod-g1-zr (pts zrs)
  ;; form the dot producct between a list of G_1 points and a list of
  ;; Z_r vals. Used to perform the Reed-Solomon check on the
  ;; commitment proofs vector
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

;; V+-- This is the table that should be in consistent sort order
(defvar *node-table* '(1 2)) 

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
       (node-bitmap-table)
       #+nil
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

;; ------------------------------------------------------------
(defvar *cosi-gossip-neighborhood-graph* nil) ;; T if neighborhood graph has been established

(defun ensure-cosi-gossip-neighborhood-graph (my-node)
  (declare (ignore my-node))
  (or *cosi-gossip-neighborhood-graph*
      (setf *cosi-gossip-neighborhood-graph*
            (or :UBER ;; for now while debugging
                (gossip:establish-broadcast-group
                 (mapcar 'first (get-witness-list))
                 :graphID :cosi)))
      ))

(defun gossip-neighborcast (my-node &rest msg)
  "Gossip-neighborcast - send message to all witness nodes."
  (gossip:broadcast msg
                    :style :neighborcast
                    :graphID (ensure-cosi-gossip-neighborhood-graph my-node)))

(defun broadcast+me (msg)
  ;; make sure our own Node gets the message too
  (gossip:singlecast msg
                     :graphID nil) ;; force send to ourselves
  ;; this really should go to everyone
  (gossip:broadcast msg
                    :graphID :UBER))

(defun broadcast-to-others (msg)
  (gossip:broadcast msg
                    :graphID :UBER))

;; -------------------------------------------------------------------
(defun establish-broadcast-group (pkeys &key graphID)
  (or :UBER ;; for now...
      (gossip:establish-broadcast-group pkeys
                                        :graphID graphID)))

(defun broadcast-grp+me (msg &key graphID)
  (gossip:singlecast msg :graphID nil)
  (gossip:broadcast  msg :graphID graphID))

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
;; STAGE 1 - Startup
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
  
  (let* ((node
          (current-node))
         (me
          (node-pkey node)))

    (when (pbc= me *beacon*)
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
             (ngrp       (if (< tsqrt 6)              ;; nbr nodes per group
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
;; STAGE 2 -- Generate Shared Secret Randomness
;; ----------------------------------------------------------------------------------

(defun make-signed-message (msg)
  (append msg (list :sig (pbc:sign-hash msg (node-skey (current-node))))))


(defun make-start-message-skeleton (session epoch pkey groups)
  `(:randhound :start
    :session ,session
    :epoch   ,epoch
    :from    ,pkey
    :groups  ,groups))

(defun make-signed-start-message (session epoch pkey groups)
  (let ((skel (make-start-message-skeleton session epoch pkey groups)))
    (make-signed-message skel)))

(defun validate-signed-start-message (session epoch pkey groups sig)
  (let ((skel (make-start-message-skeleton session epoch pkey groups)))
    (pbc:check-hash skel sig pkey)))


(defstruct subgroup-commit
  encr-shares ;; list of encrypted shares
  proofs      ;; list of share proofs
  kpt)        ;; random generator


(defmethod rh-dispatcher ((msg-sym (eql :start)) &key session epoch from groups sig)
  ;; Every node runs this startup code.
  ;;
  ;; Construct Randhound state for ourselves, compute a shared secret
  ;; randomness along with ZKP proofs on the sharing polynomial
  ;; coefficients and proofs of the computed shares. Send all this
  ;; information to all other nodes in our group.
  ;;
  (when (and (= epoch *local-epoch*)
             (pbc= from *beacon*)
             (validate-signed-start-message session epoch from groups sig))

    (let* ((node           (current-node))
           (me             (node-pkey node))
           (my-group       (find-if (lambda (grp)
                                      (find me grp :test 'pbc= :key 'first))
                                    groups))
           (graph-name     (format nil "beacon-~A" (1+ (position my-group groups))))
           (pkeys          (mapcar 'first my-group))
           (graph          (establish-broadcast-group pkeys :graphID graph-name))
           (group-leader   (caar my-group))
           (group-leader-p (pbc= me group-leader))
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

      (when (pbc= me *beacon*)
        (let* ((tnodes        (loop for grp in groups sum (length grp)))
               (tfails        (floor (1- tnodes) 3))
               (grp-fails     (um:nlet-tail iter ((grps   groups)
                                                  (fails  tfails)
                                                  (gfails 0))
                                ;; count up number of groups that could fail
                                (let* ((grp       (car grps))
                                       (byz-fails (1+ (max-byz-fails (length grp)))))
                                  (if (>= fails byz-fails)
                                      (iter (cdr grps) (- fails byz-fails) (1+ gfails))
                                    gfails)))
                              ))
          (setf (rh-group-info-groups     *rh-state*) groups
                (rh-group-info-beacon-thr *rh-state*) (1+ grp-fails))
          ))

      (let* ((commit (pbc:with-pairing *randhound-pairing*
                       (let* ((q          (pbc:get-order))
                              (xvals      (um:range 1 (1+ ngrp)))
                              (coffs      (loop repeat share-thresh collect (random-between 1 q)))
                              (krand      (random-between 1 q))
                              (kpt        (mul-pt-zr (pbc:get-g1) krand))
                              (shares     (mapcar (um:curry 'poly q coffs) xvals))
                              (proofs     (mapcar (um:curry 'mul-pt-zr kpt) shares))
                              (pkeys      (mapcar 'second my-group))
                              (enc-shares (mapcar 'mul-pt-zr pkeys shares))

                              ;; construct the Reed-Solomon check vector
                              (kcheck    (- ngrp share-thresh))
                              (coffs     (loop repeat kcheck collect (random-between 1 q)))
                              (rschks    (mapcar (um:curry 'poly q coffs) xvals))
                              (invwts    (mapcar (um:curry 'invwt q ngrp) xvals))

                              (my-pos    (position me my-group :test 'pbc= :key 'first)))

                         ;; cache the RS check vector
                         (setf (rh-group-info-rschkv *rh-state*)
                               (with-mod q
                                 (mapcar 'm/ rschks invwts)))

                         ;; stuff our own decrypt in our list of commits
                         ;; this saves about 20% of processing time, since
                         ;; every node does this
                         (push
                          (list me
                                (list (1+ my-pos)
                                      (mul-pt-zr (get-g2) (elt shares my-pos))
                                      (elt proofs my-pos)
                                      kpt))
                          (rh-group-info-commits *rh-state*))
                                                      
                         
                         (make-subgroup-commit
                          :encr-shares enc-shares
                          :proofs      proofs
                          :kpt         kpt))))

             (msg (make-signed-subgroup-commit-message session me commit)))

        (tally :subgroup-commit)
        (broadcast-grp+me msg :graphID graph)
        ))))

;; ----------------------------------------------------------------------------
;; STAGE 3 -- Accumulate Commitments until a super-majority, then show
;; all the decrypted shares
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


(defun validate-commitment (commit pkeys rschkv)
  ;; validate incoming commitment - returns decrypted share if okay
  (let ((kpt         (subgroup-commit-kpt         commit))
        (eshares     (subgroup-commit-encr-shares commit))
        (proofs      (subgroup-commit-proofs      commit))
        (short-pkeys (mapcar 'second pkeys)))
    (pbc:with-pairing *randhound-pairing*
      (and (every (lambda (eshare proof pkey)
                    ;; check pairing relations between encrypted shares and proofs
                    (let ((p1  (compute-pairing proof pkey))
                          (p2  (compute-pairing kpt   eshare)))
                      (pbc= p1 p2)))
                  eshares proofs short-pkeys)
           
           ;; check that we don't have a contstant share setxs
           (not (every (um:curry 'pbc= (car proofs)) (cdr proofs)))
           
           ;; verify consistency of polynomial shares
           (let* ((rschk  (dotprod-g1-zr proofs rschkv)))
             (when (zerop (int rschk))
               ;; provide decrypted-share
               (let ((my-index (position (node-pkey (current-node)) pkeys
                                         :test 'pbc=
                                         :key  'first)))
                 (list (1+ my-index)                        ;; x index
                       (mul-pt-zr (elt eshares my-index)    ;; y share val
                                  (with-mod (get-order)
                                    (m/ (int (node-short-skey (current-node))))))
                       (elt proofs my-index)                ;; proof
                       kpt                                  ;; R pt in G_1
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
  ;; secret.
  ;;
  ;; We stash that decrypted share, and after seeing a super-majority
  ;; of other commitments, we send the whole stash to all other nodes
  ;; in the group.
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
           (barrier-thr (- (1- ngrp) (floor (1- ngrp) 3))) ;; account for me already in commits
           (decr-share (and
                        (not (pbc= from me))        ;; don't process our own commitment
                        (< ncomms barrier-thr)      ;; still awaiting commits?
                        (hash= session my-session)  ;; correct session?
                        (find from my-group         ;; from someone in my group?
                              :test 'pbc=
                              :key  'first)
                        (not (find from my-commits ;; not-seen yet
                                   :test 'pbc=
                                   :key  'first))
                        (validate-signed-subgroup-commit-message session from commit sig) ;; valid message?
                        (validate-commitment commit my-group rschkv))))  ;; valid collection of proofs?
      
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
;; STAGE 4 -- Accumulate decrypted shares until we can perform
;; Lagrange interpolation to unwrap the hidden randomness
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
  ;; validate a decrypted share
  (destructuring-bind (x y w kpt) share
    (declare (ignore x))
    (let ((p1 (compute-pairing w   (get-g2)))
          (p2 (compute-pairing kpt y)))
      (pbc= p1 p2))))  ;; e(W, V) = e(K, D) ?
    
(defmethod rh-dispatcher ((msg-sym (eql :subgroup-decrypted-share)) &key session from shares sig)
  ;; Each node gets this message from other nodes in the group, and
  ;; from themself. Collect the decrypted shares into lists per the
  ;; poynomial identity.
  ;;
  ;; After a BFT threshold number of decrypted shares for any one
  ;; polynomial, combine the hidden randomness at p(0) them using
  ;; Lagrange interpolation.
  ;;
  ;; Shares arrive in batches. As we find polynomials to decode, we
  ;; stash their results until the end of batch processing. Then we
  ;; forward any decoded randomness to the group leader node,
  ;; identifying the polynomial from which the randomness was
  ;; obtained.
  ;;
  (with-accessors ((my-shares       rh-group-info-decr-shares)
                   (my-group-leader rh-group-info-leader)
                   (my-group        rh-group-info-group)
                   (share-thr       rh-group-info-share-thr)
                   (my-session      rh-group-info-session)
                   (share-thresh    rh-group-info-share-thr)) *rh-state*
    (when (and
           (hash= session my-session)                         ;; correct session?
           (find from my-group :test 'pbc= :key 'first)       ;; from member of my group?
           (validate-signed-decr-share-message session from shares sig))
      
      (let* ((node         (current-node))
             (me           (node-pkey node))
             (rands        nil))
        (pbc:with-pairing *randhound-pairing*
          (loop for (poly share) in shares do
                (when (validate-share share) ;; refuse invalid decryptions
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
;; STAGE 6 -- Group Leader accumulates incoming randomness
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
  ;; Decoded randomness arrives in batches, identified by polynomial.
  ;; For each polynomial we accumulate the incoming randomness until a
  ;; sharing threshold of accumulations has occured.
  ;;
  ;; At that point we stop accepting new randomness for the polynomial
  ;; and accumulate its accumulated randomness into a group randomness
  ;; bucket.
  ;;
  ;; After a sharing threshold of group randomness accumulation, we
  ;; stop accepting any more incoming batches of randomness, and
  ;; forward the accumulated group randomness to the Beacon node.
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
             (hash= session my-session)                        ;; correct session?
             (find from my-group :test 'pbc= :key 'first)      ;; from witness in my group
             (validate-signed-subgroup-randomness-message session from rands sig)) ;; valid message?

        (loop for (poly rand) in rands do
              
              (let* ((key   (int poly))
                     (rec   (gethash key my-rands)))
                (cond (rec
                       
                       (assert (not (find from (rand-entry-froms rec)
                                          :test 'pbc=)))

                       (let ((npoly (length (rand-entry-froms rec))))
                         (when (< npoly share-thresh)
                           (tally :incr-polynomial)
                           (push from (rand-entry-froms rec))
                           (with-pairing *randhound-pairing*
                             (setf (rand-entry-rand rec)
                                   ;; (mul-gts (rand-entry-rand rec) rand)
                                   (add-pts (rand-entry-rand rec) rand)))
                           (when (= (1+ npoly) share-thresh)
                             (if my-entropy
                                 (let ((ngrpr (length (rand-entry-froms my-entropy))))
                                   
                                   (assert (not (find poly (rand-entry-froms my-entropy)
                                                      :test 'pbc=)))

                                   (when (< ngrpr share-thresh)
                                     (tally :incr-group)
                                     (push poly (rand-entry-froms my-entropy))
                                     (with-pairing *randhound-pairing*
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
;; STAGE 7 -- Beacon node accumulates group-leader randomness
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
  ;; Collect incoming group randomness into a bucket for Randhound.
  ;; After we accumulate a sharing threshold number of group-random
  ;; values from the group leaders, we stop accepting any more
  ;; randomness, and compute an election seed.
  ;
  ;; Then we broadcast a HOLD-ELECTION message to all witness nodes in
  ;; the blockchain system, supplying that seed. We are finished at
  ;; that point.
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
               groups                                         ;; only *BEACON* should have this
               (< entropy-count beacon-thr)                   ;; still awaiting contributions?
               (hash= session my-session)                     ;; correct session?
               (find from (mapcar 'caar groups) :test 'pbc=)  ;; should only arrive from group leaders
               (not (and tentropy                             ;; not already seen?
                         (find from entropy-froms :test 'pbc=)))
               (validate-signed-group-randomness-message session from rand sig)) ;; valid message?

          (if tentropy
              (progn
                (push from entropy-froms)
                (pbc:with-pairing *randhound-pairing*
                  (setf entropy-rand
                        ;; (mul-gts rand entropy-rand)
                        (add-pts rand entropy-rand))))
            ;; else
            (setf tentropy
                  (make-rand-entry
                   :froms (list from)
                   :rand  rand)))

          (when (= (1+ entropy-count) beacon-thr)
            (let* ((rand  (compute-pairing (get-g1) (rand-entry-rand tentropy))) ;; just to be proper
                   (seed  (float (/ (int (hash/256 rand))
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
