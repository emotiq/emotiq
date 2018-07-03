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

(defun rh-dispatch-message (node)
  (ac:make-actor
   (um:dlambda
     (:randhound-init (reply-to config)
      (rh-serve-init node reply-to config))

     (:subgroup-commit (reply-to commit)
      (rh-serve-validate-commit node reply-to commit))

     (:subgroup-decrypted-share (owner-pkey pkey share)
      (rh-serve-record-decypted-share node owner-pkey pkey share))
     
     )))

(defun find-my-group (node grps)
  (let ((limit (length grps))
        (pkey  (int (node-assoc-pkey node))))
    (um:nlet-tail iter ((ix   0))
      (unless (>= ix limit)
        (if (find pkey (aref grps ix)
                  :key 'int)
            (aref grps ix)
          (iter (1+ ix))))
      )))

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

(defstruct rh-server-state
  byz-frac grp reply-to config
  (commits      (make-hash-table))
  (decr-shares  (make-hash-table))
  entropy
  grp-entropy)

(defstruct rh-group-info
  epoch grp leader super graph
  commits     
  decr-shares 
  entropy
  grp-entropy)

(defun establish-broadcast-group (pkeys &key graphID)
  (cond (*use-real-gossip*
         (or :UBER ;; for now...
             (gossip:establish-broadcast-group pkeys
                                               :graphID graphID)))

        (t  pkeys)))

(defmethod rh-dispatcher ((msg-sym (eql :init)) &key reply-to config)
  ;; take the list of witness nodes, and partition into a 2-level
  ;; graph with *BEACON* at the top
  (let* ((witnesses (get-witness-list))
         (seed      (hash/256 *local-epoch* *beacon*))
         (grpids    (mapcar (lambda (wit)
                              (list (first wit) (int (hash/256 seed wit))))
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
         (grps      (nreverse (um:group swits ngrp)))
         (me        (node-pkey (current-node))))

    (when (> (length grps) 1)
      ;; absorb short group into last group if fewer than 6 members
      (let ((short-grp (first grps)))
        (when (< (length short-grp) 6)
          (setf grps (nconc short-grp (second grps) (cdddr grps))))))
    
    (loop for grp in grps
          for ix from 0
          do
          (when (find me grp
                      :test 'int=)
            (let* ((group-leader (first grp))
                   (leader-p     (int= me group-leader))
                   (graph        (establish-broadcast-group grp
                                                            :graphID (format nil "beacon-~A" ix))))
              (setf *rh-state* (make-rh-group-info
                                :epoch  *local-epoch*
                                :grp    grp
                                :leader group-leader
                                :super  (when leader-p
                                          *beacon*)
                                :graph  graph))
              (when leader-p
                (let* ((ngrp       (length grp))
                       (xvals      (um:range 1 (1+ ngrp)))
                       (byz-frac   (floor (1- ngrp) 3))
                       (kcoffs     (1+ byz-frac))
                       (q          (pbc:get-order))
                       (coffs      (loop repeat kcoffs collect (random-between 1 q)))
                       (shares     (mapcar (um:curry 'poly q coffs) xvals))
                       (proofs     (mapcar (um:curry 'mul-pt-zr (pbc:get-g1) shares)))
                       (enc-shares (mapcar 'mul-pt-zr (coerce grp 'list) shares))
                       (msg        (make-subgroup-commit-message-skeleton
                                    me
                                    (make-subgroup-commit
                                     :epoch       *local-epoch*
                                     :thresh      kcoffs
                                     :encr-shares enc-shares
                                     :proofs      proofs)))
                       (sig        (pbc:sign-hash msg (node-skey (current-node)))))
                  
                  (broadcast-grp+me (um:conc msg sig) :graphID graph)
                  )))))
    ))

(defun make-subgroup-commit-message-skeleton (pkey cmt)
  `(:subgroup-commit
    :pkey   ,pkey
    :commit ,cmt
    :sig))

(defmethod rh-dispatcher ((msg-sym (eql :subgroup-commit)) &key pkey commit sig)
    (when (and
           (= (rh-group-info-epoch *rh-state*)
              (subgroup-commit-epoch commit)                   ;; correct session?
           (int= pkey (rh-group-info-leader *rh-state*))       ;; from my group leader?
           (let ((chk  (make-subgroup-commit-message-skeleton pkey commit)))
             (pbc:check-hash chk sig pkey))                    ;; valid message?
           (null (rh-group-info-commits *rh-state*))           ;; not seen yet?
           (chk-proofs (subgroup-commit-proofs commit)
                       (subgroup-commit-encr-shares commit)
                       (rh-group-info-grp *rh-state*)))
      
      (let* ((q        (pbc:get-order))
             (ngrp     (length (rh-group-info-grp *rh-state*)))
             (byz-frac (floor (1- ngrp) 3))
             (kcheck   (- ngrp byz-frac 1))
             (xvals    (um:range 1 (1+ ngrp)))
             (coffs    (loop repeat kcheck collect (random-between 1 q)))
             (chks     (mapcar (um:curry 'poly q coffs) xvals))
             (invwts   (mapcar (um:curry 'invwt q ngrp) xvals))
             (chkv     (with-mod q
                         (mapcar 'm/ chks invwts)))
             (chk      (dotprod-g1-zr (subgroup-commit-proofs commit)
                                      chkv)))
        (when (zerop (int chk))
          (setf (rh-group-info-commits *rh-state*) commit)
          (let* ((node      (current-node))
                 (my-pkey   (node-pkey node))
                 (my-grp    (rh-group-info-grp *rh-state*))
                 (my-index  (position my-pkey my-grp
                                      :test 'int=))
                 (my-share   (aref (coerce (subgroup-commit-encr-shares commit) 'vector)
                                   my-index))
                 (decr-share (mul-pt-zr my-share
                                        (inv-zr (node-skey node))))
                 (msg        (make-subgroup-derypted-share-message-skeleton
                              *local-epoch* pkey my-pkey decr-share)))
            (broadcast-grp+me (um:conc msg
                                       (pbc:sign-hash msg (node-skey node)))
                              :graphID (rh-group-info-graph *rh-state*))
            )))))

(defun make-subgroup-derypted-share-message-skeleton (epoch group-leader my-pkey decr-share)
  `(:subgroup-decrypted-share
    :epoch        epoch
    :group-leader group-leader
    :from-pkey    my-pkey
    :decr-share   decr-share
    :sig))

(defmethod rh-dispatcher ((msg-sym (eql :subgroup-decrypted-share)) &key epoch group-leader from-pkey decr-share sig)
  (with-accessors ((decr-shares     rh-group-info-decr-shares)
                   (my-group-leader rh-group-info-leader)
                   (my-group        rh-group-info-grp)
                   (my-epoch        rh-group-info-epoch)) *rh-state*
    (let* ((ngrp       (length my-group))
           (bft-thresh (- ngrp (floor (1- ngrp) 3))))
      (when (and
             (< (length decr-shares) bft-thresh)
             (= epoch my-epoch)                   ;; correct session?
             (int= group-leader my-leader)          ;; correct group leader?
             (find from-pkey my-group :test 'int=)  ;; from member of my group?
             (let ((chk-msg (make-subgroup-derypted-share-message-skeleton
                             epoch
                             group-leader
                             from-pkey
                             decr-share)))
               (pbc:check-hash chk-msg sig (node-pkey (current-node))))   ;; valid message?
             (not (find from-pkey decr-shares       ;; not seen yet?
                        :test 'int=
                        :key  'first)))
        (push (list from-pkey decr-shares) decr-shares)
        (when (>= (length decr-shares) bft-thresh)
          (send-message *beacon* :subgroup-randomness THE-ANSWER))
        ))))

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

(defun chk-proofs (proofs encr-shares pkeys)
  (every (lambda (proof encr-share pkey)
           (let ((p1  (compute-pairing proof pkey))
                 (p2  (compute-pairing (get-g1) encr-share)))
             (= (int p1) (int p2))))
         proofs encr-shares pkeys))

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
                              

;; --------------------------------------

  
