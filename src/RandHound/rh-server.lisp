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

(defparameter *rh-state*  nil)

(defun rh-serve-init (node reply-to config)
  (setf *rh-state* nil)
  (let ((grp  (find-my-group node (session-config-tgrps config))))
    (when grp
      (let* ((ngrp       (length grp))
             (xvals      (um:range 1 (1+ ngrp)))
             (byz-frac   (floor (1- ngrp) 3))
             (kcoffs     (1+ byz-frac))
             (q          (get-order))
             (coffs      (loop repeat kcoffs collect (random-between 1 q)))
             (shares     (mapcar (um:curry 'poly q coffs) xvals))
             (proofs     (mapcar (um:curry 'mul-pt-zr (get-g1)) shares))
             (enc-shares (mapcar 'mul-pt-zr (coerce grp 'list) shares))
             (msg  (list :subgroup-commit
                         (node-assoc-pkey node)
                         (make-subgroup-commit
                          :thresh      kcoffs
                          :encr-shares enc-shares
                          :proofs      proofs))))
        (broadcast-message msg grp)
        (setf *rh-state* (make-rh-server-state
                          :byz-frac    byz-frac
                          :grp         grp
                          :config      config
                          :reply-to    reply-to))
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

  
