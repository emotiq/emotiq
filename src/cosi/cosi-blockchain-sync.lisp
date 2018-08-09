(in-package :emotiq/cosi)


(defvar *max-query-depth*  8)


(defmethod node-dispatcher ((msg-sym (eql :blockchain-head)) &key pkey hash-id sig)
  (when (validate-blockchain-head-message pkey hash-id sig)
    (unless (eql *blockchain* hash-id)
      (setf *blockchain* hash-id)
      (sync-blockchain hash-id))))

(defmethod node-dispatcher ((msg-sym (eql :request-block)) &key reply-to hash-id depth)
  (if-let (blk (gethash hash-id *blocks*))
    (reply reply-to :block-you-requested hash-id blk)
    (when (< depth *max-query-depth*)
      (ask-neighbor-node :request-block ;; else forward to someone else
                         :reply-to reply-to
                         :hash-id hash-id
                         :depth (1+ depth)))))


(defun ask-neighbor-node (&rest msg)
  (let* ((my-pkey (node:pkey (current-node)))
         (nodes (remove my-pkey  ;; avoid asking myself
                        (get-witness-list)
                        :test #'vec-repr:int=)))
    (gossip:singlecast msg (random-elt nodes)
                       :graphID :uber
                       :startNodeID my-pkey)))


(defun validate-block-signature (blk-id blk)
  (and (vec-repr:int= blk-id (block:hash blk)) ;; is it really the block I think it is?
       (check-block-multisig blk)))          ;; does the signature check out?


(defun sync-blockchain (node hash-id &optional (retries 1))
  (when hash-id
    (assert (integerp hash-id))
    (with-current-node node
      (ask-neighbor-node :request-block :reply-to (current-actor) :hash-id hash-id :depth 1)
      (let ((start (get-universal-time))
            (timeout 60))
        (labels ((retry-ask ()
                   (if (>= retries 3)
                       (pr "ERROR - No response to blockchain query for block ~A" hash-id)
                       (sync-blockchain node hash-id (1+ retries)))))
          (recv
           ((list :answer :block-you-requested blk-id blk)
            (with-current-node node
              (assert (integerp blk-id))
              (cond ((and (vec-repr:int= blk-id hash-id)
                          (validate-block-signature blk-id blk)) ;; a valid response?
                     (setf (gethash blk-id *blocks*) blk)
                     (when-let (new-id (block:prev-block-hash blk)) ;; do I have predecessor?
                       (assert (integerp new-id))
                       (unless (gethash new-id *blocks*)
                         ;; NOTE: While the following may appear to be a recursive call,
                         ;; the semantics of ACTORS:RECV ensure that it performs in constant
                         ;; stack space.
                         (sync-blockchain node new-id)))) ;; if not, then ask for it
                    (t
                     ;; wasn't what I asked for, or invalid block response
                     (setf timeout (- (+ start 60) (get-universal-time)))
                     (if (plusp timeout)
                         (retry-recv) ;; maybe node I asked forwarded to another node?
                         (retry-ask))))))
           :TIMEOUT    timeout
           :ON-TIMEOUT (retry-ask)))))))


(defun make-blockchain-comment-message-skeleton (pkey hash-id)
  `(:blockchain-head :pkey ,pkey :hash-id ,hash-id))

(defun make-signed-blockchain-comment-message (pkey hash-id)
  (sign-message (make-blockchain-comment-message-skeleton pkey hash-id)))

(defun send-blockchain-comment ()
  (let ((pkey (node:pkey (current-node))))
    (gossip:broadcast (make-signed-blockchain-comment-message pkey *blockchain*)
                      :graphID :UBER)))

(defun validate-blockchain-head-message (pkey hash-id sig)
  (pbc:check-hash (make-blockchain-comment-message-skeleton pkey hash-id) sig pkey))
