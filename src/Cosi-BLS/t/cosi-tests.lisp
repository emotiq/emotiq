
(in-package :cosi-tests)

(define-test basis-consistency
  (assert-true (hash-check range-proofs::*bp-basis* range-proofs::*chk-bp-basis*)))

(define-test cloaked-transaction-consistency
  (let* ((k     (make-key-pair :dave)) ;; genesis keying
         (pkey  (keying-triple-pkey k))
         (skey  (keying-triple-skey k))
       
         (km    (make-key-pair :mary)) ;; Mary keying
         (pkeym (keying-triple-pkey km))
         (skeym (keying-triple-skey km)))
    
    (print "Construct Genesis transaction")
    (let ((trans (make-transaction :ins `((:kind :cloaked
                                           :amount 1000
                                           :gamma  1
                                           :pkey   ,pkey
                                           :skey   ,skey))
                                   :outs `((:kind :cloaked
                                            :amount 750
                                            :pkey   ,pkeym)
                                           (:kind :cloaked
                                            :amount 240
                                            :pkey   ,pkey))
                                   :fee 10)))
      
      (print "Validate transaction")
      (assert-true (validate-transaction trans)) ;; 7.6s MacBook Pro
      
      (print "Find UTX for Mary")
      (let* ((utxm   (find-txout-for-pkey-hash (hash/256 pkeym) trans))
             (minfo  (decrypt-txout-info utxm skeym)))
        
        (print "Construct 2nd transaction")
        (let ((trans (make-transaction :ins `((:kind :cloaked
                                               :amount ,(txout-secr-amt minfo)
                                               :gamma  ,(txout-secr-gamma minfo)
                                               :pkey   ,pkeym
                                               :skey   ,skeym))
                                       :outs `((:kind :cloaked
                                                :amount 240
                                                :pkey   ,pkeym)
                                               (:kind :cloaked
                                                :amount 500
                                                :pkey  ,pkey))
                                       :fee 10)))
          
          (print "Validate 2nd transaction")
          (assert-true (validate-transaction trans))
          )))))

(define-test uncloaked-transaction-consistency
  (let* ((k     (pbc:make-key-pair :dave)) ;; genesis keying
         (pkey  (pbc:keying-triple-pkey k))
         (skey  (pbc:keying-triple-skey k))
         
         (km    (pbc:make-key-pair :mary)) ;; Mary keying
         (pkeym (pbc:keying-triple-pkey km))
         (skeym (pbc:keying-triple-skey km)))
    
    (print "Construct Genesis transaction")
    (let ((trans (make-transaction :ins `((:kind :uncloaked
                                           :amount 1000
                                           :gamma  1
                                           :pkey   ,pkey
                                           :skey   ,skey))
                                   :outs `((:kind :uncloaked
                                            :amount 750
                                            :pkey   ,pkeym)
                                           (:kind :uncloaked
                                            :amount 240
                                            :pkey   ,pkey))
                                   :fee 10)))
      
      (print "Validate transaction")
      (assert-true (validate-transaction trans)) ;; 7.6s MacBook Pro
      
      (print "Find UTX for Mary")
      (let* ((utxm   (find-txout-for-pkey-hash (hash:hash/256 pkeym) trans))
             (amt    (uncloaked-txout-amt utxm))
             (gamma  (uncloaked-txout-gamma utxm)))
        
        (print "Construct 2nd transaction")
        (let ((trans (make-transaction :ins `((:kind :uncloaked
                                               :amount ,amt
                                               :gamma  ,gamma
                                               :pkey   ,pkeym
                                               :skey   ,skeym))
                                       :outs `((:kind :uncloaked
                                                :amount 240
                                                :pkey   ,pkeym)
                                               (:kind :uncloaked
                                                :amount 500
                                                :pkey  ,pkey))
                                       :fee 10)))
          
          (print "Validate 2nd transaction")
          (assert-true (validate-transaction trans))
          )))))

