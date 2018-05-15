
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
    (multiple-value-bind (utxin info)  ;; spend side
        (make-cloaked-txin 1000 1 pkey skey)
    
      (multiple-value-bind (utxo1 secr1) ;; sends
          (make-cloaked-txout 750 pkeym)
        (multiple-value-bind (utxo2 secr2)
            (make-cloaked-txout 240 pkey)
        
          (let ((trans (make-transaction `(,utxin) `(,info)
                                         `(,utxo1 ,utxo2)
                                         `(,secr1 ,secr2)
                                         :fee 10)))

            (print "Validate transaction")
            (assert-true (validate-transaction trans)) ;; 7.6s MacBook Pro

            (print "Find UTX for Mary")
            (let* ((utxm   (find-txout-for-pkey-hash (hash/256 pkeym) trans))
                   (minfo  (decrypt-txout-info utxm skeym)))

              (print "Construct 2nd transaction")
              (multiple-value-bind (utxin info)  ;; spend side
                  (make-cloaked-txin (txout-secr-amt   minfo)
                                     (txout-secr-gamma minfo)
                                     pkeym skeym)
              
                (multiple-value-bind (utxo1 secr1) ;; sends
                    (make-cloaked-txout 250 pkeym)
                  (multiple-value-bind (utxo2 secr2)
                      (make-cloaked-txout 490 pkey)
                  
                    (let ((trans (make-transaction `(,utxin) `(,info)
                                                   `(,utxo1 ,utxo2)
                                                   `(,secr1 ,secr2)
                                                   :fee 10)))
                      (print "Validate 2nd transaction")
                      (assert-true (validate-transaction trans))
                      )))))
            ))))))

(define-test uncloaked-transaction-consistency
  (let* ((k     (pbc:make-key-pair :dave)) ;; genesis keying
         (pkey  (pbc:keying-triple-pkey k))
         (skey  (pbc:keying-triple-skey k))
         
         (km    (pbc:make-key-pair :mary)) ;; Mary keying
         (pkeym (pbc:keying-triple-pkey km))
         (skeym (pbc:keying-triple-skey km)))
    
    (print "Construct Genesis transaction")
    (multiple-value-bind (utxin info)  ;; spend side
        (make-uncloaked-txin 1000 1 pkey skey)
      
      (multiple-value-bind (utxo1 secr1) ;; sends
          (make-uncloaked-txout 750 pkeym)
        (multiple-value-bind (utxo2 secr2)
            (make-uncloaked-txout 240 pkey)
          
          (let ((trans (make-transaction `(,utxin) `(,info)
                                         `(,utxo1 ,utxo2)
                                         `(,secr1 ,secr2)
                                         :fee 10
                                         :skey skey)))
            (print "Validate transaction")
            (assert-true (validate-transaction trans)) ;; 7.6s MacBook Pro
            
            (print "Find UTX for Mary")
            (let* ((utxm   (find-txout-for-pkey-hash (hash:hash/256 pkeym) trans))
                   (amt    (uncloaked-txout-amt utxm))
                   (gamma  (uncloaked-txout-gamma utxm)))
              
              (print "Construct 2nd transaction")
              (multiple-value-bind (utxin info)  ;; spend side
                  (make-uncloaked-txin amt gamma pkeym skeym)
                
                (multiple-value-bind (utxo1 secr1) ;; sends
                    (make-uncloaked-txout 240 pkeym)
                  (multiple-value-bind (utxo2 secr2)
                      (make-uncloaked-txout 500 pkey)
                    
                    (let ((trans (make-transaction `(,utxin) `(,info)
                                                   `(,utxo1 ,utxo2)
                                                   `(,secr1 ,secr2)
                                                   :fee 10
                                                   :skey skeym)))
                      (print "Validate 2nd transaction")
                      (assert-true (validate-transaction trans))
                      )))))
            ))))))

