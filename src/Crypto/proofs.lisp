
(in-package :pbc)

#|

Any commitment, written as C = H^x, in bent nomenclature, can be
rewritten in log form as log C = x*log H. where log H = alpha * log G
= alpha, considering group element G as the identity element for
exponentiation, or base of the log i.e., the base generator of the
cryptosystem over the curve on which C and H are defined.

So, a Pedersen commitment of the form C = H^beta * G^x can be
rewritten as log C = beta * log H + x * log G = beta * log H + x, for
independent generators G, H.

In un-bent nomenclature that Pedersen commitment would be written as C
= beta * H + x * G, where H = alpha * G for some randomly selected
alpha. So unbent nomenclature is already in log form.

We share knowledge of H, G, C, and keep x, beta hidden. When asked to
open the commitment for C we rturn x, always keeping randomness beta
hidden.

A commitment in the form C = x*G is computationally binding, but not
hiding, because anyone capable of performing ECDLP can solve for x. A
commitment of the form C' = beta*H + x*G, for random beta and
independent generator H, is both computationally binding and hiding
because the ability to perform ECDLP does not help in the face of
unknmown randomness beta.

To build a proof that x = v, we need to provide a commitment to x and
offer a function of random variable z, such that the challenger cnn
predict the answer to the function F(z) given our commitment, as well
as have us perofm the computation and return an answer that can be
matched, without giving away any information about the binding value
for x.


 |#


(defun make-proof (a1 &optional (a2 0))
  "Proof of a1,a2 with blinding, compute-binding, commitment on a1, a2.
  
    Commitment: C = (a1 + a2-blind) * G1 + (a2 + a2-blind) * G2
    LF = (a2 + a2-blind) * G1
    RT = (a1 + a1-bline) * G2
    publish G1, G2, C, along with LF, RT
  
    Challenge: z = Hash(a1 | a1-blind | a2 | a2-blind | G1 | G2 | LF | RT)
  
    Verifier computes:  G' = 1/z * G1 + z * G2
                and  :  C' = 1/z^2 * LF + C + z^2 * RT
  
    Prover supplies: alpha = (a1 + a1-blind) * z + (a2 + a2-blind) / z
  
    Verifier sees that: alpha * G' = C' "

  (with-mod *ed-r*
    (let* ((a1-blind  (random-between 1 *ed-r*))
           (a1-sum    (m+ a1 a1-blind))
           (a2-blind  (random-between 1 *ed-r*))
           (a2-sum    (m+ a2 a2-blind)))
      (multiple-value-bind (s1 g1) (ed-random-pair)
        (declare (ignore s1))
        (multiple-value-bind (s2 g2) (ed-random-pair)
          (declare (ignore s2))
          (let* ((lf      (ed-mul g1 a2-sum))
                 (rt      (ed-mul g2 a1-sum))
                 (cmt     (ed-add (ed-mul g1 a1-sum)
                                  (ed-mul g2 a2-sum)))
                 (chal    (int (hash:hash/256 a1 a1-blind
                                              a2 a2-blind
                                              g1 g2
                                              lf rt)))
                 (alpha   (m+ (m* a1-sum chal)
                              (m/ a2-sum chal))))
            (values
             (list
              :g1     (ed-compress-pt g1)
              :g2     (ed-compress-pt g2)
              :cmt    (ed-compress-pt cmt)
              :lf     (ed-compress-pt lf)
              :rt     (ed-compress-pt rt)
              :chal   chal
              :alpha  alpha)
             (list
              :a1       a1
              :a1-blind a1-blind
              :a2       a2
              :a2-blind a2-blind))
            ))))))

(defun verify-proof (lst)
  "Verify proof of a1,a2

   Challenge = z
   Compute:  G' = 1/z * G1 + z * G2
   Compute:  C' = (1/z^2) * LF + C + z^2 * RT
   Check:    C' = alpha * G'  "

  (with-mod *ed-r*
    (let* ((g1   (ed-decompress-pt (getf lst :g1)))
           (g2   (ed-decompress-pt (getf lst :g2)))
           (chal (getf lst :chal))
           (gprime (ed-add (ed-mul g1 (m/ chal))
                           (ed-mul g2 chal)))
           (chal^2 (m* chal chal))
           (lf    (ed-decompress-pt (getf lst :lf)))
           (rt    (ed-decompress-pt (getf lst :rt)))
           (cmt   (ed-decompress-pt (getf lst :cmt)))
           (cprime (ed-add (ed-mul lf (m/ chal^2))
                           (ed-add cmt
                                   (ed-mul rt chal^2)))))
      (ed-pt= cprime (ed-mul gprime (getf lst :alpha))))))
