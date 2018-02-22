;; hash.lisp -- Hashing for ADS
;;
;; DM/Emotiq  01/18
;; -----------
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


(in-package :ads)

;; ----------------------------------------------------------------
;;
;; We focus on SHA3 for post-Quantum security. Any hash of N bits has
;; inherent security of 2^(N/2) by virtue of the Birthday Attack.
;; Grover's quantum algorithm further degrades that by Sqrt(2^N)
;; giving security of Sqrt(Sqrt(2^N)) => N/4 bits. SHA3 produces 512
;; bit hashes (64 octets), for a post quantum security level of 128
;; bits.
;;
;; For comparison, the SHA256, so often used, will have only 64 bits
;; of security. The generally accepted minimum security level is 80
;; bits.  So SHA256 will be insufficient in a post-quantum world.
;;
;; Additionally, the sponge algorithm used by SHA3 exhibits greater
;; quantum resistance than the Merkle-Damgard operations carried out
;; in SHA256.  As of this date (01/18) nothing less than SHA256 is
;; recommended.
;;
;; The SHA256 family was developed and certified c.a. 2001-2004. SHA3
;; was certified in 2015, giving 10 years of research effort more.

(defun basic-hash (v)
  ;; Ironclad can only digest ub8 simple vectors. For now, we leave it
  ;; to Ironclad to issue errors if this is misused.
  ;; with SHA3:
  ;; (vector (unsigned-byte 8) (*)) -> (vector (unsigned-byte 8) (64))
  (let ((dig (ironclad:make-digest :sha3)))
    (ironclad:update-digest dig (to-ub8v v))
    (ironclad:produce-digest dig)))

(defun basic-hash-256 (v)
  ;; Ironclad can only digest ub8 simple vectors. For now, we leave it
  ;; to Ironclad to issue errors if this is misused.
  ;; with SHA256:
  ;; (vector (unsigned-byte 8) (*)) -> (vector (unsigned-byte 8) (32))
  (let ((dig (ironclad:make-digest :sha256)))
    (ironclad:update-digest dig (to-ub8v v))
    (ironclad:produce-digest dig)))


;; --------------------------------------------------------------
;; There are several ways to express the result of H(A|B|C), for
;; concatenated items A, B, and C. You could use HASHEM as shown here,
;; where A, B, and C are successively accumulated into the hash. Or
;; you could first encode (A|B|C) as a ub8v and hash that single
;; vector. The two hash values will differ, but both are legitimate
;; representations. The key is to be consistent, however you do it.
;;
;; The usual approach is to perform successive accumulation into the
;; hash digest. But that misses the structural form of the
;; concatenation, which a ub8v representation would capture along with
;; the values. If structure is important, i.e., list of values vs
;; vector of values, then you should prefer the ub8v encoding for
;; disambiguation.
;;
(defun basic-hashem (&rest args)
  ;; multiple value hash...
  (let ((dig (ironclad:make-digest :sha3)))
    (dolist (arg args)
      (ironclad:update-digest dig (to-ub8v arg)))
    (ironclad:produce-digest dig)))

