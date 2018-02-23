(in-package cl-user)

(let ((test-hex-string "a0b1c2d3e4f56789"))
  (prove:plan 3)
  (prove:diag "octet-vector-to-hex-string")
  (let ((octet-vector (emotiq:hex-string-to-octet-vector test-hex-string)))
    (prove:is (emotiq:octet-vector-p octet-vector)
              t
              "octet-vector-p…")
    ;; length of octet vector should be 1/2 that of hex string
    (prove:is (length octet-vector)
              (floor (length test-hex-string) 2)
              "length octect-vector…")
    (let ((back-hex-string (emotiq:octet-vector-to-hex-string octet-vector)))
      (prove:is back-hex-string
                test-hex-string
                "back-hex in octect-vector-to-hex-string…"))))

(prove:plan 1)
(prove:is (emotiq ovref (emotiq:hex-string-to-octet-vector "3d") 0)
          #x3d
          "hex-string-to-octet-vector…")

(let ((string-rosetta-code "Rosetta code"))

  (prove:plan 2)
  (let ((digest (emotiq:sha-256-string string-rosetta-code)))
    (prove:is
     digest
     "764faf5c61ac315f1497f9dfa542713965b785e5cc2f707d6468d7d1124cdfcf"
     "sha256-string…")
    (prove:is (length digest)
              64
              "digest length…"))

  (prove:plan 2)
  (let ((digest (emotiq:sha-256-vector string-rosetta-code)))
    (prove:is digest
              #(118 79 175 92 97 172 49 95 20 151 249 223 165 66 113
                57 101 183 133 229 204 47 112 125 100 104 215 209 18
                76 223 207)
              :test #'equalp
              "Vector of digest…")
    (prove:is (length digest)
              32
              "Length of digest…"))
  (prove:plan 2)
  (let ((digest (emotiq:sha3-512-string string-rosetta-code)))
    (prove:is 
     digest   
     "a05395817c9c06a4dfbf88553041463b8a4ace62f438d4f74a61356a66a232b69512544b303be599aa875af1803f3afc2ebe57f1b7a5226717baefc2470d4574"
     "sha3-512-string…")
    (prove:is (length digest)
              128
              "sha3-512-string digest length…"))

  (prove:plan 2)
  (let ((digest (emotiq:sha3-512-vector string-rosetta-code)))
    (prove:is digest
              #(160 83 149 129 124 156 6 164 223 191 136 85 48 65 70
               59 138 74 206 98 244 56 212 247 74 97 53 106 102 162 50
               182 149 18 84 75 48 59 229 153 170 135 90 241 128 63 58
               252 46 190 87 241 183 165 34 103 23 186 239 194 71 13
                69 116)
              :test #'equalp
              "sha3-512 vector comparison…")
    (prove:is (length digest)
              64
              "sha3-512 vector digest length…")))

(prove:finalize)



