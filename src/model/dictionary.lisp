(in-package :model/wallet)

(defun enumerate-dictionaries ()
  "List all the available code dictionary by ISO code"
  `("en"))

(defun get-dictionary (iso-code)
  "Get the 2048 word list for language with ISO-CODE"
  (declare (ignore iso-code))
  `(:|dictionary| ,(cosi-keying::import-wordlist "english.txt")))

           
