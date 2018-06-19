(in-package :model/wallet)

(defun enumerate-dictionaries ()
  `("en"))

(defun get-dictionary (language)
  (declare (ignore language))
  `(:|dictionary| ,(cosi-keying::import-wordlist "english.txt")))

           
