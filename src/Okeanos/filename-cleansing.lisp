
;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

(defconstant *spchars* "\\*:")

(defmethod translate-to-filename ((sym symbol))
  (translate-to-filename (symbol-name sym)))

(defmethod translate-to-filename ((str string))
  (with-output-to-string (d)
    (map nil #'(lambda (c)
               (if (find c *spchars*)
                   (format d "~2,'0D" (char-code c))
                 (write-char c d)))
         str)))

#|
(setf str (symbol-name '|12~`!@#$%^&*()_+=-][}{'":;/?>.<,|\|))
(translate-to-filename str)
(with-open-file (f (translate-to-filename "<diddly.x>") ;; (translate-to-filename str)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
  (print :Hello! f))

(mapcar (compose #'translate-to-filename 'package-name) (list-all-packages))
|#

