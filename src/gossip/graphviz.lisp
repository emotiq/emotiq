;;; graphviz interface for gossip
;;; SVS

;;; You need to install graphviz before this will work.
;;; On a Mac, just do this:
;;;  brew install graphviz --with-gts
;;; 
;;; On linux:
;;;  sudo apt install libgts-dev
;;;  pkg-config --libs gts
;;;  pkg-config --cflags gts
;;; Download source from www.graphviz.org
;;; Extract
;;; .configure --with-gts
;;; make
;;; make install
;;; 
;;; Following WILL NOT work because the default graphviz build is not built with gts, and sfdp is useless without it.
;;;  sudo apt-get install graphviz

(in-package :gossip)

(defvar *graphviz-command* nil "Location of graphviz sfdp program")

(defun find-graphviz ()
  (let ((loc (uiop:run-program (list "which" "sfdp") :output :string)))
    (when (and loc (eql 0 (ignore-errors (position #\/ loc))))
      (string-trim '(#\newline) loc))))

(defun random-name (&optional (prefix "ELEM"))
  (string-downcase (symbol-name (gensym prefix))))

(defun temp-folder ()
  #+LISPWORKS
  (hcl:get-temp-directory)
  #+CLOZURE
  (let* ((ptr (CCL:EXTERNAL-CALL
               "tempnam"
               :ADDRESS
               (CCL:%NULL-PTR)
               :ADDRESS
               (CCL:%NULL-PTR)
               :ADDRESS))
        (tempname (ccl::%get-cstring ptr)))
    (CCL:EXTERNAL-CALL "free" :ADDRESS PTR :VOID)
    (directory-namestring tempname)))

(defun make-temp-dotfile ()
  (let* ((name (random-name "DOTFILE"))
         (folder (temp-folder))
         (dotfile (merge-pathnames (make-pathname :name name :type "dot") folder)))
    dotfile))

(defun short (n)
  "Returns a (possibly shortened) string version of integer n."
  (let ((orig (format nil "~D" n)))
    (cond ((> (length orig) 12)
           (concatenate 'string (subseq orig 0 4) "..." (subseq orig (- (length orig) 4))))
          (t orig))))

;; Note: Firefox won't show second and succeeding lines as smaller if you have a minimum
;;   font size set.
;;   https://stackoverflow.com/questions/29878587/firefox-doesnt-respect-font-size-in-svg-text#29879109
(defmethod dump-node ((node proxy-gossip-node) stream edgetable graphID)
  "Dump proxy node showing UID, real address, and real port"
  (declare (ignore edgetable graphID))
  (format stream "~%  \"~A\" [fontsize=\"10.0\", tooltip=\"~A\", label=<~A<BR /> ~
        <FONT POINT-SIZE=\"8\">~A/~D<BR />~A</FONT>>, style=\"filled\", fillcolor=\"#ffff00Af\"] ;"
                     (uid node)
                     (uid node)
                     (short (uid node))
                     (real-address node)
                     (real-port node)
                     (short (real-uid node))))

#| OLD
(format stream "~%  \"~A\" [fontsize=\"12.0\", label=\"\\N\", style=\"filled\", fillcolor=\"#ffff00Af\"] ;"
                     (uid node))
|#

(defmethod dump-node ((node gossip-node) stream edgetable graphID)
  (format stream "~%  \"~A\" [fontsize=\"12.0\", penwidth=4.0, label=\"\~A\", tooltip=\"~A\", style=\"filled\", fillcolor=\"#00ff00Af\"] ;"
          (uid node)
          (short (uid node))
          (uid node))
  (dolist (neighbor (neighborhood node graphID))
    (let* ((minuid (min neighbor (uid node)))
           (maxuid (max neighbor (uid node)))
           (key (cons minuid maxuid)))
      (unless (gethash key edgetable) ; don't dump links twice
        (format stream "~%  \"~A\" -- \"~A\";" (uid node) neighbor)
        (setf (gethash key edgetable) t)))))

(defun write-inner-commands (stream nodelist graphID)
  (let ((edges-already-dumped (make-hash-table :test 'equalp)))
    (dolist (node nodelist)
      (dump-node node stream edges-already-dumped graphID))))

(defun write-dotfile-stream (stream nodelist graphID)
  (let ((mapname (random-name)))
    (format stream "graph ~A {~%" mapname)
    (format stream "graph [outputorder=\"edgesfirst\"];~%")
    (format stream "graph [ratio=\"compress\"];~%")
    (format stream "graph [overlap=false];~%")
    (format stream "graph [repulsiveforce=8];~%")
    (format stream "graph [K=1.0];~%")
    (write-inner-commands stream nodelist graphID)
    (format stream "~%}")
    mapname))

(defun write-dotfile (dotfile nodelist graphID)
  (with-open-file (stream dotfile :direction :output :if-exists :supersede)
    (write-dotfile-stream stream nodelist graphID)))

(defun convert-dotfile-to-svg (dotpath &optional svgpath)
  (let ((cmd *graphviz-command*))
    (unless cmd
      (setf cmd
            (setf *graphviz-command* (find-graphviz))))
    (cond (cmd
           (uiop:run-program (list cmd
                                   "-Tsvg"
                                   ;"-Gmodel=subset"
                                   (uiop:native-namestring dotpath)
                                   "-o"
                                   (uiop:native-namestring svgpath))))
          (t (error "Cannot locate graphviz sfdp command. Please install graphviz first.")))))

(defmethod visualize-nodes ((nodes hash-table) &optional (graphID *default-graphID*))
  (visualize-nodes (listify-nodes nodes) graphID))

(defparameter *html-header*
  "<!DOCTYPE html>
  <html>
  <body style=\" margin:0; padding:0; overflow:hidden;\">")

(defparameter *html-footer*
  "</body>
  </html>")

(defun massage-graphviz-svg-file (infile outfile)
  "Hackliy massages a graphviz svg output file into an html file that will automatically
  resize to match the size of your browser window.
  Would be nice if there were options to graphviz for this, but alas, no."
  ; https://stackoverflow.com/questions/5643254/how-to-scale-svg-image-to-fill-browser-window
  (with-open-file (in infile :direction :input)
    (let ((line ""))
      (loop until (equal 0 (search "<svg width=" line)) do
        (setf line (read-line in nil nil nil)))
      ;(print line)
      (with-open-file (out outfile :direction :output :if-exists :supersede)
        (write-string *html-header* out)
        (write-string "<svg style=\"position:fixed; top:0; left:0; height:100%; width:100%;\"" out)
        (loop until (null line) do
          (setf line (read-line in nil nil nil))
          (when line
            (write-string line out)))
        (write-string *html-footer* out)))))

(defmethod visualize-nodes-svg ((nodelist list) graphID)
  "Makes a graphviz .dot file from nodelist, then converts that to an .svg file, and returns that pathname.
  Opens svg file in browser."
  (let* ((dotpath (make-temp-dotfile))
         (svgpath (make-pathname :directory (pathname-directory dotpath)
                                 :name (pathname-name dotpath)
                                 :type "svg")))
    (write-dotfile dotpath nodelist graphID)
    (convert-dotfile-to-svg dotpath svgpath)
    (values dotpath svgpath)))

(defmethod visualize-nodes ((nodelist list) &optional (graphID *default-graphID*))
  "Calls visualize-nodes-svg, then converts that file into an html file that automatically
  resizes the svg image as you resize your browser window. Opens html file in browser."
  (let ((len (length nodelist)))
    (when (> len 5000)
      (cerror "Do it anyway." "There are ~D nodes. It will take a long time to visualize that many." len))
    (multiple-value-bind (dotpath svgpath)
                         (visualize-nodes-svg nodelist graphID)
      (let ((htmlpath (make-pathname :host (pathname-host svgpath)
                                     :directory (pathname-directory svgpath)
                                     :name (pathname-name svgpath)
                                     :type "html")))
        (massage-graphviz-svg-file svgpath htmlpath)
        (let ((urlstring (concatenate 'string "file://" (uiop:native-namestring htmlpath))))
          #+LISPWORKS (sys:open-url urlstring)
          #+(and :CLOZURE :DARWIN)
          (uiop:run-program (list "open" urlstring))
          #-(or :LISPWORKS (and :CLOZURE :DARWIN))
          (uiop:run-program (list "xdg-open" urlstring))
          (values dotpath htmlpath))))))
    

; (gossip:visualize-nodes gossip::*nodes* :uber) ; should draw a fully-connected graph of all known real nodes. Not terribly useful, but a good test.