;;; graphviz interface for gossip
;;; SVS

;;; You need to install graphviz before this will work. On a Mac, do this:
;;; brew install graphviz --with-gts

(in-package :gossip)

(defparameter *graphviz-command* "/usr/local/bin/sfdp")

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

(defun write-inner-commands (stream nodelist)
  (let ((edges-already-drawn (make-hash-table :test 'equalp)))
    (dolist (node nodelist)
      (format stream "~%  \"~A\" [fontsize=\"12.0\", label=\"\\N\", style=\"filled\", fillcolor=\"#00ff00Af\"] ;"
              (uid node))
      (dolist (neighbor (neighbors node))
        (let* ((minuid (min neighbor (uid node)))
               (maxuid (max neighbor (uid node)))
               (key (cons minuid maxuid)))
          (unless (gethash key edges-already-drawn) ; don't draw links twice
            (format stream "~%  \"~A\" -- \"~A\";" (uid node) neighbor)
            (setf (gethash key edges-already-drawn) t)))))))

(defun write-dotfile-stream (stream nodelist)
  (let ((mapname (random-name)))
    (format stream "graph ~A {~%" mapname)
    (format stream "graph [outputorder=\"edgesfirst\"];~%")
    (format stream "graph [ratio=\"compress\"];~%")
    (format stream "graph [overlap=prism];~%")
    (format stream "graph [repulsiveforce=0.2];~%")
    (format stream "graph [K=0.1];~%")
    (write-inner-commands stream nodelist)
    (format stream "~%}")
    mapname))

(defun write-dotfile (dotfile nodelist)
  (with-open-file (stream dotfile :direction :output :if-exists :supersede)
    (write-dotfile-stream stream nodelist)))

(defun convert-dotfile-to-svg (dotpath &optional svgpath)
  (let ((cmd *graphviz-command*))
    (uiop:run-program (list cmd
                            "-Tsvg"
                            ;"-Gmodel=subset"
                            (uiop:native-namestring dotpath)
                            "-o"
                            (uiop:native-namestring svgpath)))))

(defmethod visualize-nodes ((nodes null))
  (visualize-nodes *nodes*))

(defmethod visualize-nodes ((nodes hash-table))
  (visualize-nodes (listify-nodes nodes)))

(defmethod visualize-nodes ((nodelist list))
  "Makes a graphviz .dot file from nodelist, then converts that to an .svg file, and returns that pathname.
  Opens svg file in browser."
  (let* ((dotpath (make-temp-dotfile))
         (svgpath (make-pathname :directory (pathname-directory dotpath)
                                 :name (pathname-name dotpath)
                                 :type "svg")))
    (write-dotfile dotpath nodelist)
    (convert-dotfile-to-svg dotpath svgpath)
    (let ((urlstring (concatenate 'string "file://" (uiop:native-namestring svgpath))))
      #+LISPWORKS (sys:open-url urlstring)
      #+CLOZURE
      (let* ((url (NEXTSTEP-FUNCTIONS::|absoluteURL|
                                       (make-instance 'ns:ns-url
                                         :with-string (ccl::%make-nsstring urlstring)))))
        (ccl::%open-url-in-browser url))
      (values dotpath svgpath))))