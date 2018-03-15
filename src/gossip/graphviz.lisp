(in-package :gossip)

(defun random-name (&optional (prefix "ELEM"))
  (string-downcase (symbol-name (gensym prefix))))

(defun temp-folder ()
  (let* ((ptr (#_tempnam (ccl::%null-ptr) (ccl::%null-ptr)))
        (tempname (ccl::%get-cstring ptr)))
    (#_free ptr)
    (directory-namestring tempname)))

(defun make-temp-dotfile ()
  (let* ((name (random-name "DOTFILE"))
         (folder (temp-folder))
         (dotfile (merge-pathnames (merge-pathnames ".dot" name) folder)))
    dotfile))

(defun write-inner-commands (stream nodelist)
  (let ((edges-already-drawn (make-hash-table :test 'equalp)))
    (dolist (node nodelist)
      (format stream "~%  \"~A\" [fontsize=\"18.0\", label=\"\\N\", fillcolor=\"white\"] ;"
              (uid node))
      (dolist (neighbor (neighbors node))
        (let* ((minuid (min neighbor (uid node)))
               (maxuid (max neighbor (uid node)))
               (key (cons minuid maxuid)))
          (unless (gethash key edges-already-drawn) ; don't draw links twice
            (format t "~%~D, ~D" minuid maxuid)
            (format stream "~%  \"~A\" -- \"~A\";" (uid node) neighbor)
            (setf (gethash key edges-already-drawn) t)))))
    (inspect edges-already-drawn)))

(defun write-dotfile-stream (stream nodelist)
  (let ((mapname (random-name)))
    (format stream "graph ~A {~%" mapname)
    (format stream "graph [ratio=\"compress\"];~%")
    (write-inner-commands stream nodelist)
    (format stream "~%}")
    mapname))

(defun write-dotfile (dotfile nodelist)
  (with-open-file (stream dotfile :direction :output :if-exists :supersede)
    (write-dotfile-stream stream nodelist)))

(defun convert-dotfile-to-svg (dotpath &optional svgpath)
  (let ((neato "/usr/local/bin/neato"))
    (ccl:RUN-PROGRAM neato (list
                            "-Tsvg"
                            (ccl::native-translated-namestring dotpath)
                            "-o"
                            (ccl::native-translated-namestring svgpath)))))

(defun graph-nodes (nodelist)
  "Makes a graphviz .dot file from nodelist, then converts that to an .svg file, and returns that pathname.
   Opens svg file in browser."
  (let* ((dotpath (make-temp-dotfile))
         (svgpath (make-pathname :directory (pathname-directory dotpath)
                                 :name (pathname-name dotpath)
                                 :type "svg")))
    (write-dotfile dotpath nodelist)
    (convert-dotfile-to-svg dotpath svgpath)
    (let* ((url (#/absoluteURL
                 (make-instance 'ns:ns-url
                   :with-string (ccl::%make-nsstring (concatenate 'string "file://"
                                                             (ccl::native-translated-namestring svgpath)))))))
      (ccl::%open-url-in-browser url))
    (values dotpath svgpath)))