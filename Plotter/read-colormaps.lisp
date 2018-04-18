
(in-package :plotter)

(defstruct cmap
  name
  rgb)

(defun read-cmaps (&key (fname
                         #+:MSWINDOWS "c:/projects/lib/nml/colortable.bin"
                         #+:MAC       "/usr/local/lib/nml/colortable.bin"
                         #+:LINUX     "~/Linux-stuff/colortable.bin"))
  (with-open-file (f fname
                     :direction :input
                     :element-type '(unsigned-byte 8))
    (let* ((nmaps (read-byte f))
           (maps  (coerce
                   (loop repeat nmaps collect
                         (let* ((r  (make-array 256
                                                :element-type '(unsigned-byte 8)))
                                (g  (make-array 256
                                                :element-type '(unsigned-byte 8)))
                                (b  (make-array 256
                                                :element-type '(unsigned-byte 8))))
                           (read-sequence r f)
                           (read-sequence g f)
                           (read-sequence b f)
                           (make-cmap
                            :rgb (vector r g b))) )
                   'vector)))
      (loop for map across maps do
            (let ((str (make-array 32
                                   :element-type '(unsigned-byte 8))))
              (read-sequence str f)
              (setf (cmap-name map)
                    (string-trim '(#\space)
                                 (coerce
                                  (map 'vector #'code-char str)
                                  'string))) ))
      maps)))

(defvar *cmaps* (read-cmaps))

(defmethod set-cmap ((nbr fixnum))
  (let* ((cmap (aref *cmaps* nbr))
         (map  (get-cmap nbr)))
    (setf *current-colormap* map)
    (tvscl 'tstcmap *tst-img* :magn 4 :clear t)
    (cmap-name cmap)))

(defmethod set-cmap ((name string))
  (set-cmap (position name *cmaps*
                      :key #'cmap-name
                      :test #'string-equal)))

(defmethod get-cmap ((nbr fixnum))
  (let* ((cmap (aref *cmaps* nbr))
         (rs   (aref (cmap-rgb cmap) 0))
         (gs   (aref (cmap-rgb cmap) 1))
         (bs   (aref (cmap-rgb cmap) 2))
         (map  (make-array 256)))
    (loop for ix from 0
          for r across rs
          for g across gs
          for b across bs
          do
          (setf (aref map ix)
                (color:make-rgb
                 (float (/ r 256) 1s0)
                 (float (/ g 256) 1s0)
                 (float (/ b 256) 1s0))))
    map))

(defmethod get-cmap ((name string))
  (get-cmap (position name *cmaps*
                      :key #'cmap-name
                      :test #'string-equal)))
