
(in-package :plotter)

;; ------------------------------------------
(defconstant $gray-colormap
  (let ((map (make-array 256)))
    (loop for ix from 0 to 255 do
          (setf (aref map ix) (color:make-gray (/ (float ix) 255.0))
                ))
    map)
  "A gray-scale colormap.")

(defconstant $heat-colormap
  (let ((map (make-array 256)))
    (labels ((clipx (v)
               (clip (/ v 255) 0.0 1.0)))
      (loop for ix from 0 to 255 do
            (setf (aref map ix)
                  (color:make-rgb
                   (clipx (/ (* 255 ix) 176))
                   (clipx (/ (* 255 (- ix 120)) 135))
                   (clipx (/ (* 255 (- ix 190)) 65)))
                  )))
    map)
  "A reddish to white colormap.")

(defparameter *current-colormap* $heat-colormap ;;$gray-colormap
  "The colormap currently in effect for TVSCL and Plot-Image.")

(defparameter *tst-img*
  (let ((img (make-array '(64 64))))
    (loop for row from 0 below 64 do
          (loop for col from 0 below 64 do
                (setf (aref img row col) (abs (complex row col)))
                ))
    img))

;; -----------------------------------------------------

(defun do-convert-array-to-color-image-for-pane (arr pane port continuation
                                                      &key
                                                      (colormap *current-colormap*)
                                                      first-row
                                                      zrange
                                                      zlog
                                                      flipv
                                                      fliph
                                                      &allow-other-keys)
  (let* ((wd   (array-dimension-of arr 1))
         (ht   (array-dimension-of arr 0))
         (first-row (or first-row
                        (1- ht))))
    (declare (fixnum wd ht first-row))
    
    (with-image (port (img #-:WIN32 (gp:make-image port wd ht)
                           #+:WIN32 (gp:make-image port wd ht :alpha nil)
                           ))
      (with-image-access (acc (gp:make-image-access port img))
        
        (labels ((z-value (z)
                   (if zlog
                       (log (1+ z))
                     z)))
           
          (destructuring-bind (mn mx)
              (mapcar #'z-value
                      (or zrange
                          (multiple-value-list (vextrema-of arr))))
      
            (let* ((gsf  (let ((diff (- mx mn)))
                           (if (zerop diff)
                               0
                             (/ 255 diff))))
                   (xcolors  (rest (or (let ((cmap (cached-cmap pane)))
                                         (and cmap
                                              (eql colormap (first cmap))
                                              cmap))
                                       (setf (cached-cmap pane)
                                             (cons colormap (make-array 256))) ))))
              
              (labels ((convert-to-color (v)
                         (let* ((cix  (round (* gsf (- (clip (z-value v) mn mx) mn)))))
                           (declare (fixnum cix))
                           
                           (or (aref xcolors cix)
                               (setf (aref xcolors cix)
                                     (color:convert-color pane
                                                          (aref colormap cix)))) ))
                       
                       (xfer-line (src-row dst-row)
                         (declare (fixnum src-row dst-row))
                         
                         (labels ((xfer-pixel (src-col dst-col)
                                    (declare (fixnum src-col dst-col))
                                    (setf (gp:image-access-pixel acc dst-col dst-row)
                                          (convert-to-color
                                           (aref-of arr src-row src-col)))))
                           (if fliph
                               (loop for src-col fixnum from (1- wd) downto 0
                                     for dst-col fixnum from 0 do
                                     (xfer-pixel src-col dst-col))
                             ;; else
                             (loop for col fixnum from 0 below wd do
                                   (xfer-pixel col col)) ))))
              
                ;; split the conversion into two portions to allow
                ;; for scrolling waterfall displays. Top line indicated by
                ;; keyword parameter :first-row

                ;; NOTE: image addressing is upside down vertically.
                ;; We display, by default (- not flipped -), so that
                ;; the origin of the array is shown at the lower left
                ;; corner.
                
                (if flipv
                    (progn
                      (loop for src-row fixnum from first-row downto 0
                            for dst-row fixnum from (1- ht) by -1 do
                            (xfer-line src-row dst-row))
                      (loop for src-row fixnum from (1- ht) above first-row
                            for dst-row fixnum from (- ht first-row 2) by -1 do
                            (xfer-line src-row dst-row)))
                  ;; else
                  (progn
                    (loop for src-row fixnum from first-row downto 0
                          for dst-row fixnum from 0 do
                          (xfer-line src-row dst-row))
                    (loop for src-row fixnum from (1- ht) above first-row
                          for dst-row fixnum from (1+ first-row) do
                          (xfer-line src-row dst-row)))
                  )))))
            
        (gp:image-access-transfer-to-image acc))
      
      (funcall continuation img)) ))

(defmacro with-array-converted-to-color-image-for-pane ((arr pane port img args)
                                                        &body body)
  `(apply #'do-convert-array-to-color-image-for-pane
          ,arr ,pane ,port
          (lambda (,img)
            ,@body)
          ,args))

#+:LISPWORKS
(editor:setup-indent "with-array-converted-to-color-image-for-pane" 1)


(defun do-tvscl (pane arr
                      &rest args
                      &key
                      (magn 1)
                      reply-mbox
                      clear
                      &allow-other-keys)
  "Internal workhorse routine for TVSCL."
  (let ((pane (plotter-mixin-of pane)))
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (when clear
        (discard-display-list pane))
      (append-display-list
       pane
       (lambda (pane port x y width height)
         (declare (ignore x y width height))

         (let* ((wd (array-dimension-of arr 1))
                (ht (array-dimension-of arr 0)))

           ;; this scaling gives the unflipped origin at the LLC
           ;; with positive Y values upward, positive X values rightward
           (pw-init-xv-yv pane (vector 0 wd) (vector 0 ht)
                          :xrange `(0 ,wd)
                          :yrange `(0 ,ht)
                          :box    `(0 0 ,(* magn wd) ,(* magn ht)))
             
           (with-array-converted-to-color-image-for-pane (arr pane port img args)
             (let ((sf (* magn (plotter-sf pane))))
               (gp:with-graphics-scale (port sf sf)
                 (gp:draw-image port img 0 0))
               ))
           (setf (plotter-magn pane) magn)
           ))))))

;; user callable routine
(defun tvscl (pane arr &rest args)
  "Display an image array of z-values in the specified pane.
   This is distinct from plotting an image with user specified
   X and Y values. TVSCL always uses implicit pixel coordinates
   for its X and Y values."
  (apply 'do-tvscl pane arr (append args *default-args*)))

(defun do-plot-image (pane xv yv arr
                           &rest args
                           &key
                           reply-mbox
                           clear
                           &allow-other-keys)
  "Internal workhorse for image plotting."
  (let ((pane (plotter-mixin-of pane)))
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (when clear
        (discard-display-list pane))
      (append-display-list 
       pane
       (lambda (pane port x y width height)
         (declare (ignore x y width height))
         
         (apply 'pw-init-xv-yv pane xv yv args)
         (gp:clear-graphics-port port)
         (apply 'pw-axes pane port :clear nil args)

         (with-array-converted-to-color-image-for-pane (arr pane port img args)
           
           (let* ((wd   (array-dimension-of arr 1))
                  (ht   (array-dimension-of arr 0))
                  (sf   (plotter-sf  pane))
                  (box  (let ((box (plotter-box pane)))
                          (adjust-box
                           (list (1+ (* sf (box-left box)))
                                 (* sf (box-top box))
                                 (1- (* sf (box-width box)))
                                 (* sf (box-height box))))
                          )))
             (declare (fixnum wd ht))
             
             (labels ((x-value (x)
                        (if (plotter-xlog pane)
                            (log10 x)
                          x))
                      (y-value (y)
                        (if (plotter-ylog pane)
                            (log10 y)
                          y)))
               
               (multiple-value-bind (px py)
                   (gp:transform-point (plotter-xform pane)
                                       (x-value (elt xv 0))
                                       (y-value (elt yv 0)))
                 
                 (multiple-value-bind (px2 py2)
                     (gp:transform-point (plotter-xform pane)
                                         (x-value (elt xv 1))
                                         (y-value (elt yv 1)))
                   
                   (let ((plt-wd (1+ (- px2 px)))
                         (plt-ht (1+ (- py py2))))
                     
                     ;; (print (list plt-wd plt-ht))
                     (gp:with-graphics-scale (port sf sf)
                       (gp:with-graphics-state (port
                                                :mask box)
                         (gp:draw-image port img
                                        px py2
                                        :from-width  wd
                                        :from-height ht
                                        :to-width    plt-wd
                                        :to-height   plt-ht
                                        ))))
                   )))))
         )) )))

(defun plot-image (pane xv yv image-arr &rest args)
  "Plot an image in the specified pane using the sequences xv and yv
   for the X and Y scales. The image array holds the z-values."
  (apply 'do-plot-image pane xv yv image-arr (append args *default-args*)))
  
#|
;; tst plot-image
(let* ((nx  32)
       (ny  8)
       (arr (make-array (list (1+ ny) (1+ nx))
                       :element-type 'single-float
                       :initial-element 0.0)))
  (loop for iy from 0 to ny do
        (loop for ix from 0 to nx do
              (setf (aref arr iy ix) (float (* ix iy) 1.0))))
  ;; (loop for ix from 0 to 50 do (setf (aref arr ix (round ix 2)) 0.0))
  (let ((plt (wset 'plt :background :black :foreground :white)))
    (plot-image plt '(0 100) '(0 100) arr :watermarkfn nil)))
|#

#|
(defvar *dbg* (debug-stream:make-debug-stream
               :display t
               :title "Debugger Window"
               ))

(defun pdbg (fmt &rest args)
  (debug-stream:debug-print *dbg* (apply #'format nil fmt args)))
|#

(defun do-render-image (pane ext-img
                     &key
                     (magn 1)
                     (to-x 0)
                     (to-y 0)
                     (from-x 0)
                     (from-y 0)
                     to-width
                     to-height
                     from-width
                     from-height
                     transform
                     global-alpha
                     reply-mbox
                     clear
                     &allow-other-keys)
  "Internal workhorse for image rendering."
  (let ((pane (plotter-mixin-of pane)))
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (when clear
        (discard-display-list pane))
      (append-display-list
       pane
       #'(lambda (pane port x y wd ht)
           (declare (ignore x y wd ht))
           (progn ;; let ((sf (* magn (plotter-sf pane))))
             (with-image (port (img (gp:convert-external-image port ext-img)))
               (let* ((from-width  (or from-width  (gp:image-width  img)))
                      (from-height (or from-height (gp:image-height img)))
                      (to-width    (or to-width
                                       (if (>= from-width from-height)
                                           (gp:port-width port)
                                         (* (gp:port-height port)
                                            (/ from-width from-height)))))
                      (to-height   (or to-height
                                       (if (>= from-height from-width)
                                           (gp:port-height port)
                                         (* (gp:port-width port)
                                            (/ from-height from-width))))))
                 #|
                 (pdbg "i-wd: ~A, i-ht: ~A, p-wd: ~A, p-ht: ~A"
                       from-width from-height
                       to-width to-height)
                 |#
                 (progn ;; gp:with-graphics-scale (port sf sf)
                   (setf (plotter-box pane) `(0 0 ,to-width ,to-height)
                         (plotter-xmin pane) 0
                         (plotter-ymin pane) 0
                         (plotter-xmax pane) from-width
                         (plotter-ymax pane) from-width
                         (plotter-xlog pane) nil
                         (plotter-ylog pane) nil)
                   (let ((xform (gp:make-transform))
                         (inv-xform (gp:make-transform)))
                     (gp:apply-scale xform 1 -1)
                     (gp:apply-translation xform 0 to-height)
                     (gp:invert-transform xform inv-xform)
                     (setf (plotter-xform pane) xform
                           (plotter-inv-xform pane) inv-xform))
                     
                   #|(pw-init-xv-yv pane (vector 0 from-width) (vector 0 from-height)
                                  :aspect 1
                                  :box    (list 0 0 to-height to-width))|#
                   (gp:draw-image port img to-x to-y
                                  :transform    transform
                                  :from-x       from-x
                                  :from-y       from-y
                                  :to-width     to-width
                                  :to-height    to-height
                                  :from-width   from-width
                                  :from-height  from-height
                                  :global-alpha global-alpha)
                   )))
             (setf (plotter-magn pane) magn)
             )))
      )))

;; user callable routine
(defun render-image (pane ext-img &rest args)
  "Render an external image in the specified window pane."
  (apply #'do-render-image pane ext-img (append args *default-args*)))


(defvar *last-image-path* nil
  "Holds the last used folder path for read-image.")

(defun read-image (&optional filename)
  "Read an external image file. If the filename argument is elided,
   CAPI will prompt for one."
  (let ((path (or filename
                  (capi:prompt-for-file
                   "Select Image File"
                   :filter "*.*"
                   :pathname *last-image-path*))))
    (when path
      (setf *last-image-path* path)
      (gp:read-external-image path))
    ))

