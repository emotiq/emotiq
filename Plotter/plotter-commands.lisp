
(in-package :plotter)

;; ----------------------------------------------------------------

(defun do-plot (cpw port xvector yvector
                    &rest args)
  (progn ;; vm:without-denormals
   (apply #'pw-init-xv-yv cpw xvector yvector args)
   (apply #'pw-axes cpw port args)
   ;;
   ;; Now plot the data points
   ;; 
   (apply #'pw-plot-xv-yv cpw port xvector yvector args)))
  
(defun do-plot-bars (cpw port xvector yvectors
                         &rest args)
  (progn ;; vm:without-denormals
   (apply #'pw-init-bars-xv-yv cpw xvector yvectors args)
   (apply #'pw-axes cpw port args)
   ;;
   ;; Now plot the data points
   ;; 
   (apply #'pw-plot-bars-xv-yv cpw port xvector yvectors args)))

;; -------------------------------------------------------------------

(defparameter *default-args*
  (list
   :watermarkfn 'watermark))

(defun do-with-default-args (args fn)
  (let ((*default-args* (append args *default-args*)))
    (declare (special *default-args*))
    (funcall fn)))
     
(defmacro with-default-args ((&rest args) &body body)
  `(do-with-default-args ',args (lambda () ,@body)))

;; -------------------------------------------------------------------

(defun set-reply-mbox (pane mbox)
  (unless (eq mp:*current-process* mp:*main-process*)
    (when mbox
      (loop until (mp:mailbox-empty-p mbox) do
            (mp:mailbox-read mbox))
      (setf (reply-mbox pane) mbox)
      )))

;; -------------------------------------------------------------------

(defun draw-shape (shape pane x0 y0 x1 y1
                     &key
                     (color :darkgreen)
                     (filled t)
                     (alpha 1)
                     border-thick
                     (border-color :black)
                     (border-alpha 1)
                     start-angle  ;; for arc
                     sweep-angle  ;; for arc
                     reply-mbox
                     &allow-other-keys
                     )
  (let ((pane (plotter-mixin-of pane)))
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (append-display-list pane
                           #'(lambda (pane port x y width height)
                               (declare (ignore x y width height))
                               (progn ;; vm:without-denormals
                                (plt-draw-shape pane port shape
                                                x0 y0 x1 y1
                                                :color  color
                                                :alpha  alpha
                                                :filled filled
                                                :border-thick border-thick
                                                :border-color border-color
                                                :border-alpha border-alpha
                                                :start-angle  start-angle
                                                :sweep-angle  sweep-angle)))
                           ))))

;; user callable function
(defun draw-rect (&rest args)
  (apply #'draw-shape :rect (append args *default-args*)))

;; user callable function
(defun draw-ellipse (&rest args)
  (apply #'draw-shape :ellipse (append args *default-args*)))

;; user callable function
(defun draw-arc (&rest args)
  (apply #'draw-shape :arc (append args *default-args*)))


;; -------------------------------------------------------------------

(defun oplot2 (pane xv yv 
                  &rest args
                  &key
                  clear
                  ;;draw-axes
                  ;;(color :darkgreen)
                  ;; thick
                  xlog
                  ylog
                  ;; (linewidth (or thick 1))
                  (logo *ext-logo*)
                  (logo-alpha *ext-logo-alpha*)
                  (cright1 *cright1*)
                  (cright2 *cright2*)
                  ;;(fullgrid t)
                  reply-mbox
                  &allow-other-keys)
  
  (multiple-value-bind (xv yv)
      (cond (xv
             (filter-potential-x-y-nans-and-infinities xv yv xlog ylog))
            (yv
             (values nil
                     (filter-potential-nans-and-infinities yv ylog)))
            (t (values nil nil)))
    
    (let ((pane  (plotter-mixin-of pane args))
          (style (apply 'get-plot-style args)))
      (with-delayed-update (pane)
        (set-reply-mbox pane reply-mbox)
        (if (or clear
                (display-list-empty-p pane))
            (progn
              #|
                (setf (plotter-x-readout-hook pane) #'identity
                      (plotter-y-readout-hook pane) #'identity)
                |#
              (discard-display-list pane)
              (append-display-list pane
                                   #'(lambda (pane port x y width height)
                                       (declare (ignore x y width height))
                                       (apply #'do-plot pane port xv yv
                                              :plot-style style
                                              ;; :color     color
                                              ;; :linewidth linewidth
                                              ;; :fullgrid  fullgrid
                                              :logo       logo
                                              :logo-alpha logo-alpha
                                              :cright1 cright1
                                              :cright2 cright2
                                              args))
                                   ))
          (append-display-list pane
                               #'(lambda (pane port x y width height)
                                   (declare (ignore x y width height))
                                   (progn ;; vm:without-denormals
                                    (apply #'pw-plot-xv-yv pane port xv yv
                                           :plot-style style
                                           ;; :color color
                                           args)))
                               ))
        ))))

;; -------------------------------------------------------------------

(defun oplot-bars2 (pane xv yvs
                       &rest args
                       &key
                       ;; draw-axes
                       clear
                       (color     :black)
                       (neg-color color)
                       thick
                       (linewidth (or thick 1))
                       ;; (fullgrid t)
                       (logo *ext-logo*)
                       (logo-alpha *ext-logo-alpha*)
                       (cright1 *cright1*)
                       (cright2 *cright2*)
                       reply-mbox
                       &allow-other-keys)
  (let ((pane (plotter-mixin-of pane args)))
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (if (or clear
              (display-list-empty-p pane))
          (progn
            #|
                (setf (plotter-x-readout-hook pane) #'identity
                      (plotter-y-readout-hook pane) #'identity)
                |#
            (discard-display-list pane)
            (append-display-list pane
                                 #'(lambda (pane port x y width height)
                                     (declare (ignore x y width height))
                                     (apply #'do-plot-bars pane port xv yvs
                                            :color     color
                                            :neg-color neg-color
                                            :linewidth linewidth
                                            ;; :fullgrid  fullgrid
                                            :logo logo
                                            :logo-alpha logo-alpha
                                            :cright1 cright1
                                            :cright2 cright2
                                            args))
                                 ))
        (append-display-list pane
                             #'(lambda (pane port x y width height)
                                 (declare (ignore x y width height))
                                 (progn ;; vm:without-denormals
                                  (apply #'pw-plot-bars-xv-yv pane port xv yvs 
                                         :color color
                                         :neg-color neg-color
                                         args)))
                             ))
      )))

;; ------------------------------------------
(defun find-x-y-parms (args)
  (let* ((nargs (or (position-if #'keywordp args)
                    (length args))))
    (case nargs
      (0   (list nil nil args))
      (1   (list nil (first args) (rest args)))
      (2   (list (first args) (second args) (rest (rest args))))
      (otherwise (error "Too many arguments"))
      )))

(defun vector-to-plotfn (fn pane args)
  (destructuring-bind (xs ys parms) (find-x-y-parms args)
    (apply fn pane xs ys parms)))

;; user callable function
(defun plot (pane &rest args)
  (vector-to-plotfn #'oplot2 pane (append args *default-args*)))

;; user callable function
(defun plot-bars (pane &rest args)
  (vector-to-plotfn #'oplot-bars2 pane (append args *default-args*)))

;; ------------------------------------------

;; user callable function
(defun clear (pane &key reply-mbox)
  (let ((pane (plotter-mixin-of pane)))
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (discard-display-list pane))))

;; -------------------------------------------------------------------

(defun stuff-display-list (pane lst &key reply-mbox)
  ;; be careful here... the list is expected to be a list of lambda forms
  ;; as if from another plotter's display list...
  (let ((pane (plotter-mixin-of pane)))
    (discard-display-list pane)
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (um:collector-stuff-contents (plotter-display-list pane) lst))
    ))
      
;; -------------------------------------------------------------------

(defun axes2 (pane xvector yvectors &rest args &key xrange xlog ylog
                   (logo *ext-logo*)
                   (logo-alpha *ext-logo-alpha*)
                   (cright1 *cright1*)
                   (cright2 *cright2*)
                   reply-mbox
                   &allow-other-keys)
  ;; allow a list of yvectors to be given
  ;; so that we can find the best fitting autoscale that accommodates all of them
  (multiple-value-bind (xv yv)
      (let ((ylist (remove nil (um:mklist yvectors))))
        (values (or (and xvector
                         (let ((xv (filter-potential-nans-and-infinities xvector xlog)))
                           (vector (vmin-of xv) (vmax-of xv))))
                    (and (null xrange)
                         ylist
                         (vector (if xlog 0.1 0) (1- (length-of (first ylist))))
                         ))
                (and ylist
                     (let ((ys (mapcar (um:rcurry #'filter-potential-nans-and-infinities ylog) ylist)))
                       (vector (vector-group-min ys)
                               (vector-group-max ys))))
                ))
    (let ((pane (plotter-mixin-of pane args)))
      (with-delayed-update (pane)
        (set-reply-mbox pane reply-mbox)
        #|
    (setf (plotter-x-readout-hook pane) #'identity
          (plotter-y-readout-hook pane) #'identity)
    |#
        (clear pane)
        (append-display-list pane 
                             #'(lambda (pane port x y width height)
                                 (declare (ignore x y width height))
                                 (progn ;; vm:without-denormals
                                  (apply #'pw-init-xv-yv pane
                                         xv yv args)
                                  (apply #'pw-axes pane port
                                         :logo logo
                                         :logo-alpha logo-alpha
                                         :cright1 cright1
                                         :cright2 cright2
                                         args)))
                             ))
      )))

;; user callable function
(defun axes (pane &rest args)
  (vector-to-plotfn #'axes2 pane (append args *default-args*)))

;; ------------------------------------------
(defun outsxy (pane x y str
                  &rest args
                  &key
                  (font-size $normal-times-font-size)
                  (font "Times")
                  anchor
                  (align :w)
                  (offset-x 0) ;; pixel offsets
                  (offset-y 0)
                  clip
                  (color :black)
                  alpha
                  reply-mbox
                  &allow-other-keys)
  (let ((pane (plotter-mixin-of pane)))
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (append-display-list pane
                           #'(lambda (pane port xarg yarg width height)
                               (declare (ignore xarg yarg width height))
                               (let* ((xx (+ offset-x (get-x-location pane x)))
                                      (yy (+ offset-y (get-y-location pane y)))
                                      (sf   (plotter-sf pane))
                                      (font (find-best-font pane
                                                            :family font
                                                            :size   (* sf font-size)))
                                      (x-align (ecase (or anchor align)
                                                 ((:nw :w :sw) :left)
                                                 ((:n :s :ctr) :center)
                                                 ((:ne :e :se) :right)))
                                      (y-align (ecase (or anchor align)
                                                 ((:nw :n :ne) :top)
                                                 ((:w :ctr :e) :center)
                                                 ((:sw :s :se) :baseline)))
                                      (mask (and clip
                                                 (adjust-box
                                                  (mapcar (um:expanded-curry (v) #'* sf)
                                                          (plotter-box pane))
                                                  )))
                                      (color (adjust-color pane color alpha)))
                                 
                                 #+:WIN32
                                 (with-mask (port mask)
                                     (apply #'draw-string-x-y pane port str
                                            (* sf xx) (* sf yy)
                                            :font font
                                            :x-alignment x-align
                                            :y-alignment y-align
                                            :color       color
                                            args))
                                 #-:WIN32
                                 (let* ((font-attrs (gp:font-description-attributes
                                                     (gp:font-description font)))
                                        (font-name  (getf font-attrs :name))
                                        (font-size  (getf font-attrs :size)))
                                   (apply #'add-label port str (* sf xx) (* sf yy)
                                          :font        font-name
                                          :font-size   font-size
                                          :color       color
                                          :x-alignment x-align
                                          :y-alignment y-align
                                          :box         mask
                                          args))
                                 
                                 ))))
    ))

(defun draw-text-box (pane strs xorg yorg
                           &key
                           (font-size $normal-times-font-size)
                           (font "Times")
                           (text-color :black)
                           filled
                           (color :white)
                           alpha
                           border-thick
                           (border-color :black)
                           border-alpha
                           &allow-other-keys)
  (let ((pane (plotter-mixin-of pane)))
    (append-display-list pane
                         (lambda (pane port xarg yarg width height)
                           (declare (ignore xarg yarg width height))
                           (let* ((sf   (plotter-sf pane))
                                  (font (find-best-font pane
                                                        :size   (* sf font-size)
                                                        :family font))
                                  (width (loop for s in strs maximize
                                               (multiple-value-bind (left top right bottom)
                                                   (gp:get-string-extent port s font)
                                                 (declare (ignore top bottom))
                                                 (- right left))))
                                  (height (multiple-value-bind (left top right bottom)
                                              (gp:get-string-extent port (car strs) font)
                                            (declare (ignore left right))
                                            (- bottom top)))
                                  (x0     (get-x-location pane xorg))
                                  (y0     (get-y-location pane yorg))
                                  (color     (adjust-color pane color alpha))
                                  (bcolor    (adjust-color pane border-color border-alpha))
                                  (linewidth (adjust-linewidth (* sf (or border-thick 0)))))

                             (gp:with-graphics-state (port
                                                      :foreground color
                                                      :thickness  linewidth
                                                      :line-end-style   :butt
                                                      :line-joint-style :miter)
                               (when filled
                                 (gp:draw-rectangle port
                                                    (* sf x0) (* sf y0)
                                                    (+ width (* sf 4))
                                                    (* (length strs) height)
                                                    :filled color))
                               (when border-thick
                                 (with-color (port bcolor)
                                   (gp:draw-rectangle port
                                                      (* sf x0) (* sf y0)
                                                      (+ width (* sf 4))
                                                      (* (length strs) height)
                                                      :filled nil))))
                             
                             (loop for y from (+ (* sf (- y0 2)) height) by height
                                   for s in strs
                                   for x = (* sf (+ x0 2))
                                   do
                                   (gp:draw-string port s x y
                                                   :font font
                                                   :foreground text-color
                                                   :block nil) )
                             )) )))

;; --------------------------------------------

(defun cmplx-plot (pane zs &rest args &key &allow-other-keys)
  (let ((xs (map 'vector #'realpart zs))
        (ys (map 'vector #'imagpart zs)))
    (apply #'plot pane xs ys args)))

(defun cmplx-paramplot (pane dom fn &rest args &key &allow-other-keys)
  (let* ((last-param nil)
         (last-value nil)
         (cfn        (lambda (param)
                       (if (eql param last-param)
                           last-value
                         (setf last-param param
                               last-value (funcall fn param)))))
         (xfn        (lambda (param)
                       (realpart (funcall cfn param))))
         (yfn        (lambda (param)
                       (imagpart (funcall cfn param)))))
    (apply #'paramplot pane dom xfn yfn args)))

(defun polar-fplot (pane dom fn &rest args &key &allow-other-keys)
  (let ((rfn (lambda (th)
               (let ((r (funcall fn th)))
                 (complex (* r (cos th)) (* r (sin th)))))))
    (apply #'plt:cmplx-paramplot pane dom rfn args)))


