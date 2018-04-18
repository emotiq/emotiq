
(in-package "PLOTTER")

;; -----------------------------------------------------------------
;; Functional plotting with adaptive gridding
;; DM/RAL 12/06
;; DM/RAL 09/07 -- cleanup and improvements
;; ----------------------------------------------------------------------------------------
;; Parametric Plotting with adaptive gridding
;;

(defstruct plot-arg min max prepfn iprepfn vals sf (autoscale t))

(defun compute-adaptive-values (xfn yfn tparms xparms yparms)
  "Collect the function (x,y) values on an adaptive grid for smooth plotting"
  (let* ((screen (capi:convert-to-screen))
         (wd     (capi:screen-width screen))
         (ht     (capi:screen-height screen))
         
         (xmin (plot-arg-min xparms))
         (xmax (plot-arg-max xparms))
         (xauto (plot-arg-autoscale xparms))
         (xsf   (/ wd (- xmax xmin)))
         
         (ymin (plot-arg-min yparms))
         (ymax (plot-arg-max yparms))
         (yauto (plot-arg-autoscale yparms))
         (ysf   (/ ht (- ymax ymin)))
         
         (ts  (plot-arg-vals tparms))
         (xs  (plot-arg-vals xparms))
         (ys  (plot-arg-vals yparms))
         
         (xfn (um:curry #'real-eval-with-nans
                        (um:compose (plot-arg-prepfn xparms)
                                    xfn
                                    (plot-arg-iprepfn tparms))
                        ))
         
         (yfn (um:curry #'real-eval-with-nans
                        (um:compose (plot-arg-prepfn yparms)
                                    yfn
                                    (plot-arg-iprepfn tparms))
                        )))
    
    (labels ((midpt (v1 v2)
               (* 0.5d0 (+ v1 v2)))

             (update-sfs (xval yval)
               (when (and xauto
                          (simple-real-number xval)
                          (not (<= xmin xval xmax)))
                 (setf xmax (max xval xmax)
                       xmin (min xval xmin)
                       xsf  (/ wd (- xmax xmin))
                       ))
               (when (and yauto
                          (simple-real-number yval)
                          (not (<= ymin yval ymax)))
                 (setf ymax (max yval ymax)
                       ymin (min yval ymin)
                       ysf  (/ ht (- ymax ymin))
                       )))
             
             (split-interval (lvl t0 t1 x0 x1 y0 y1 new-xs new-ys)
               #|
               (format t "~&~ASplitting interval: (~A,~A), level = ~A"
                       (make-string (* 2 lvl) :initial-element #\Space)
                       t0 t1 lvl)
               |#
               (if (or (> lvl 9)
                       (and (not xauto)
                            (not (<= xmin x0 xmax))
                            (not (<= xmin x1 xmax)))
                       (and (not yauto)
                            (not (<= ymin y0 ymax))
                            (not (<= ymin y1 ymax))))
                   
                   (list (cons x0 new-xs)
                         (cons y0 new-ys))
                 
                 (let* ((tmid (real-eval-with-nans #'midpt t0 t1))
                        (xmid (real-eval-with-nans #'midpt x0 x1))
                        (ymid (real-eval-with-nans #'midpt y0 y1))
                        (xmv  (funcall xfn tmid))
                        (ymv  (funcall yfn tmid)))

                   ;; calling update-sfs can only reduce the magnification factors used
                   ;; to judge quality of smoothness. So skip it...
                   ;;(update-sfs xmv ymv)
                   
                   (if (or (not (simple-real-number xmv))
                           (not (simple-real-number xmid))
                           (not (simple-real-number ymv))
                           (not (simple-real-number ymid))
                           (> (abs (* xsf (- xmv xmid))) 0.125d0)
                           (> (abs (* ysf (- ymv ymid))) 0.125d0))
                       
                       (destructuring-bind (new-xs new-ys)
                           (split-interval (1+ lvl)
                                           t0 tmid x0 xmv y0 ymv
                                           new-xs new-ys)

                         (split-interval (1+ lvl)
                                         tmid t1 xmv x1 ymv y1
                                         new-xs new-ys))
                     
                     (list (cons x0 new-xs)
                           (cons y0 new-ys))))
                 ))
             
             (iter-points (ts xs ys new-xs new-ys)
               (if (endp (rest ts))
                   
                   (list (cons (first xs) new-xs)
                         (cons (first ys) new-ys))
                 
                 (destructuring-bind ((t0 t1 &rest trest)
                                      (x0 x1 &rest xrest)
                                      (y0 y1 &rest yrest))
                     (list ts xs ys)
                   (declare (ignore trest xrest yrest))
                   
                   (destructuring-bind (new-xs new-ys)
                       (split-interval 0 t0 t1 x0 x1 y0 y1
                                       new-xs new-ys)
                     
                     (iter-points (rest ts) (rest xs) (rest ys)
                                  new-xs new-ys)
                     ))
                 )))
      (iter-points ts xs ys nil nil)
      )))

(defun do-param-plotting (plotfn pane xfn yfn npts tparms xparms yparms args)
  "Internal workhorse routine for functional and parametric plotting"
  (destructuring-bind (xs-raw ys-raw)
      (if npts
          
          (list (plot-arg-vals xparms)
                (plot-arg-vals yparms))

        (compute-adaptive-values xfn yfn tparms xparms yparms))
    
    (let ((xs (mapcar (um:curry #'real-eval-with-nans (plot-arg-iprepfn xparms)) xs-raw))
          (ys (mapcar (um:curry #'real-eval-with-nans (plot-arg-iprepfn yparms)) ys-raw)))
      
      (apply plotfn pane xs ys args)
      (list (length xs) xs ys) ;; for voyueristic pleasure
      )))

(defun fill-in-prepfns (islog parms)
  "Log plots need pref-functions of log10 and pow10"
  (setf (plot-arg-prepfn  parms) (if islog #'log10 #'identity)
        (plot-arg-iprepfn parms) (if islog #'pow10 #'identity)))

(defun fill-in-range (parms range)
  "Set up the range in the plot-arg object"
  (destructuring-bind (vmin vmax) range
    (let* ((prepfn (plot-arg-prepfn parms))
           (pmin   (funcall prepfn (min vmin vmax)))
           (pmax   (funcall prepfn (max vmin vmax))))
      (setf (plot-arg-min parms) pmin
            (plot-arg-max parms) pmax
            (plot-arg-autoscale parms) nil)
      )))

(defun fill-in-estimated-range (parms)
  "Estimate the range of the function"
  (let* ((vs    (filter-nans-and-infinities (plot-arg-vals parms)))
         (vsmin (vmin vs))
         (vsmax (vmax vs)))
    (setf (plot-arg-min parms) (min vsmin vsmax)
          (plot-arg-max parms) (if (= vsmin vsmax)
                                   (if (zerop vsmin)
                                       0.1
                                     (* 1.1 vsmin))
                                 (max vsmin vsmax))
          (plot-arg-autoscale parms) t)
    ))

(defun fill-in-computed-vals (parms fn tparms)
  "Set up the computed values in the plot-arg structure"
  (setf (plot-arg-vals parms)
        (mapcar (um:curry #'real-eval-with-nans
                          (um:compose (plot-arg-prepfn parms) fn (plot-arg-iprepfn tparms)))
                (plot-arg-vals tparms))
        ))

(defun fill-in-parms (parms islog fn range tparms screen-size)
  "Collect the plotting parameters for the plot-arg structure"
  (setf (plot-arg-prepfn parms)  (if islog #'log10 #'identity)
        (plot-arg-iprepfn parms) (if islog #'pow10 #'identity))
  (fill-in-computed-vals parms fn tparms)
  (if range
      (fill-in-range parms range)
    (fill-in-estimated-range parms))
  (setf (plot-arg-sf parms) (/ screen-size (- (plot-arg-max parms)
                                              (plot-arg-min parms)))
        ))

#|
(defun compute-linear-tvals (tmin tmax npts)
  (let* ((npts (or npts
                   16))
         (tsf  (/ (- tmax tmin) (float npts 1.0))))
    (loop for ix from 0 to npts collect
          (+ tmin (* tsf ix)))
    ))
|#

(defun compute-tcheby-tvals (tmin tmax npts)
  "Compute roots of the Tchebyshev polynomial of order npts in the domain
(tmin, tmax) mapped to (-1,1)"
  (let* ((npts (or npts
                   16))
         (sft   (* 0.5d0 (- tmax tmin)))
         (sfpi  (/ pi npts)))
    (labels ((t-of-x (xval)
               (+ tmin (* sft (+ xval 1d0))))

             (x-of-index (ix)
               (cos (* sfpi ix))))

      (um:lc ((t-of-x (x-of-index ix))
              (ix <.. 0 npts)))
      )))

(defun internal-paramplot (pane domain xfn yfn &rest args
                         &key tlog xlog ylog xrange yrange npts
                         &allow-other-keys)
  "Internal driver routine for paramplot and fplot"
  (destructuring-bind (tmin tmax) domain
    (let* ((tprepfn  (if tlog #'log10 #'identity))
           (itprepfn (if tlog #'pow10 #'identity))
           (tmin     (funcall tprepfn tmin))
           (tmax     (funcall tprepfn tmax))
           (tvals    (compute-tcheby-tvals tmin tmax npts))
           (tparms (make-plot-arg
                    :min     tmin
                    :max     tmax
                    :prepfn  tprepfn
                    :iprepfn itprepfn
                    :vals    tvals
                    ))
           (xparms (make-plot-arg))
           (yparms (make-plot-arg))
           (screen (capi:convert-to-screen)))
      
      (fill-in-parms xparms xlog xfn xrange tparms (capi:screen-width screen))
      (fill-in-parms yparms ylog yfn yrange tparms (capi:screen-height screen))

      (do-param-plotting #'plot pane xfn yfn npts tparms xparms yparms args)
      )))

;; --------------------------------------------------------------------------
;; user callable functions
(defun paramplot (pane domain xfn yfn &rest args)
  "Parametric ploting in pane of the xfn and yfn functions over the indicated domain.
   All functional plotting is really performed as parametric plotting. Parametric plotting
   allows for non-single-valued functions.
   - Example:
   ;;; (plt:paramplot 'plt '(0 1)
   ;;;    (lambda (phi) (cos (* 2 pi phi))) ;; the X-function
   ;;;    (lambda (phi) (sin (* 2 pi phi))) ;; the Y-function
   ;;;    :clear t :thick 2 :title \"Circle\")
   where 'plt is the name of the desired plotting window. It will be
   created if not already visible on screen."
  (apply #'internal-paramplot pane domain xfn yfn (append args *default-args*))
  (values))

(defun fplot (pane domain fn &rest args)
  "Functional plotting in pane of the function over the indicated domain.
   - Example:
   ;;; (plt:fplot 'plt '(-20 20) (lambda (x) (/ (sin x) x))
   ;;;     :clear t :thick 2 :title \"Sinc(x)\")
   where 'plt is the name of the desired plotting window. It will be
   created if not already visible on screen."
  (apply #'internal-paramplot pane domain #'identity fn (append args *default-args*))
  (values))

;; --------------------------------------------------------------------------

#|
;; test it out...
(clear 'myplot)
(fplot 'myplot '(-20 20) (lambda (x) (/ (sin x) x)) :thick 2 :title "Sinc")
(destructuring-bind (npts xvals yvals)
    (fplot 'myplot '(-20 20) (lambda (x) (/ (sin x) x)) :color :blue :symbol :circle)
  (clear 'xvals)
  (plot 'xvals xvals))
(clear 'tan)
(with-default-args (:fullgrid    nil
                    ;;:watermarkfn nil
                    :thick 2
                    :color :blue
                    :alpha 0.5)
  (fplot 'tan '(-1.57 1.57) #'tan :yrange '(-10 10)))
|#

#|
;; generate HTML documentation
(user::asdf "cldoc")
(user::with-working-directory (translate-logical-pathname "PROJECTS:LISP;Plotter;")
  (cldoc:extract-documentation :html
                               "doc-html"
                               (asdf:find-system "plotter")))
|#
#|
(user::asdf :documentation-template)
(user::with-working-directory (translate-logical-pathname "PROJECTS:LISP;Plotter;")
  (documentation-template:create-template :plotter
                                          :target "html-template.html"
                                          :subtitle "a library for scientific graphing"))
|#
