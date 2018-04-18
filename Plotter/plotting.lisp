
(in-package :plt)

;; ----------------------------------

(defclass colorpatch (capi:drawn-pinboard-object)
  ((cp-color   :accessor cp-color   :initarg :color  :initform :black))
  (:default-initargs
   :display-callback 'draw-colorpatch
   :x       0
   :y       0
   :width  32
   :height 16))

(defmethod draw-colorpatch (pane (self colorpatch) x y width height)
  (gp:draw-rectangle pane x y width height
                     :foreground (cp-color self)
                     :filled t)
  (gp:draw-rectangle pane x y width height
                     :foreground :black))

;; ----------------------------------

(capi:define-interface colorpatch-text-input-pane ()
  ()
  (:panes
   (patch colorpatch
          :color :black
          :accessor patch)
   (textpane capi:text-input-pane
             :text ":BLACK"
             :completion-function 'color-completion-callback
             :buttons '(:ok nil
                        :completion t)
             :accessor textpane)
   )
  (:layouts
   (colorpatch-layout capi:pinboard-layout
                      '(patch)
                      :visible-min-width  33
                      :visible-max-width  33
                      :visible-min-height 17
                      :visible-max-height 17)
   (main-layout capi:row-layout
                '(colorpatch-layout
                  textpane)
                :adjust :center))
  (:default-initargs
   :layout 'main-layout
   :title  "ColorPatch Selector"))


(defun color-completion-callback (widget data)
  (multiple-value-bind (new-color success)
      (capi:prompt-for-color 
       "Choose a color"
       :color  (read-from-string data nil :black))
    (if success
        (let ((new-text (format nil "~S" new-color))
              (cp       (patch (capi:element-interface widget))))
          (setf (cp-color cp) new-color)
          (gp:invalidate-rectangle (capi:pinboard-object-pinboard cp))
          new-text)
      data)))

(defmethod set-colorpatch-color ((widget colorpatch-text-input-pane) color)
  (setf (cp-color (patch widget)) color
        (capi:text-input-pane-text (textpane widget)) (format nil "~S" color)))

(defmethod get-colorpatch-color ((widget colorpatch-text-input-pane))
  (values (cp-color (patch widget))
          (capi:text-input-pane-text (textpane widget))))

;; ----------------------------------------------

(defvar *plot-style*
  '(:thick 2))

(defvar *plot-axis-properties*
  '(:plot-fn plt::sinc
    :domain  (0 1)))

(capi:define-interface axis-property-sheet ()
    ()
  (:panes
   (plot-title capi:text-input-pane
               :title "Plot Title"
               :text  "A Plot of Y vs X"
               :accessor plot-title)
   #|(domain capi:text-input-pane
                :title "Domain (min,max)"
                :text "(0 1)"
                :accessor domain)|#
   (plot-fn capi:text-input-pane
       :title "Function"
       :text  "plt:sinc"
       :accessor plot-fn)
   (x-title capi:text-input-pane
            :title "Title"
            :text "X Values"
            :visible-min-width 200
            :accessor x-title)
   (y-title capi:text-input-pane
            :title "Title"
            :text "Y Values"
            :visible-min-width 200
            :accessor y-title)
   (x-log capi:radio-button-panel
          :title "Style"
          :items '(:linear :log)
          :print-function 'string-capitalize
          :layout-class 'capi:row-layout
          :accessor x-log)
   (y-log capi:radio-button-panel
          :title "Style"
          :items '(:linear :log)
          :print-function 'string-capitalize
          :layout-class 'capi:row-layout
          :accessor y-log)
   #|(x-scale capi:text-input-pane
            :title "Scaling"
            :text  "1.0"
            :accessor x-scale)|#
   (x-min capi:text-input-pane
           :title "Min"
           :accessor x-min)
   (x-max  capi:text-input-pane
           :title "Max"
           :accessor x-max)
   (y-min capi:text-input-pane
           :title "Min"
           :accessor y-min)
   (y-max capi:text-input-pane
         :title "Max"
         :accessor y-max)
   #|(y-scale capi:text-input-pane
            :title "Scaling"
            :text "1.0"
            :accessor y-scale)|#
   (bg-color colorpatch-text-input-pane
             :title "Background"
             :accessor bg-color)
   (fg-color colorpatch-text-input-pane
             :title "Foreground"
             :accessor fg-color)
   (ok-button capi:push-button
              :text "OK"
              :default-p t
              :visible-min-width 100
              :callback 'exit-axis-dialog)
   (cancel-button capi:push-button
                  :text "Cancel"
                  :visible-min-width 100
                  :callback (lambda (item intf)
                              (declare (ignore item))
                              (capi:destroy intf)))
   (style  capi:text-input-pane
           :title "Plot style"
           :text  "(:thick 2)"
           :accessor style)
   (grid capi:check-button
          :title "Grid"
          :text ""
          :accessor grid)
   )
  (:layouts
   (bgfg-row
    capi:row-layout
    '(bg-color
      fg-color)
    :background (color:make-gray 0.75)
    )
   (x-minmax
    capi:row-layout
    '(x-min x-max))
   (y-minmax
    capi:row-layout
    '(y-min y-max))
   (x-logscale
    capi:row-layout
    '(x-log #|x-scale|#))
   (y-logscale
    capi:row-layout
    '(y-log #|y-scale|#))
   (x-column
    capi:column-layout
    '(x-title x-logscale x-minmax)
    :background (color:make-rgb 0.77 0.8 0.8)
    :title "X Axis")
   (y-column
    capi:column-layout
    '(y-title y-logscale y-minmax)
    :background (color:make-rgb 0.77 0.8 0.8)
    :title "Y Axis")
   (title-row
    capi:column-layout
    '(plot-title #|domain|# plot-fn style
                 grid))
   (filler1
    capi:column-layout
    ())
   (button-row
    capi:row-layout
    '(filler1 ok-button cancel-button))
   (axis-row
    capi:row-layout
    '(x-column y-column))
   (main-layout
    capi:column-layout
    '(title-row
      bgfg-row
      axis-row
      button-row))
   )
  (:default-initargs
   :layout 'main-layout
   :title  "Function Plotting"
   ))

(defun make-axis-property-dialog (&key
                                  xlog
                                  ylog
                                  xrange
                                  yrange
                                  (xtitle "X Values")
                                  (ytitle "Y Values")
                                  (title  "A Plot of Y vs X")
                                  (background :white)
                                  (foreground :black)
                                  (fullgrid t)
                                  (plot-fn 'plt::sinc)
                                  ;; (domain '(0 1))
                                  ;; continuation
                                  &allow-other-keys)
  (let ((intf (make-instance 'axis-property-sheet)))
    (setf (capi:choice-selected-item (x-log intf)) (if xlog :log :linear)
          (capi:choice-selected-item (y-log intf)) (if ylog :log :linear)
          (capi:text-input-pane-text (plot-title intf)) title
          (capi:text-input-pane-text (x-title intf)) xtitle
          (capi:text-input-pane-text (y-title intf)) ytitle
          (capi:text-input-pane-text (x-min intf)) (if xrange
                                                     (format nil "~A" (elt xrange 0))
                                                     "")
          (capi:text-input-pane-text (x-max intf)) (if xrange
                                                     (format nil "~A" (elt xrange 1))
                                                     "")
          (capi:text-input-pane-text (y-min intf)) (if yrange
                                                     (format nil "~A" (elt yrange 0))
                                                     "")
          (capi:text-input-pane-text (y-max intf)) (if yrange
                                                     (format nil "~A" (elt yrange 1))
                                                     "")
          (capi:text-input-pane-text (plot-fn intf)) (format nil "~S" plot-fn)
          ;; (capi:text-input-pane-text (domain intf))  (format nil "~S" domain)
          (capi:text-input-pane-text (style intf))   (format nil "~S" *plot-style*)
          (capi:button-selected (grid intf))         fullgrid
          )
    (set-colorpatch-color (bg-color intf) background)
    (set-colorpatch-color (fg-color intf) foreground)
    (capi:contain intf)
    ;; (capi:display-dialog intf :continuation continuation)
    ))


(defun read-text-pane-text (widget)
  (read-from-string (capi:text-input-pane-text widget) nil nil))

(defun eval-text-pane-text (widget)
  ;; (gdsp::gz (capi:text-input-pane-text widget))
  (read-text-pane-text widget))

(defun exit-axis-dialog (item intf)
  (declare (ignore item))
  (catch :gz-error
    (let ((xmin    (eval-text-pane-text (x-min   intf)))
          (xmax    (eval-text-pane-text (x-max   intf)))
          (ymin    (eval-text-pane-text (y-min   intf)))
          (ymax    (eval-text-pane-text (y-max   intf)))
          #|(domain  (eval-text-pane-text (domain  intf)))|#)
      
      (unless xmin
        (setf xmin 0))
      (unless xmax
        (setf xmax 0))
      
      (unless ymin
        (setf ymin 0))
      (unless ymax
        (setf ymax 0))

      #|(unless domain
        (setf domain '(0 1)))|#
      
      (cond ((not (numberp xmin))
             (capi:display-message "X Min not numeric"))
            ((not (numberp xmax))
             (capi:display-message "X Max not numeric"))
            ((not (numberp ymin))
             (capi:display-message "Y Min not numeric"))
            ((not (numberp ymax))
             (capi:display-message "Y Max not numeric"))
            #|((or (not (consp domain))
                 (null (cdr domain))
                 (not (numberp (first domain)))
                 (not (numberp (second domain))))
             (capi:display-message "Domain should be (min max) number pair"))|#
            (t
             (let* ((title   (capi:text-input-pane-text (plot-title intf)))
                    (xtitle  (capi:text-input-pane-text (x-title intf)))
                    (ytitle  (capi:text-input-pane-text (y-title intf)))
                    (xlog    (eq :log (capi:choice-selected-item (x-log intf))))
                    (ylog    (eq :log (capi:choice-selected-item (y-log intf))))
                    (bg      (get-colorpatch-color (bg-color intf)))
                    (fg      (get-colorpatch-color (fg-color intf)))
                    (plot-fn (read-text-pane-text (plot-fn intf)))
                    (props   `(:title      ,title
                               :xtitle     ,xtitle
                               :ytitle     ,ytitle
                               :xlog       ,xlog
                               :ylog       ,ylog
                               :xrange     ,(unless (= xmin xmax) `(,xmin ,xmax))
                               :yrange     ,(unless (= ymin ymax) `(,ymin ,ymax))
                               :fullgrid   ,(capi:button-selected (grid intf))
                               :foreground ,fg
                               :background ,bg
                               ;; :domain     ,domain
                               :plot-fn    ,plot-fn)))
               ;; (capi:exit-dialog props)
               (setf *plot-style* (read-text-pane-text (style intf)))
               (helpme-plot props)
               ))
            ))))

(defun sinc (x)
  (if (zerop x)
      1.0
    (/ (sin (* pi x)) (* pi x))))

(defun helpme-plot (props)
  (let ((bg-prev (getf *plot-axis-properties* :background))
        (fg-prev (getf *plot-axis-properties* :foreground)))
    (unless (and (equalp bg-prev (getf props :background bg-prev))
                 (equalp fg-prev (getf props :foreground fg-prev)))
      (wclose 'plt)))
  (setf *plot-axis-properties* props)
  (let ((fn (getf props :plot-fn #'identity)))
    (apply #'plt:fplot 'plt
           ;; (getf *plot-axis-properties* :domain)
           (let ((xrange (getf props :xrange '(0 1))))
             (if (or (null xrange)
                     (= (elt xrange 0)
                        (elt xrange 1)))
                 '(0 1)
               xrange))
           (if (consp fn)
               (compile nil fn)
             fn)
           :clear t
           (append props
                   *plot-style*))))

(defun helpme ()
  (apply #'make-axis-property-dialog *plot-axis-properties*))

#|
(capi:with-dialog-results (val okp)
    (apply 'make-axis-property-dialog
           :no-y-style-selector t
           '(:title "Thingy"
             :xtitle "Time [ms]"
             :ytitle "Amplitude [dB]"
             :xlog    t
             :ylog    nil))
  (when okp
    (let ((*print-length* nil))
      (print val))
    ))
|#
