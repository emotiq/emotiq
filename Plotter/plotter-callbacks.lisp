
(in-package :plotter)

;; ------------------------------------------
;; these callbacks are only called from the capi process
;;
;; For COCOA the backing store is a pixmap image
;; For Win32 the backing store is a pixmap

(defun save-backing-pixmap (pane port)
  (with-accessors ((backing-pixmap  plotter-backing-pixmap )) pane
    (when backing-pixmap
      (gp:destroy-pixmap-port backing-pixmap))
    (setf backing-pixmap port)))
  
(defun discard-backing-pixmap (pane)
  (with-accessors ((backing-pixmap plotter-backing-pixmap)) pane
    (when backing-pixmap
      (gp:destroy-pixmap-port backing-pixmap)
      (setf backing-pixmap nil))
    ))

;; ---------------------------------------------------
;;
(defun save-backing-image (pane port)
  (with-accessors ((backing-image  plotter-backing-image )) pane
    (when backing-image
      (gp:free-image pane backing-image))
    (setf backing-image
          (gp:make-image-from-port port))
    ))

(defun discard-backing-image (pane)
  (with-accessors ((backing-image  plotter-backing-image)) pane
    (when backing-image
      (gp:free-image pane backing-image)
      (setf backing-image nil)
      (discard-backing-pixmap pane))
    ))

;; --------------------------------------------------
#+:WIN32
(defun draw-crosshair-lines (pane color x y)
  (when (and x y)
    (gp:with-graphics-state
        (pane
         :foreground color
         :operation  boole-xor)
      (gp:draw-line pane x 0 x (gp:port-height pane))
      (gp:draw-line pane 0 y (gp:port-width  pane) y))
    ))

#-:WIN32
(defun redraw-legend (pane)
  (let ((legend (plotter-legend pane)))
    (when (activep legend)
      (draw-existing-legend pane pane))))
  
#-:WIN32
(defun draw-crosshair-lines (pane color x y)
  (when (and x y)
    (with-color (pane color)
      (gp:draw-line pane x 0 x (gp:port-height pane))
      (gp:draw-line pane 0 y (gp:port-width pane) y))
    (redraw-legend pane)))

#-:COCOA
(defun draw-mark (pane)
  (let ((sf (plotter-sf pane))
        (x  (mark-x-raw pane))
        (y  (mark-y-raw pane)))
    (with-color (pane (color:make-rgb 1 0 0))
      (gp:with-graphics-state (pane
                               :dashed t
                               :dash '(4 2 2 2)
                               )
        
        (if x
            (let ((xx (* sf x)))
              (gp:draw-line pane xx 0 xx (gp:port-height pane))))

        (if y
            (let ((yy (* sf y)))
              (gp:draw-line pane 0 yy (gp:port-width pane) yy)))
        ))))

#+:COCOA
(defun draw-mark (pane)
  (let ((sf (plotter-sf pane))
        (x  (mark-x-raw pane))
        (y  (mark-y-raw pane))
        (pat '(13 -1 1 4 9)))
    (destructuring-bind (period pat-1 pat-2 pat-3 pat-4) pat
      (with-color (pane (color:make-rgb 1 0 0))
        (labels ((hdraw (py start end)
                   (loop for px from start below end by period do
                         (gp:draw-line pane (+ px pat-1) py (+ px pat-2) py)
                         (gp:draw-line pane (+ px pat-3) py (+ px pat-4) py)))
                 (vdraw (px start end)
                   (loop for py from start below end by period do
                         (gp:draw-line pane px (+ py pat-1) px (+ py pat-2))
                         (gp:draw-line pane px (+ py pat-3) px (+ py pat-4)))))
          (if (and x y)
              (let ((xx (* sf x))
                    (yy (* sf y)))
                (vdraw xx (- (rem yy period) period) (gp:port-height pane))
                (hdraw yy (- (rem xx period) period) (gp:port-width pane)))
            ;; else
            (progn
              (when x
                (let ((xx (* sf x)))
                  (vdraw xx 0 (gp:port-height pane))))
              (when y
                (let ((yy (* sf y)))
                  (hdraw yy 0 (gp:port-width pane)))) ))))
      (redraw-legend pane) )))

#+:WIN32
(defmethod win32-display-callback ((pane <plotter-pane>) x y width height)
  (when (gp:port-representation pane)
    (display-callback pane x y width height)))

(defmethod redraw-display-list ((pane <plotter-pane>) port x y width height &key legend)
  (discard-legends pane)
  (dolist (item (display-list-items pane))
    (funcall item pane port x y width height))
  (if legend
      (draw-accumulated-legend pane port)))
  
(defmethod display-callback ((pane <plotter-pane>) x y width height)
  (with-accessors ((nominal-width   plotter-nominal-width )
                   (nominal-height  plotter-nominal-height)
                   (sf              plotter-sf            )
                   (magn            plotter-magn          )
                   (xform           plotter-xform         )
                   (port-width      gp:port-width         )
                   (port-height     gp:port-height        )
                   (backing-pixmap  plotter-backing-pixmap)
                   (backing-image   plotter-backing-image )
                   (full-crosshair  plotter-full-crosshair)
                   (delay-backing   plotter-delay-backing )
                   (dirty           plotter-dirty         )
                   (needs-legend    plotter-needs-legend  )
                   (prev-x          plotter-prev-x        )
                   (prev-y          plotter-prev-y        )
                   (reply-mbox      reply-mbox            )) pane

    ;;(gp:clear-graphics-port-state pane)
    
    (setf xform '(1 0 0 1 0 0)
          magn  1
          sf    (min (/ port-height nominal-height)
                     (/ port-width  nominal-width)))
    
    
    #|
     (print (list x y width height sf
                  nominal-width port-width
                  nominal-height port-height))
     |#
    
    (cond  (backing-image
            (gp:draw-image pane backing-image 0 0
                           :from-width  (gp:image-width  backing-image)
                           :from-height (gp:image-height backing-image)
                           :to-width    (* sf nominal-width)
                           :to-height   (* sf nominal-height)))
           
           (delay-backing
            (redraw-display-list pane pane x y width height :legend t))
           
           (t
            (when (and backing-pixmap
                       (or (/= port-width  (gp:port-width backing-pixmap))
                           (/= port-height (gp:port-height backing-pixmap))))
              (discard-backing-pixmap pane))
            
            (unless backing-pixmap
              (let* ((gs   (gp:get-graphics-state pane))
                     (fg   (gp:graphics-state-foreground gs))
                     (bg   (gp:graphics-state-background gs))
                     (port (create-pixmap-port pane port-width port-height
                                               :background bg
                                               :foreground fg
                                               :clear      nil)))
                (save-backing-pixmap pane port)
                (setf dirty t)))
            
            (when dirty
              (gp:clear-graphics-port backing-pixmap)
              (redraw-display-list pane backing-pixmap x y width height))
            
            (gp:copy-pixels pane backing-pixmap 0 0 port-width port-height 0 0)
            
            (when (or dirty needs-legend)
              (draw-accumulated-legend pane pane)
              (setf dirty nil
                    needs-legend nil))
            
            (when full-crosshair
              (draw-crosshair-lines pane full-crosshair prev-x prev-y))
            
            (when (or (mark-x pane)
                      (mark-y pane))
              (draw-mark pane))
            
            ))
    (um:when-let (mbox (shiftf reply-mbox nil))
      (mp:mailbox-send mbox :done))
    ))
    
(defun resize-callback (pane x y width height)
  (declare (ignore x y width height))
  (with-accessors ((resize-timer  plotter-resize-timer   )
                   (backing-image plotter-backing-image  )
                   (backing-pixmap plotter-backing-pixmap)) pane
    (unless backing-image
      ;; if we already have a backing image then this is another call
      ;; in the same resize operation
      (if backing-pixmap
          ;; but if we don't already have a backing pixmap then we
          ;; probably haven't been instantiated just yet, so don't do anything.
          (save-backing-image pane pane)))
    (unless resize-timer
      (setf resize-timer
            (mp:make-timer
             (lambda ()
               (capi:apply-in-pane-process pane
                                           (lambda ()
                                             (discard-backing-image  pane)
                                             (redraw-entire-pane pane)
                                             )))
             )))
    (mp:schedule-timer-relative-milliseconds resize-timer 100)
    ))

(defun compute-x-y-at-cursor (pane x y)
  (with-accessors  ((sf         plotter-sf                 )
                    ;; (magn       plotter-magn               )
                    (inv-xform  plotter-inv-xform          )
                    (xlog       plotter-xlog               )
                    (ylog       plotter-ylog               )
                    (x-readout-hook  plotter-x-readout-hook)
                    (y-readout-hook  plotter-y-readout-hook)) pane
    (let ((eff-sf sf)) ;; (* sf magn)))
      (multiple-value-bind (xx yy)
          (gp:transform-point inv-xform (/ x eff-sf) (/ y eff-sf))
        (list (funcall ;;real-eval-with-nans
               (if xlog
                   (um:compose x-readout-hook #'pow10)
                 x-readout-hook)
               xx)
              (funcall ;;real-eval-with-nans
               (if ylog
                   (um:compose y-readout-hook #'pow10)
                 y-readout-hook)
               yy)
              ))
      )))

(defmethod display-cursor-readout (intf name x y)
  (declare (ignore intf name x y))
  ;; base method does nothing
  nil)

(defmethod display-cursor-readout ((obj capi:simple-pane) name x y)
  ;; default for CAPI panes - defer to parent object
  (display-cursor-readout (capi:element-parent obj) name x y))

(defun mouse-move (pane x y &rest args)
  (declare (ignore args))
  (cond ((on-legend pane x y) (highlight-legend pane))
        (t  (unhighlight-legend pane)
            #-:WIN32 (capi:display-tooltip pane)
            ;; #+:WIN32 (capi:display-tooltip pane)
            (destructuring-bind (xx yy) (compute-x-y-at-cursor pane x y)
              (display-cursor-readout pane
                                      (capi:capi-object-name pane) xx yy))
            
            (with-accessors ((full-crosshair plotter-full-crosshair)
                             (prev-x         plotter-prev-x)
                             (prev-y         plotter-prev-y)) pane
              
              (when full-crosshair ;; NIL or a color spec
                
                #+(AND :WIN32 (NOT (OR :LISPWORKS6.1 :LISPWORKS7)))
                (progn
                  (draw-crosshair-lines pane full-crosshair prev-x prev-y)
                  (draw-crosshair-lines pane full-crosshair x      y)
                  
                  (setf prev-x x
                        prev-y y))
                
                #+(OR :COCOA :LISPWORKS6.1 :LISPWORKS7)
                (let ((xx (shiftf prev-x x))
                      (yy (shiftf prev-y y)))
                  (when (and xx yy)
                    (let ((wd (gp:port-width pane))
                          (ht (gp:port-height pane)))
                      (if (plotter-backing-pixmap pane)
                          (progn
                            (gp:invalidate-rectangle pane (1- xx) 0 3 ht)
                            (gp:invalidate-rectangle pane 0 (1- yy) wd 3))
                        (gp:invalidate-rectangle pane)) ))))
              ))))

(defun show-x-y-at-cursor (pane x y &rest _)
  (declare (ignore _))
  (cond ((on-legend pane x y) (start-drag-legend pane x y))
        
        (t (destructuring-bind (xx yy) (compute-x-y-at-cursor pane x y)
             (labels ((fmt (val)
                        ;; (format nil "~,5g" val)
                        (string-trim " "
                                     (if (realp val)
                                         (engfmt:engineering-format nil val :nsig 3)
                                       (format nil "~A" val)))
                        ))
               (let* ((xstr (fmt xx))
                      (ystr (fmt yy))
                      (mx   (mark-x pane))
                      (my   (mark-y pane))
                      (txt  (cond ((and mx my
                                        (realp mx)
                                        (realp my))
                                   (let* ((dxstr (fmt (- xx (mark-x pane))))
                                          (dystr (fmt (- yy (mark-y pane))))
                                          (wd    (max (length xstr) (length dxstr))))
                                     (format nil " x ~vA   y ~A~&dx ~vA  dy ~A"
                                             wd xstr ystr wd dxstr dystr)))
                                  
                                  ((and mx
                                        (realp mx))
                                   (let* ((dxstr (fmt (- xx (mark-x pane))))
                                          (wd    (max (length xstr) (length dxstr))))
                                     (format nil " x ~vA   y ~A~&dx ~vA"
                                             wd xstr ystr wd dxstr)))
                                  
                                  ((and my
                                        (realp my))
                                   (let* ((dystr (fmt (- yy (mark-y pane))))
                                          (wd    (length xstr)))
                                     (format nil " x ~vA   y ~A~&~vTdy ~A"
                                             wd xstr ystr (+ wd 5) dystr)))
                                  
                                  (t
                                   (format nil "x ~A  y ~A" xstr ystr))
                                  )))
                 
                 (capi:display-tooltip pane
                                       :x  (+ x 10)
                                       :y  (+ y 10)
                                       :text txt)
                 (capi:set-clipboard pane txt)
                 ))))))

(defun mark-x-at-cursor (pane x y &rest _)
  (declare (ignore _))
  (destructuring-bind (xx yy) (compute-x-y-at-cursor pane x y)
    (declare (ignore yy))
    (let ((sf (plotter-sf pane)))
      (setf (mark-x pane) xx
            (mark-x-raw pane) (/ x sf)
            (mark-y pane) nil
            (mark-y-raw pane) nil)
      (redraw-entire-pane pane)
      )))

(defun mark-y-at-cursor (pane x y &rest _)
  (declare (ignore _))
  (destructuring-bind (xx yy) (compute-x-y-at-cursor pane x y)
    (declare (ignore xx))
    (let ((sf (plotter-sf pane)))
      (setf (mark-y pane) yy
            (mark-y-raw pane) (/ y sf)
            (mark-x pane) nil
            (mark-x-raw pane) nil)
      (redraw-entire-pane pane)
      )))
  
(defun mark-x-y-at-cursor (pane x y &rest _)
  (declare (ignore _))
  (destructuring-bind (xx yy) (compute-x-y-at-cursor pane x y)
    (let ((sf (plotter-sf pane)))
      (setf (mark-x pane) xx
            (mark-x-raw pane) (/ x sf)
            (mark-y pane) yy
            (mark-y-raw pane) (/ y sf))
      (redraw-entire-pane pane)
      )))

(defun unmark-x-y (pane &rest _)
  (declare (ignore _))
  (setf (mark-x pane) nil
        (mark-y pane) nil
        (mark-x-raw pane) nil
        (mark-y-raw pane) nil)
  (redraw-entire-pane pane))

;; user callable function
(defun set-x-readout-hook (pane fn)
  (let ((pane (plotter-mixin-of pane)))
    (setf (plotter-x-readout-hook pane) fn)))

;; user callable function
(defun set-y-readout-hook (pane fn)
  (let ((pane (plotter-mixin-of pane)))
    (setf (plotter-y-readout-hook pane) fn)))

;; -----------------------------------------------------------

(defun do-with-bare-pdf-image (pane fn)
  (setf (plotter-delay-backing pane) t)
  (gp:invalidate-rectangle pane)
  (unwind-protect
      (funcall fn)
    (setf (plotter-delay-backing pane) nil)
    ))
  
(defmacro with-bare-pdf-image ((pane) &body body)
  `(do-with-bare-pdf-image ,pane (lambda () ,@body)))

(defun draw-nominal-image (pane port)
  (with-accessors ((nominal-width   plotter-nominal-width )
                   (nominal-height  plotter-nominal-height)
                   (sf              plotter-sf            )
                   (magn            plotter-magn          )
                   (xform           plotter-xform         )) pane

    (let ((save-xform  xform)
          (save-magn   magn)
          (save-sf     sf))
      (unwind-protect
          (progn
            (gp:clear-graphics-port-state pane)
            
            (setf xform '(1 0 0 1 0 0)
                  magn  1
                  sf    1)

            (redraw-display-list pane port 0 0 nominal-width nominal-height :legend t))
        
        (progn
          (setf sf    save-sf
                magn  save-magn
                xform save-xform))
        ))))

(defun get-nominal-image (pane)
  ;; should only be called by the capi process
  (let* ((xpane (create-pixmap-port pane
                                    (plotter-nominal-width pane)
                                    (plotter-nominal-height pane)
                                    :background (background-color pane)
                                    :foreground (foreground-color pane)
                                    :clear      t)))
    ;; this avoids image artifacts due to image shrinkage or expansion
    ;; just draw at original (nominal) scale
    (draw-nominal-image pane xpane)
    (values xpane (gp:make-image-from-port xpane))
    ))

(defmacro with-nominal-image ((pane img) &body body)
  ;; should only be used by functions called by the capi process
  (let ((xpane (gensym)))
    `(multiple-value-bind (,xpane ,img)
         (get-nominal-image ,pane)
       (unwind-protect
           (progn
             ,@body)
         (progn
           (gp:free-image ,xpane ,img)
           (gp:destroy-pixmap-port ,xpane))))
    ))

;; ----------------------------------------------------------
;;

(defvar *last-save-path* nil)

(defun get-dest-path ()
  (let ((path (capi:prompt-for-file
               "Write Image to File"
               :operation :save
               :filter #-:WIN32 "*.pdf"
                       #+:WIN32 "*.bmp"
               :pathname *last-save-path*)))
    (if path
        (setf *last-save-path* path))
    path))

;; user callable function
(defun save-image (pane file &key &allow-other-keys)
  ;; can be called from anywhere
  (let ((dest (or file
                  (get-dest-path))))
    (when dest
      (let ((pane (plotter-mixin-of pane)))
        (sync-with-capi pane
                        #-:WIN32
                        (lambda ()
                          (with-bare-pdf-image (pane)
                            (save-pdf-plot pane (namestring dest))
                            ))
                        #+:WIN32
                        (lambda ()
                          (with-nominal-image (pane img)
                            (let ((eimg (gp:externalize-image pane img)))
                              (gp:write-external-image eimg dest
                                                       :if-exists :supersede)
                              )))
                        )))
    ))

;; user callable function
(defun save-plot (&rest args)
  (apply #'save-image args))

(defun save-image-from-menu (pane &rest args)
  ;; called only in the pane's process
  (declare (ignore args))
  (let ((dest (get-dest-path)))
    (when dest
      #-:WIN32
      (with-bare-pdf-image (pane)
        (save-pdf-plot pane (namestring dest)))
      #+:WIN32
      (save-image pane (namestring dest))
      )))

(defun copy-image-to-clipboard (pane &rest args)
  ;; called only as a callback in the capi process
  (declare (ignore args))
  #-:WIN32
  (with-bare-pdf-image (pane)
    (copy-pdf-plot pane))
  #+:WIN32
  (with-nominal-image (pane img)
    (capi:set-clipboard pane nil nil (list :image img)))
  )

(defun print-plotter-pane (pane &rest args)
  (declare (ignore args))
  ;; executed in the process of the capi pane
  #-:WIN32
  (with-bare-pdf-image (pane)
    (capi:simple-print-port pane
                            :interactive t))
  #+:WIN32
  (with-nominal-image (pane img)
    (capi:simple-print-port pane
                            :interactive t))
  )

;; user callable function
#-(OR :LISPWORKS6.1 :LISPWORKS7)
(defun set-full-crosshair (pane full-crosshair)
  (let ((pane (plotter-mixin-of pane)))
    (sync-with-capi pane
                    (lambda ()
                      (setf (plotter-full-crosshair pane)
                            #-:WIN32 full-crosshair
                            #+:WIN32
                            (and full-crosshair
                                 (complementary-color pane full-crosshair
                                                      (background-color pane))
                                 ))
                      (when (null full-crosshair)
                        (setf (plotter-prev-x pane) nil
                              (plotter-prev-y pane) nil))

                      (redraw-entire-pane pane))
                    )))

#+(OR :LISPWORKS6.1 :LISPWORKS7)
(defun set-full-crosshair (pane full-crosshair)
  (let ((pane (plotter-mixin-of pane)))
    (sync-with-capi pane
                    (lambda ()
                      (setf (plotter-full-crosshair pane)
                            full-crosshair)
                      (when (null full-crosshair)
                        (setf (plotter-prev-x pane) nil
                              (plotter-prev-y pane) nil))

                      (redraw-entire-pane pane))
                    )))

;; called only from the plotter-window menu (CAPI process)
#-(OR :LISPWORKS6.1 :LISPWORKS7)
(defun toggle-full-crosshair (pane &rest args)
  (declare (ignore args))
  (setf (plotter-full-crosshair pane)
        (if (plotter-full-crosshair pane)
            (setf (plotter-prev-x pane) nil
                  (plotter-prev-y pane) nil)
          #-:WIN32 :red
          #+:WIN32 (complementary-color pane :red
                                        (background-color pane))))
  (redraw-entire-pane pane)
  )

#+(OR :LISPWORKS6.1 :LISPWORKS7)
(defun toggle-full-crosshair (pane &rest args)
  (declare (ignore args))
  (setf (plotter-full-crosshair pane)
        (if (plotter-full-crosshair pane)
            (setf (plotter-prev-x pane) nil
                  (plotter-prev-y pane) nil)
          :red))
  (redraw-entire-pane pane))

