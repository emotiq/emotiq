
(in-package :plotter)

;; ------------------------------------------
(defvar *ext-logo*
  (progn ;; ignore-errors
    (gp:read-external-image
     (translate-logical-pathname
      #+:COCOA "PROJECTS:DYLIB;Logo75Img-Alpha25y.pdf"
      ;; #+:COCOA "PROJECTS:DYLIB;AcudoraLogo.pdf"
      ;; #+:COCOA "PROJECTS:DYLIB;Logo75Img-Alpha20y-BlackBG.pdf"
      ;; "~/Desktop/Watermarks/graph_watermark1.jpg"
      #+:WIN32 "PROJECTS:DYLIB;Logo75Img-Alpha20y-BlackBG.bmp"
      ;; "Logo75Img-Alpha25y.bmp"
      #+:LINUX "~/Linux-stuff/Logo75Img-Alpha25y.bmp"
      ))))

(defvar *ext-logo-alpha* 1)

(defvar *cright1* "Copyright (c) 2006-2018 by Refined Audiometrics Laboratory, LLC")
(defvar *cright2* "All rights reserved.")

(defun stamp-logo (pane port logo logo-alpha)
  (when logo
    (let* ((box (plotter-box pane))
           (bwd (box-width  box))
           (bht (box-height box))
           (sf  (plotter-sf  pane)))
      (with-image (port
                   (image (gp:convert-external-image port logo)))
        (let* ((iwd  (gp:image-width  image))
               (iht  (gp:image-height image))
               (isf  (min (/ bwd iwd)
                          (/ bht iht)))
               (top  (+ (box-top box)
                        (* 0.5 (- bht (* isf iht)))
                        ))
               (left (+ (box-left box)
                        (* 0.5 (- bwd (* isf iwd)))
                        )))
          (gp:with-graphics-scale (port sf sf)
            (gp:draw-image port image left top
                           :from-width  iwd
                           :from-height iht
                           :to-width    (* isf iwd)
                           :to-height   (* isf iht)
                           ;; WATCH OUT! if we don't have a float for global alpha
                           ;; then the COCOA system bombs out really badly...
                           :global-alpha (float logo-alpha 1.0)))
          ))
      )))

(defun watermark (pane port logo logo-alpha cright1 cright2)
  (let* ((box     (plotter-box pane))
         (sf      (plotter-sf  pane))
         (font2   (find-best-font pane
                                  :size (* sf $tiny-times-font-size)))
         (color2  #.(color:make-gray 0.7)))
    
    (stamp-logo pane port logo logo-alpha)
    ;; (com.sd.butterfly.lfm:log-info :system-log "Watermark called")
    #||#
    (let* ((left   (+ (box-left   box) 18))
           (bottom (- (box-bottom box) 30)))
      (draw-string-x-y pane port cright1
                       (* sf left)
                       (* sf (- bottom 11))
                       :x-alignment :left
                       :y-alignment :top
                       :font  font2
                       :color color2)
      (draw-string-x-y pane port cright2
                       (* sf left)
                       (* sf bottom)
                       :x-alignment :left
                       :y-alignment :top
                       :font  font2
                       :color color2)
      )
    #||#
    ))

