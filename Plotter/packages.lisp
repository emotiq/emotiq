
(defpackage :plotter
  (:nicknames #:plt)
  (:use       #:common-lisp #:vector-ops)
  (:export
   #:sinc
   #:find-named-plotter-pane
   #:*plotter-window-class*
   #:<plotter-window>
   #:<plotter-pane>
   #:<plotter-mixin>
   #:window ;; these functions take a symbolic window id argument
   #:wset
   #:wshow
   #:wclose
   
   #:clear  ;; these functions require a <plotter-pane> argument
   #:plot
   #:axes
   #:draw-text
   #:with-delayed-update
   #:histogram
   #:fplot
   #:paramplot
   #:plot-bars
   #:read-image
   #:render-image
   #:plot-image
   #:save-image
   #:save-plot  ;; a synonym for save-image
   #:tvscl
   #:set-x-readout-hook
   #:set-y-readout-hook
   #:display-cursor-readout
   
   #:draw-rect
   #:draw-ellipse
   #:draw-arc

   #:set-full-crosshair

   #:$tiny-times-font-size
   #:$normal-times-font-size
   #:$big-times-font-size

   #:$heat-colormap
   #:$gray-colormap
   #:get-cmap
   #:set-cmap
   
   #:with-default-args
   #:wait-until-finished

   #:helpme

   #:draw-text-box

   #:cmplx-plot
   #:cmplx-paramplot
   #:polar-fplot
   ))

#|
;; generate HTML documentation
(doctools:gen-docs
 :asdf-system-name :plotter
 :package-name     :plotter
 :directory        (translate-logical-pathname "PROJECTS:LISP;Plotter;")
 :subtitle         "a library for scientific graphing")
|#
