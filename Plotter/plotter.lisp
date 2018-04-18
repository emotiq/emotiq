;; plotter.lsp -- Plotting support for Lisp
;; DM 02/07
;;
;; The basic notions are as follows:
;;
;; For Mac OS/X we want to utilize Display PDF to the max. We can do that by drawing
;; directly in the pane. Making a backing store image of the screen looks good only while
;; viewing the screen. Making such a backing image interferes with nice PDF file output, or
;; copy/paste. So for those times, we avoid constructing an image of the screen to allow the
;; full PDF elegance to shine through.
;;
;; OS/X Cocoa cannot perform XOR image combination in PDF space. So for full cross-hairs
;; that follow the cursor position, we resort to a fast(!!) copying of the backing store
;; image to the screen followed by overdrawing of the crosshairs.

;; For Win/XP the output does not look as nice since Win/XP is limited to bitmapped
;; graphics. Furthermore, it is necessary to draw initially in an off-screen compatible
;; pixmap, then create an image of that pixmap for backing store and for direct transfer
;; the output screen. Only in this way is it possible to produce backing images unmolested
;; by overlapping windows. In general Win/XP is severely limited. E.g., it cannot use
;; fractional line widths or coordinates. Bad things happen if you try. So we intercept
;; those possibilities and produce corrected requests on behalf of the caller.
;;
;; Win/XP can produce output with XOR combination, so we don't have to use the heavy-handed
;; approach of constantly refreshing the image on the screen for full crosshair cursors. We
;; just need to use the overall BG-complement of the desired cursor color in XOR draw mode.
;;
;; So, to accommodate both kinds of drawing needs with one body of source code, most of
;; the drawing primitive routines take two arguments - pane and port -- in addition to
;; other specializing arguments. The pane refers to the <plotter-pane> object described
;; below, which contains all of the plotting specific information. The port object is
;; that used by the GP primitives for the actual drawing operations. On OS/X the pane and the
;; port point to the same underlying object. But for Win/XP the pane is the <plotter-pane>
;; and the port is a temporary off-screen pixmap port.
;;
;; Until the backing store image of the screen exists, both systems utilze an ordered
;; collection of lambda closures representing the various plotting commands needed to build
;; up the whole image. Once those commands have executed we can grab a copy of the
;; screen image for use as a fast-copy backing store.
;;
;; ------------------------------------------
;; All of the plotting commands now require a keyword PANE argument
;; so that our plotting routines are multiprocessing safe, and can operate on
;; an embedded <plotter-pane> or some subclass thereof...
;; There is no longer any notion of a "current plotting window".
;;

(in-package :PLOTTER)

;; ------------------------------------------
;; ------------------------------------------------------------------------------------
#|
(progn
  (setf plt1 (window 'plt1 :xsize 600 :ysize 378 :cursor nil))
  (setf img1 (read-image "/Volumes/Repository/World Map Images/1345001162-B.jpg"))
  (render-image 'plt1 img1))

(plot plt1
      (mapcar #'second places)
      (mapcar #'third places)
      :symbol :circle
      :symbol-filled :t
      
      :border-color  :black)

(setf places
      `(("Boston"        164 111)
        ("San Francisco"  87 117)
        ("Tucson"         96 127)
        ("Seattle"        89 102)
        ("Chicago"       145 110)
        ("Denver"        124 119)
        ("Mexico City"   120 158)
        ("Paris"         288 100)
        ("Sao Paulo"     207 242)
        ("Tel Aviv"      340 136)
        ("Montreal"      160 103)
        ("Melbourne"     513 284)
        ("Sydney"        524 276)
        ("Peking"        467 122)
        ("Brussels"      293  95)
        ("Lagos"         281 190)
        ("London"        283  92)
        ("Stockholm"     312  78)
        ("Moscow"        351  87)
        ("Miami"         153 147)
        ("Madrid"        276 118)
        ("Athens"        320 126)
        ("Tokyo"         507 135)
        ("Hong Kong"     468 161)
        ("Honolulu"       34 165)
        ("Ho Chi Minh"   454 186)
        ("Anchorage"      34  74)
        ("Bangalore"     407 181)))

(setf img1 (read-image "/Volumes/Repository/World Map Images/1345001162-B.jpg"))
(setf img1 (read-image "/Volumes/Repository/World Map Images/1345001155-B.jpg"))
(setf img1 (read-image "/Volumes/Repository/World Map Images/1345001163-B.jpg"))

(defun show-city (pane x y &rest args)
  (labels ((dist (city)
             (destructuring-bind (name cx cy) city
               (declare (ignore name))
               (abs (complex (- cx (- x 2)) (- cy (- y 2))))))
           (nearest (c1 c2)
             (<= (dist c1) (dist c2))))
    (if (some (lambda (c)
                (< (dist c) 5))
              places)
        (let ((splaces (sort places #'nearest)))
          (capi:display-tooltip pane
                                :x (+ x 10)
                                :y (+ y 10)
                                :text (first (first splaces)))
          ))))

(setf w1 (make-instance 'capi:output-pane
                        :visible-min-width 600
                        :visible-max-width 600
                        :visible-min-height 378
                        :visible-max-height 378
                        :input-model   '((:motion show-city))
                        ))

(setf intf (capi:contain w1
                         :window-styles '(:internal-borderless)
                         ))
(capi:apply-in-pane-process w1
                            (lambda ()
                              (with-image (w1 (img (gp:convert-external-image w1 img1)))
                                (gp:draw-image w1 img 0 0))
                              (let ((xs (mapcar #'second places))
                                    (ys (mapcar #'third places)))
                                (loop for x in xs
                                      for y in ys
                                      do
                                      (gp:draw-circle w1 x y 3
                                                      :filled t
                                                      :foreground :green3)
                                      (gp:draw-circle w1 x y 3
                                                      :foreground :black)
                                      ))))

|#
;; ------------------------------------------------------------------------------------
#|
;; for debugging...
(defun dump-hex (arr &key (nlines 10))
  (loop for ix from 0 below (array-total-size-of arr) by 16
        for line from 0 below nlines
        do
        (format t "~%~4,'0x: ~{~{~2,'0x ~} ~} ~A"
                ix
                (loop for jx from 0 below 16 by 4
                      collect
                      (coerce (subseq-of arr (+ ix jx) (+ ix jx 4)) 'list))
                (let ((s (make-string 16)))
                  (loop for jx from 0 below 16 do
                        (setf (aref s jx)
                              (let ((v (code-char (aref-of arr (+ ix jx)))))
                                (if (graphic-char-p v)
                                    v
                                  #\.))
                              ))
                  s))
        ))

(defun sinc (x)
  (/ (sin x) x))
|#

#|
(window 'tst)
(fplot 'tst '(0.001 10) (lambda (x) (/ (sin x) x)))
(tvscl 'tst *tst-img* :magn 4)
|#

;; ------------------------------------------
#| Test code...

(let (x y)
  (defun ramp (min max npts)
    (let ((val (make-array npts))
          (rate (/ (- max min) npts)))
      (dotimes (ix npts val)
        (setf (aref val ix) (+ min (* ix rate))))
      ))
  
  (setf x (ramp -10 10 100))
  (defun sinc (val)
    (if (zerop val)
        1.0
      (/ (sin val) val)))
  (setf y (map 'vector 'sinc x))
  
  (window 'tst :xsize 400 :ysize 300)
  (plot 'tst x y 
        :color (color:make-rgb 1.0 0.0 0.0 0.25) ;;:red
        :thick 2
        :title "Sinc(x)"
        :xtitle "X Values"
        :ytitle "Y Values")

  ;;  (window 'tst :background :black :foreground :yellow :xsize 400 :ysize 300)
  ;;  (plot 'tst x y 
  ;;        :color (color:make-rgb 1.0 0.0 1.0 0.25) ;;:magenta
  ;;        :linewidth 2
  ;;        :fullgrid (color:make-gray 0.25)
  ;;        :title "Sinc(x)"
  ;;        :xtitle "X Values"
  ;;        :ytitle "Y Values")
  )
|#


;; *eof* ;;


#|
;; test code for alternate logos...
(let ((logo (gp:read-external-image "/Users/davidmcclain/Desktop/MMWSim Logo4.pdf"))
      (logo-alpha 0.2)
      (win (plt:wset 'sinc :clear t)))
  (plt:fplot win '(-20 20) (lambda (x) (/ (sin x ) x))
             :thick 2 :color :darkgreen
             :logo logo
             :logo-alpha logo-alpha))

;; test default logo
(let ((win (plt:wset 'sinc :clear t)))
  (plt:fplot win '(-20 20) (lambda (x) (/ (sin x ) x))
             :logo *ext-logo*
             :thick 2 :color :darkgreen))

|#

;; ======================================================================================
