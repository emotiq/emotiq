
(in-package :plotter)

;; ------------------------------------------
;; Win32/OS X compatibility constants and functions
;; ------------------------------------------
(defconstant $tiny-times-font-size
  #-:WIN32 10
  #+:WIN32  8)

(defconstant $normal-times-font-size
  #-:WIN32 12
  #+:WIN32  9)

(defconstant $big-times-font-size
  #-:WIN32 14
  #+:WIN32 10)
  
(defun find-best-font (pane &key
                            (family "Times")
                            size
                            (weight :normal)
                            (slant  :roman))
  (gp:find-best-font (display-pane-of pane)
                     (gp:make-font-description
                      :family family
                      :size   #-:WIN32 size #+:WIN32 (round size)
                      :weight weight
                      :slant  slant)))

;; ------------------------------------------
#+:WIN32x
;; -- Under Windows/7+ we seem to not need this anymore...
;; DM/RAL  05/14
(defun adjust-linewidth (wd)
  ;; Win/XP can't handle fractional linewidths
  (max 1 (round wd)))

;; #-:WIN32
(defun adjust-linewidth (wd)
  ;; ... but Display PDF can...
  wd)

;; ------------------------------------------
(defun background-color (pane)
  (gp:graphics-state-background
   (gp:get-graphics-state pane)))

(defun foreground-color (pane)
  (gp:graphics-state-foreground
   (gp:get-graphics-state pane)))

#+:WIN32x
;; --- WE FINALLY HAVE DECENT ALPHA BLENDING ON WINDOW 7+ --
;; DM/RAL 05/14
(defun adjust-color (pane color &optional alpha)
  ;; Win/XP can't handle true alpha blending. So we use a make-pretend system
  ;; that assumes the color will be blending with the background color. That only
  ;; works properly as long as the drawing is actually over that background color.
  (let* ((c (color:get-color-spec
             (color:unconvert-color
              pane
              (color:convert-color pane color))))
         (a (or alpha (color:color-alpha c))))
    (if (= 1 a)
      color
      (let* ((bg  (color:get-color-spec
                   (color:unconvert-color pane
                                          (color:convert-color pane
                                                               (background-color pane)))))
             (1-a (- 1.0 a)))
        (labels ((mix (fn)
                   (+ (* 1-a (funcall fn bg))
                      (* a   (funcall fn c)))))
        (color:make-rgb 
         (mix #'color:color-red)
         (mix #'color:color-green)
         (mix #'color:color-blue))
        )))))
      
;; #-:WIN32
(defun adjust-color (pane color &optional alpha)
  ;; Mac OS/X Cocoa can do real alpha blending. Here we take the user's
  ;; requested color, and a possibly separate alpha level (alpha might be nil)
  ;; to produce a color that will be properly alpha blended over a varying background.
  (if (null alpha)
      color
    (let ((c (color:get-color-spec
              (color:unconvert-color pane color))))
      (color:make-rgb
       (color:color-red   c)
       (color:color-green c)
       (color:color-blue  c)
       alpha)
      )))

#+:WIN32
(defun complementary-color (pane color background)
  ;; produce a color such that XOR mode of that color against the background
  ;; will produce the requested color...
  (let* ((c  (color:get-color-spec
              (color:unconvert-color pane color)))
         (bg (color:get-color-spec
              (color:unconvert-color pane background))))
    
    (labels ((color-xor (compon1 compon2)
               (let ((icompon1  (round (* 255 compon1)))
                     (icompon2  (round (* 255 compon2))))
                 (/ (logxor icompon1 icompon2) 255.0)))

             (color-xor-components (fn)
               (color-xor (funcall fn c) (funcall fn bg))))
      
      (color:make-rgb
       (color-xor-components #'color:color-red)
       (color-xor-components #'color:color-green)
       (color-xor-components #'color:color-blue))
      )))

;; ------------------------------------------
#+:WIN32x
(defun adjust-box (box)
  ;; Win/XP can't handle fractional box coords
  (mapcar #'round box))

;; #-:WIN32
(defun adjust-box (box)
  ;; ... but OS/X Cocoa can...
  box)

;; ---------------------------------------------------------
;; WIN32 has some low-level pixmap routines that aren't reentrant
;; so we fix that up here by using our own lock to prevent multiple
;; threads from concurrent execution of those routines...

#+:WIN32
(defvar *win32-pixmap-lock* (mp:make-lock))

#+:WIN32
(defmacro create-pixmap-port (&rest args)
  `(mp:with-lock (*win32-pixmap-lock*)
     (gp:create-pixmap-port ,@args)))

#+:WIN32
(defmacro with-pixmap-graphics-port (args &body body)
  `(mp:with-lock (*win32-pixmap-lock*)
     (gp:with-pixmap-graphics-port ,args
       ,@body)))

#-:WIN32
(defmacro create-pixmap-port (&rest args)
  `(gp:create-pixmap-port ,@args))


#-:WIN32
(defmacro with-pixmap-graphics-port (args &body body)
  `(gp:with-pixmap-graphics-port ,args
     ,@body))

