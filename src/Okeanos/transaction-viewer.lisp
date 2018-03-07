;; transaction-viewer.lisp -- Database Log File operations
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

(defun read-transaction-log (&key nitems cursor)
  (with-read-lock ()
    (unless cursor
      (setf cursor (create-cursor (transaction-directory))))
    (if nitems
        (values
         (loop repeat nitems
               for row = (cursor-next cursor)
               while row
               collect row)
       cursor)
      ;; else - no limit on number of items to read
      (values
       (loop for row = (cursor-next cursor)
             while row
             collect row)
       cursor))))

(defun describe-logfile-transaction (tid &key (stream t) skip-types
                                         (nbr-entries 100) (nbr-skip 0))
  (atomic (:retries 0)
    (if-let (row (fetch-row `(:tid ,tid) (transaction-directory)))
        (with-open-file (f (logfile-path (getf row :file-id))
                           :direction    :input
                           :element-type '(unsigned-byte 8))
          (file-position f (getf row :file-pos))
          (describe-logfile-entries f
                                    :stream      stream
                                    :skip-types  skip-types
                                    :nbr-entries nbr-entries
                                    :nbr-skip    nbr-skip) ))
    (rollback)))


(defclass <transaction-viewer> (capi:interface)
  ((text-pane :reader text-pane :initarg :text-pane)
   (list-pane :reader list-pane :initarg :list-pane)
   ))

(defun make-transaction-viewer ()
  (let* ((tids-pane (make-instance 'capi:list-panel
                                   :items (read-transaction-log)
                                   :selected-item nil
                                   :print-function #'(lambda (row)
                                                     (when-created (getf row :tid)))
                                   :selection-callback
                                   #'(lambda (row interface)
                                     (let* ((pane   (text-pane interface))
                                            (stream (capi:collector-pane-stream pane)))
                                       (capi:apply-in-pane-process
                                        pane
                                        (deferred
                                            (setf (capi:editor-pane-text pane) "")))
                                       (describe-logfile-transaction (getf row :tid)
                                                                     :stream stream
                                                                     :nbr-entries nil)))
                                   :visible-min-height '(:character 10)
                                   :visible-min-width  '(:character 51)
                                   :visible-max-width  '(:character 51)
                                   #+:MACOSX
                                   :font
                                   #+:MACOSX
                                   (gp:make-font-description
                                    :family "Lucida Grande" ;;"Monaco" ;; "Courier New" ;; "Lucida Console"
                                    :size   10 ;; 10 ;; 8 ;; 9
                                    :slant  :roman
                                    :weight :regular)))

         (body-pane (make-instance 'capi:collector-pane
                                   :visible-min-width  '(:character 80)
                                   #+:WIN32
                                   :font
                                   #+:WIN32
                                   (gp:make-font-description
                                    :family "Courier New" ;; "Lucida Console"
                                    :size   8 ;; 9
                                    :slant  :roman
                                    :weight :normal)
                                   :line-wrap-marker #\!
                                   :echo-area t
                                   ;; :wrap-style nil
                                   ))

         (refresh-button (make-instance 'capi:push-button
                                        :text "Refresh"
                                        :callback
                                        #'(lambda (data intf)
                                          (declare (ignore data))
                                          (let ((pane (list-pane intf)))
                                            (capi:apply-in-pane-process
                                             pane
                                             (deferred
                                               (setf (capi:collection-items
                                                      pane)
                                                     (read-transaction-log)
                                                      
                                                     (capi:choice-selection
                                                      pane) nil
                                                     
                                                     (capi:editor-pane-text
                                                      (text-pane intf))  ""))
                                             )))))
         (boxes-row  (make-instance 'capi:row-layout
                                    :description (list tids-pane body-pane)))
         (main-layout (make-instance 'capi:column-layout
                                     :description (list boxes-row refresh-button)))
         (intf (make-instance '<transaction-viewer>
                              :layout main-layout
                              :text-pane body-pane
                              :list-pane tids-pane
                              :title     "Okeanos Transaction Viewer"
                              :window-styles
                              (append '(:textured-background
                                        :movable-by-window-background)) )))
    (capi:display intf)))

;; ----------------------------------------------------------------
#|
(connect-to-database)
(make-transaction-viewer)
(disconnect-from-database)
|#
;; ---------------------------------------------------------------------------------------


#|
(capi:contain
 (make-instance 'capi:list-panel
                :items (mapcar (compose 'when-created (rcurry 'getf :tid))
                               (read-transaction-log))
                ))
  

(capi:define-interface transaction-viewer ()
  ((text-pane :reader text-pane :initarg :text-pane))
  (:panes
   (tids capi:list-panel
         :items (read-transaction-log)
         :print-function #'(lambda (row)
                           (when-created (getf row :tid)))
         :selection-callback #'(lambda (row interface)
                               (describe-logfile-transaction (getf row :tid)
                                                             :stream (capi:collector-pane-stream
                                                                      (text-pane interface))))
                               
         :visible-min-height '(:character 10)
         :visible-min-width  '(:character 30))
   (body capi:collector-pane
         :visible-min-width  '(:character 80)
         
         ))
  (:layouts
   (top-layout capi:row-layout
               '(tids body)))
  (:default-initargs
   :title "Okeanos Transaction Viewer"
   :text-pane body))

(capi:display (make-instance 'transaction-viewer))
  
(defmethod make-number-list-editor ((nrs list)
                                    &key
                                    (title "Number List Editor")
                                    continuation)
  (let* ((ed-panel (make-instance 'capi:editor-pane
                                  :buffer-name "number-list-editor"
                                  :buffer-modes '("Lisp")
                                  :echo-area t))
         (graph-plot   nil)
         (ok-button (make-instance 'capi:push-button
                                   :text "OK"
                                   :visible-min-width 75
                                   :default-p t
                                   :callback #'(lambda (&rest args)
                                               (declare (ignore args))
                                               (let ((vals (handler-case
                                                               (read-number-list
                                                                (capi:editor-pane-text ed-panel))
                                                             (error () nil))))
                                                 (when vals
                                                   (when continuation
                                                     (funcall continuation vals))
                                                   (capi:quit-interface ed-panel))
                                                 ))
                                   ))
         (cancel-button (make-instance 'capi:push-button
                                       :text "Cancel"
                                       :visible-min-width 75
                                       :cancel-p t
                                       :callback #'(lambda (&rest args)
                                                   (declare (ignore args))
                                                   (capi:quit-interface ed-panel))))
         (graph-button (make-instance 'capi:push-button
                                      :text "Graph"
                                      :visible-min-width 75
                                      :callback #'(lambda (&rest args)
                                                  (declare (ignore args))
                                                  (ignore-errors
                                                    (let* ((data (read-number-list 
                                                                  (capi:editor-pane-text ed-panel)))
                                                           (nel  (length data))
                                                           (xs   (vm:framp nel))
                                                           (plt  (setf graph-plot
                                                                       (plt::wshow
                                                                        "Data Viewer"
                                                                        :window-styles
                                                                        '(:always-on-top
                                                                          #+:COCOA :toolbox
                                                                          :textured-background
                                                                          :internal-borderless))))
                                                           
                                                           (spl (interp:spline xs (coerce data 'vector)
                                                                               :natural
                                                                               :natural)))
                                                      (plt:fplot plt (list (reduce 'min xs)
                                                                           (reduce 'max xs))
                                                                 #'(lambda (x)
                                                                   (interp:splint spl x))
                                                                 :clear t
                                                                 :title "Table Plot"
                                                                 :xtitle "Index"
                                                                 :ytitle "Value"
                                                                 :thick  2
                                                                 :legend "Natural Cubic-Spline")
                                                      (plt:plot plt data
                                                                :symbol :circle
                                                                :color  :blue
                                                                :thick  2))))
                                      ))
         (filler-1 (make-instance 'capi:column-layout
                                  :visible-max-height 10))
         (button-panel (make-instance 'capi:row-layout
                                      :description (list ok-button
                                                         cancel-button
                                                         filler-1
                                                         graph-button)))
         (main-layout (make-instance 'capi:column-layout
                                     :description (list ed-panel
                                                        button-panel)))
         (intf (make-instance 'capi:interface
                              :title title
                              :layout main-layout
                              :destroy-callback #'(lambda (&rest args)
                                                  (declare (ignore args))
                                                  (capi:quit-interface graph-plot))
                              :visible-min-height '(:character 20)
                              :best-x  100
                              :best-y  0
                              :window-styles '(:always-on-top
                                               #+:COCOA :toolbox
                                               :textured-background
                                               :movable-by-window-background
                                               ))))
    (setf (capi:editor-pane-text ed-panel)
          (with-output-to-string (s)
            (princ ";; Number List" s)
            (terpri s)
            (loop for nr in nrs
                  do
                  (format s "~A~&" nr)
                  )))
    (capi:display intf)
    ))

|#
