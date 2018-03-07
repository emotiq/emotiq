;; tests.lisp -- 
;; --------------------------------------------------------------------------------------
;; OkeanosMM -- memory mapped database system
;;
;; DM/RAL  03/09
;; --------------------------------------------------------------------------------------
#|
The MIT License

Copyright (c) 2008 Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#
;; -------------------------------------------
(in-package #:okeanos)
;; -------------------------------------------

(hcl:change-directory (translate-logical-pathname "PROJECTS:LISP;"))

(unless (find-class 'address-entry nil)
  (load "./Okeanos/tools/examples.lisp"))

(defrpc check ()
  (rollback)
  (map-rows (oid-mappings) (let ((prev (make-oid :uuid (uuid:make-null-uuid))))
                             (lambda (row)
                               (let ((oid (getf row :oid)))
                                 (assert (not (oid= oid (shiftf prev oid))))
                                 t)))))

(defun tst ()
  (sys:call-system "rm -rf ./okeanosdb")
  (connect-to-database)
  (do-tests)
  (disconnect-from-database))

(defun tst2 ()
  (sys:call-system "rm -rf ./okeanosdb")
  (connect-to-database)
  (do-tests2)
  (disconnect-from-database))

#|
(tst2)
|#

(defun tst-remote ()
  (sys:call-system "rm -rf ./okeanosdb")
  (connect-to-database "okdb://RoadKill.local")
  (unwind-protect
      (do-tests2)
    (disconnect-from-database)))

#|
(setf com.sd.okeanos::*timeout* 1000)  
(tst-remote)
|#

(defun do-tests()
  ;; (check)
  (get-file-uuid)
  #| |#
  (atomic ()
    (save-stock "DIA"))
  (check)
  (atomic ()
    (save-stock "VIX"))
  (check)
  (atomic ()
    (let ((xx (make-instance 'ok-set)))
      (add-to-set xx 15)
      (add-to-set xx 32)
      (add-to-set xx 99)))
  (check)
  (atomic ()
    (let ((xx   (make-instance 'ok-map :name "MyJunk/Physical Constants")))
      (setf (getmap xx :c-light) 3e10)
      (setf (getmap xx :planck)  1.6e-16)))
  (check)
  (atomic ()
    (let* ((dave (make-instance 'extended-address-entry
                                :name "Dave"
                                :location "Tucson"
                                :telephone "520-529-2437")))
      (persist
       (list (ref dave)
             (ref (make-instance 'address-entry
                                 :name "Helene"
                                 :location "Tucson"))
             (ref (make-instance 'address-entry
                                 :name "Panos"
                                 :location "Boston"))))
      ))
  (check)
  #| |#
  (print
   (time
    (let ((stocks
           #|'("BKX" "CAC" "DAX" "DIA" "FTSE" "GLD" "HSI" "IWM" "N225"
                     "NDX" "OEF" "OEX" "OIH" "QQQQ" "RUT" "SMH" "SPX" "SPY"
                     "TLT" "VIX" "XAU" "XBD"
                     
                     "AA"  "AIG"  "AMGN" "AXP"  "BA"  "BAC" "C"   "CAT" "CMCSK"
                     "COP" "CSCO" "CVX"  "DD"   "DIS" "DOW" "GE"  #| "GM" |#  "HD"
                     "HPQ" "IBM"  "INTC" "IP"   "JNJ" "JPM" "KFT" "KO"  "LLY" "MCD"
                     "MMM" "MO"   "MRK"  "MSFT" "OXY" "PFE" "PG"  "SLB" "T"
                     "TWX" "UPS"  "UTX"  "VZ"   "WAG" "WFC" "WMT" "WY"  "XOM")|#
           '("BKX" "CAC" "DAX" "DIA") ))
     (print "2nd pass... adding daily histories")
     (dolist (stock stocks)
       (print stock)
       (atomic ()
         (save-stock stock))
       (check)
       )
     )))
  ;; (break)
  )

(defun get-mmf ()
  (mmf:mapped-file-of-mmptr (com.sd.okeanos::database-mapper com.sd.okeanos::*current-okeanos-db*)))

(defun do-tests2()
  ;;(check)
  (get-file-uuid)
  #||#
  (print "Saving DIA")
  (atomic ()
    (save-stock "DIA"))
  ;;(check)
  #| |#
  (loop repeat 10 do
        (atomic ()
          (let ((xx (make-instance 'ok-set)))
            (add-to-set xx 15)
            (add-to-set xx 32)
            (add-to-set xx 99)))
        ;;(check)
        )
  (atomic ()
    (let ((xx   (make-instance 'ok-map :name "MyJunk/Physical Constants")))
      (setf (getmap xx :c-light) 3e10)
      (setf (getmap xx :planck)  1.6e-16)))
  ;;(check)
  (atomic ()
    (let* ((dave (make-instance 'extended-address-entry
                                :name "Dave"
                                :location "Tucson"
                                :telephone "520-529-2437")))
      (persist
       (list (ref dave)
             (ref (make-instance 'address-entry
                                 :name "Helene"
                                 :location "Tucson"))
             (ref (make-instance 'address-entry
                                 :name "Panos"
                                 :location "Boston"))))
      ))
  ;; (check)
  (print "Saving VIX")
  (atomic ()
    (save-stock "VIX"))
  ;; (check)
  )
