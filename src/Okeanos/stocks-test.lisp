;; stocks-test.lisp -- Database Schema representations
;; --------------------------------------------------------------------------------------
;;
;; DM/RAL  08/08
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

(defmethod ensure-mjd (val)
  val)

(defmethod ensure-mjd ((str string))
  (stocks::convert-date-string-to-mjd str))

(defun normalize-mjd (val col)
  (declare (ignore col))
  (ensure-mjd val))

;; ---------------------------------------------------

(defun show-entry (stock-name table row)
  (destructuring-bind (date open high low close volume)
      (mapcar #'(lambda (name)
                  (getf row name))
              '(:mjd :open :high :low :close :volume))
    (format t "~&Stock: ~A~%"  stock-name)
    (format t "~&~D entries~%" (table-count table))
    (format t "~& MJD       Date        Open     High      Low    Close        Volume~%")
    (format t "~&--------------------------------------------------------------------~%")
    (format t "~&~6D  ~10A ~8,2F ~8,2F ~8,2F ~8,2F ~13:D~%"
            date
            (stocks::convert-mjd-to-date-string date)
            open high low close (round volume))))

(defun stock-key (stock-name)
  (format nil "Markets/Price-History/~A" (string-upcase stock-name)))

(defun find-stock-table (stock-name)
  (deref (find-ok-table (stock-key stock-name))))

;; --------------------------------------------------

(defun find-entry (stock-name date)
  (um:if-let (table (find-stock-table stock-name))
      (um:if-let (row (fetch-row `(:mjd ,date) table))
          (show-entry stock-name table row)
        ;; else
        (error "Record not found: ~A" date))
    ;; else
    (error "No stock table: ~A" stock-name)))

(defun delete-entry (stock-name date)
  (um:when-let (table (find-stock-table stock-name))
    (delete-row `(:mjd ,date) table)))

(defun insert-entry (stock-name data)
  (um:when-let (table (find-stock-table stock-name))
    (insert-row data table)))

;; ---------------------------------------------------

(defun first-entry (stock-name)
  (um:when-let (table (find-stock-table stock-name))
    (fetch-first-row table)))

;; ---------------------------------------------------

(defun last-entry (stock-name)
  (um:when-let (table (find-stock-table stock-name))
    (fetch-last-row table)))

;; ---------------------------------------------------

(defun print-heading (stock-name count)
  (format t "~&Stock: ~A~%"  stock-name)
  (format t "~&~D entries~%" count)
  (format t "~&Index  MJD       Date        Open     High      Low    Close        Volume~%")
  (format t "~&--------------------------------------------------------------------------~%"))

(defun show-stock (stock-name &key from to direction max-records)
  (um:when-let (table (find-stock-table stock-name))
    (let ((count 0))
      (print-heading stock-name (table-count table))
      (map-rows table 
                #'(lambda (row)
                  (incf count)
                  (destructuring-bind (date open high low close volume)
                      (mapcar #'(lambda (name)
                                  (getf row name))
                              '(:mjd :open :high :low :close :volume))
                    (format t "~&~4D: ~6D  ~10A ~8,2F ~8,2F ~8,2F ~8,2F ~13:D~%"
                            count date
                            (stocks::convert-mjd-to-date-string date)
                            open high low close (round volume))))
                :from from
                :to   to
                :direction direction
                :max-records max-records)
      )))

;; ---------------------------------------------------------------------------------

#+:LISPWORKS
(defun view-stock-btree (stock-name)
  (um:when-let (table (find-stock-table stock-name))
    (view-btree table)))

#|
(defun view-btree (stock-name)
  (with-read-lock ()
    (um:when-let (ptable (find-stock-table stock-name))
      (let ((ptree (primary-key-btree ptable :mjd)))
        (btree:with-locked-btree (ptree)
          (um:if-let (root (btree:root-node ptree))
              (capi:contain
               (make-instance
                'capi:graph-pane
                :title (format nil "B-Tree for ~A" stock-name)
                :roots (list root)
                :children-function #'(lambda (node)
                                     (let* ((height (btree:node-height node))
                                            (limit  (btree:node-fill-pointer node)))
                                       (when (> height 1)
                                         (nreverse
                                          (loop for ix from 0 below limit by 2 collect
                                                (btree:node-list-cell node ix))))))
                :print-function #'(lambda (node)
                                  (let* ((height (btree:node-height node))
                                         (limit  (btree:node-fill-pointer node)))
                                    (format nil "~X (H:~D, N:~D)"
                                            (mmf:pointer-address node) height
                                            (if (> height 1) (truncate limit 2) limit))))
                ))
            
            ;; else
            (error "Nothing to show")
            ))))))
|#

;; ---------------------------------------------------

(defconstant +stock-price-history-schema+  "Markets/Price-History-Entry")

(fli:define-c-struct stock-detail_t
  (:byte-packing 1)
  (date   uint32)
  (filler uint32)
  (open   flt64)
  (high   flt64)
  (low    flt64)
  (close  flt64)
  (volume flt64))

(defun make-stock-table (stock-symbol)
  (or (find-stock-table stock-symbol)
      (let ((schema (or (find-ok-schema +stock-price-history-schema+)
                        (make-instance 'ok-schema
                                       :name +stock-price-history-schema+
                                       :cols '((:mjd              :number
                                                :c-access-spec    date
                                                :c-type           uint32
                                                :constraint       mmf:i>0
                                                :compare-function <
                                                :value-normalizer normalize-mjd
                                                :indexed          :unique)
                                               
                                               (:open             :number
                                                :c-access-spec    open
                                                :c-type           flt64
                                                :constraint       mmf:d>=0
                                                :compare-function <)
                                               
                                               (:high             :number
                                                :c-access-spec    high
                                                :c-type           flt64
                                                :constraint       mmf:d>=0
                                                :compare-function <)
                                               
                                               (:low              :number
                                                :c-access-spec    low
                                                :c-type           flt64
                                                :constraint       mmf:d>=0
                                                :compare-function <)
                                               
                                               (:close            :number
                                                :c-access-spec    close
                                                :c-type           flt64
                                                :constraint       mmf:d>=0
                                                :compare-function <)
                                               
                                               (:volume           :number
                                                :c-access-spec    volume
                                                :c-type           flt64
                                                :constraint       mmf:d>=0
                                                :compare-function <))
                                       :c-type 'stock-detail_t) )))
        (make-instance 'ok-table
                       :name         (stock-key stock-symbol)
                       :schema       schema) )))

;; --------------------------------------------------------------------

(defstruct stock
  date
  open
  high
  low
  close
  volume)

(defun update/save-stock (stock-name stock-data)
  #|
     Gather the raw data for stock-name into separate vectors
     for date, open, high, low, close, and volume.
     Dates are presented as mixed-format date strings,
     e.g., "2009-07-23", "Jul-23-09", etc.
     Place these vectors into a struct named stock (shown above),
     and bound locally as stock-data.
  |#
  (let* ((table     (make-stock-table stock-name))
         (limit     (length (stock-date stock-data)))
         (col-names `(:mjd :open :high :low :close :volume))
         (pvect     (make-array (length col-names))))
    
    (labels ((get-data (ix col-names pvect)
               (map-into pvect #'(lambda (name)
                                 (let ((v (funcall (ecase name
                                                     (:mjd    'stock-date)
                                                     (:open   'stock-open)
                                                     (:high   'stock-high)
                                                     (:low    'stock-low)
                                                     (:close  'stock-close)
                                                     (:volume 'stock-volume) )
                                                   stock-data)))
                                   (aref v ix)))
                         col-names))
               
             (insert (ix)
               (insert-row
                (loop for col-name in col-names
                      for val across (progn
                                       (get-data ix col-names pvect)
                                       pvect)
                      collect col-name
                      collect val)
                table)) )

      (um:if-let (row (fetch-last-row table))
          (let ((last-date (getf row :mjd)))
            (labels ((compare-stock (ix)
                       (- last-date (ensure-mjd (aref (stock-date stock-data) ix)) )))
              (multiple-value-bind (found-it ix)
                  (um:binsearch 0 limit #'compare-stock)
                (declare (ignore found-it))
                (loop for kx from (1+ ix) below limit do (insert kx)) )))

        ;; else - no data exists yet
        ;; (dotimes (ix limit) (insert ix))) ;; slowly
        (bulk-load-table table limit #'get-data)) ;; quickly, about 5x faster
      )))

(defun save-stock (stock-name)
  (let* ((data (stocks::get-data (stocks::market-data
                                  (concatenate 'string stock-name ".csv"))))
         (stock (make-stock
                 :date   (reverse (csv:get-column "Date" data))
                 :open   (stocks::get-numeric-column "Open"   data)
                 :high   (stocks::get-numeric-column "High"   data)
                 :low    (stocks::get-numeric-column "Low"    data)
                 :close  (stocks::get-numeric-column "Close"  data)
                 :volume (stocks::get-numeric-column "Volume" data))))
    ;; stock
    ;; (return-from save-stock) ;; for base timings
    (update/save-stock stock-name stock)
    ))

;; ---------------------------------------------------------------------------------
#|
(sys:call-system "rm -rf ./okeanosdb")

(connect-to-database)
(connect-to-database nil t) ;; forced open
(connect-to-database "okdb://RoadKill.local")
(disconnect-from-database)
(break-lock)
(defparameter *ptable* (find-stock-table "VIX"))

;; (pprint (collect-all-keys))
;; (pprint (sort (collect-all-keys) 'string<))
(get-file-uuid)
(rollback)
(view-stock-btree "VIX")
(show-stock "VIX")
(show-stock "AA")
(show-stock "DIA")
;; (deserialize-object-from-database "SYS://*OID-FILES*")
(setf *print-length* 10)
(setf *print-circle* t)
(com.sd.okeanos.int::make-transaction-viewer)

(atomic ()
  (reset-current-transaction))

(atomic ()
  (save-stock "DIA"))

(atomic ()
  (save-stock "VIX"))

(atomic ()
  (make-stock-table "VIX"))

(atomic ()
  (make-stock-table "DIA"))

(atomic ()
  (make-stock-table "BKX"))

(atomic ()
  (save-stock "BKX"))

;; for update maintenance
(time
 (let ((stocks '("BKX" "CAC" "DAX" "DIA" "FTSE" "GLD" "HSI" "IWM" "N225"
                 "NDX" "OEF" "OEX" "OIH" "QQQ"  "RUT" "SMH" "SPX" "SPY"
                 "TLT" "VIX" "XAU" "XBD"
                 
                 "AA"  "AIG"  "AMGN" "AXP"  "BA"  "BAC" "C"   "CAT" "CMCSK"
                 "COP" "CSCO" "CVX"  "DD"   "DIS" "DOW" "GE"  #| "GM" |#  "HD"
                 "HPQ" "IBM"  "INTC" "IP"   "JNJ" "JPM" "KFT" "KO"  "LLY" "MCD"
                 "MMM" "MO"   "MRK"  "MSFT" "OXY" "PFE" "PG"  "SLB" "T"
                 "TWX" "UPS"  "UTX"  "VZ"   "WAG" "WFC" "WMT" "WY"  "XOM")))
   #|
   (print "1st pass... allocating stock tables")
   (atomic ()
     (dolist (stock stocks)
       (print stock)
       (make-stock-table stock)))
   |#
   #| |#
   (print "2nd pass... adding daily histories")
   (dolist (stock stocks)
     (print stock)
     (atomic ()
       (save-stock stock)))
   #| |#
   ))

(rollback)
(describe-logfile :last)

(bflyx:spawn #'(lambda ()
              (connect-to-database "okdb://RoadKill.local")
              (sleep 10)))
|#

   
