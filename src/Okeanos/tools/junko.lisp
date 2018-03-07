(defun make-stock-table (stock-symbol)
  (make-instance 'ok-table
                 :name         stock-symbol
                 :schema-name  +stock-price-history-schema+
                 :schema-cols  '((:mjd number
                                  :c-access-spec    date
                                  :c-type           uint32
                                  :constraint       i>0
                                  :compare-function <
                                  :value-normalizer normalize-mjd
                                  :indexed          :unique)
                                 
                                 (:open number
                                  :c-access-spec    open
                                  :c-type           flt64
                                  :constraint       d>=0
                                  :compare-function <)
                                 
                                 (:high number
                                  :c-access-spec    high
                                  :c-type           flt64
                                  :constraint       d>=0
                                  :compare-function <)
                                 
                                 (:low number
                                  :c-access-spec    low
                                  :c-type           flt64
                                  :constraint       d>=0
                                  :compare-function <)
                                 
                                 (:close number
                                  :c-access-spec    close
                                  :c-type           flt64
                                  :constraint       d>=0
                                  :compare-function <)
                                 
                                 (:volume number
                                  :c-access-spec    volume
                                  :c-type           flt64
                                  :constraint       d>=0
                                  :compare-function <))
                 
                     :c-type 'stock-detail))
