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
