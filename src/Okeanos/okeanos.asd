;; okeanos.asd
;; --------------------------------------------------------------------------------------
;; Memory-mapped B-Tree Files
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

(asdf:defsystem "okeanos"
  :description "Okeanos: a Lisp implementation of Memory-Mapped B-Trees"
  :version     "1.0"
  :author      "D.McClain <dbm@spectrodynamics.com>"
  :license     "Copyright (c) 2008 by SpectroDynamics, LLC. All rights reserved."
  :components  ((:file "security-check")
                (:file "packages")
                (:file "globals")
                (:file "constants")
                (:file "ctypes")
                (:file "oid")
                (:file "structs-bb")
                (:file "alloc")
                (:file "uuid")
                (:file "heaps")
                (:file "varheaps")
                ;; (:file "file-btrees")
                (:file "file-btrees-clos")
                (:file "string-pool")
                
                (:file "lock-management")
                (:file "filename-cleansing")
                (:file "schema")
                (:file "tables")
                (:file "oid-manager")
                (:file "persist")
                (:file "commit")
                (:file "rollback")
                (:file "transactions")
                (:file "file-make")
                (:file "validation")
                (:file "open-close")

                (:file "main-db")
                (:file "internal-schema")
                (:file "persistent-class")
                (:file "persistent-object")
                (:file "find-map-classes")
                ;; (:file "queries")
                (:file "queries-optima")
                (:file "ok-sets")
                (:file "ok-maps")
                (:file "ok-tables")
                (:file "serialization")
                (:file "logfiles")
                (:file "remote-access-bb")
                
                (:file "transaction-viewer")
                
                (:file "stocks-test")
                (:file "startup")
                #|
                ;;(:file "compact")
                ;;(:file "portable-objects")
                ;;(:file "locks")
                |#
                )

  :SERIAL T
  :depends-on   ("useful-macros"
                 "lisp-object-encoder"
                 "ironclad"
                 "data-objects"
                 #-:ALLEGRO "plotter"
                 "butterfly"
                 "optima"
                 "mmapper"
                 "pltstocks" ;; while testing
                 ))

