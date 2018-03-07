;; okeanos.asd
;; --------------------------------------------------------------------------------------
;; Memory-mapped B-Tree Files
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

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

