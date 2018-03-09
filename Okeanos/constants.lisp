;; constants.lisp
;; --------------------------------------------------------------------------------------
;;
;; DM/SD  08/08
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
(in-package #:com.sd.okeanos.int)
;; --------------------------------------------
(defconstant $version-major 1)
(defconstant $version-minor 1)
;; -------------------------------------------
(defconstant +page-size+    4096)
;; -------------------------------------------
;; Memory segment identifiers

(defconstant +free-block+            #/uuid/{719B6D9C-F947-4E9F-A137-8BED9A88B9BF})
(defconstant +tree-header+           #/uuid/{F5EB7CDF-6B9E-40B0-988C-0359E06EE0E2})
(defconstant +tree-node+             #/uuid/{B3CCF828-2FF9-4184-9173-947CB6EA170A})
(defconstant +fixed-heap-header+     #/uuid/{10CAA099-5DD7-4EB5-9AE8-02AF3E7A4CC5})
(defconstant +fixed-heap-block+      #/uuid/{C59D1FF7-5888-41F8-B865-68DCC327E374})
(defconstant +small-var-heap-header+ #/uuid/{6220F98F-8882-4DB1-9A2B-048951057773})
(defconstant +small-var-heap-block+  #/uuid/{61860F26-B475-4EC1-A508-F845C1EC14F1})
(defconstant +string-pool-header+    #/uuid/{A44AD614-5BAA-4B9D-B6E9-D68767042BFC})
;; (defconstant +file-uuid+             #/uuid/{00A0F0BA-5B83-4AA4-AAF7-A1C269DAD307})
(defconstant +long-string+           #/uuid/{C578F924-BB6F-4EB8-8E06-986F83156A36})
;; (defconstant +schema-entry+          #/uuid/{82F916E9-1932-40D9-B9B9-66FA5081563E})
(defconstant +file-table+            #/uuid/{914685BE-14D9-4383-B93D-E55CDE5C7307})
(defconstant +data-item-uuid+        #/uuid/{A186B055-F767-4D1B-93F7-CDE9BF9B941E})

;; ----------------------------------------------------------------

(defconstant +db-uuid-path+                      "Sys/Info/File-UUID")
(defconstant +files-list-path+                   "Sys/Info/Files-List")
(defconstant +oid-mappings-path+                 "Sys/Tables/OID-Mappings")
(defconstant +oid-pointers-schema-path+          "Sys/Schema/OID-Pointers")
(defconstant +oid-index-schema-path+             "Sys/Schema/OID-Index")
(defconstant +oid-index-collection-schema-path+  "Sys/Schema/OID-Index-Collection")
(defconstant +class-table-path+                  "Sys/Tables/Classes")
(defconstant +transaction-directory-path+        "Sys/Tables/Transactions")
(defconstant +transaction-directory-schema-path+ "Sys/Schema/Transaction-Entry")
(defconstant +user-mappings-path+                "Sys/Tables/Users")
(defconstant +user-mappings-schema-path+         "Sys/Schema/Users-Index")


(defconstant +data-file-name+  "data.dam")
(defconstant +absent+ #(absent))


