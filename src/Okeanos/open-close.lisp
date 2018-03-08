;; open-close.lisp
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
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

(defmethod close-okeanos-file-database ((db file-database))
  (bind*
      ((:struct-accessors database
        (mapper) db))
      
      (when mapper
        (mmf:close-mmapper mapper)
        (setf mapper nil))
      ))

(defmethod close-okeanos-file-database (db)
  ;; sometimes Lisp exit passes us non-data
  (declare (ignore db)))

;; --------------------------------------------------------------

(defun explode-string (str)
  (loop for c across str collect c))

(defun open-db-file (file-name magic)
  (let* ((mmptr (mmf:cast (mmf:make-mmapper file-name)
                          'magic-file-header_t :address 0))
         (file-magic (mmf:fetch-array (mmf:foreign-slot-pointer mmptr 'magic)))
         (emagic     (explode-string magic)))
    
    ;; Read the header and check the magic cookie to be sure we are a DAM-File
    (unless (every 'eql emagic file-magic)
      (mmf:close-mmapper mmptr)
      (error "Magic mismatch. Have ~S, expected ~S." file-magic emagic))
    (unless (= 1 (mmf:fetch-slot mmptr '(byte-order s)))
      (mmf:flip-byte-order mmptr))
    mmptr))
    
;; --------------------------------------------------------------

(defun open-db-returning-database (directory-name data-name use-heap uuid force)
  (let ((mapper   nil)
        (database nil)
        (cnxinfo  nil))
                     
    ;; do it this way to protect against process-kill
    (hcl:unwind-protect-blocking-interrupts-in-cleanups
        (progn
          (unless (probe-file data-name)
            (make-okeanos-file directory-name use-heap))
          
          (setf mapper (open-db-file data-name "OKNO"))

          ;; construct the database structure
          (let ((df (make-data-file-header directory-name mapper)))
            (get-tls) ;; initial version
            ;; find session OID offset
            (setf database
                  (make-instance 'file-database
                   :dirname  directory-name
                   :mapper   df
                   :uuid     uuid)

                  cnxinfo (make-cnx-info))
            
            (let ((*current-okeanos-db* database)
                  (*current-connection* cnxinfo))

              (unless force
                (quick-validate))
              
              (with-write-lock ()
                (let ((last-ts (uuid:uuid-to-integer (get-last-ts)))
                      (next-ts (uuid:uuid-to-integer (get-new-host-ts))))
                  (if (< next-ts last-ts)
                      ;; incf to permit multiple open databases
                      (incf *ts-offset* (+ 10000 (- last-ts next-ts))) ;; advance by 1ms
                    ))
                
                ;; return the logfiles vector
                (setf (database-files-list database)
                      (read-oid-file-vector))
                ))))
      
      ;; unwind clause
      (unless database
        (when mapper
          (mmf:close-mmapper mapper))
        ))
    database))

;; --------------------------------------------------------------

(defun open-okeanos-file-database (dirname use-heap uuid force)
  (bind*
      ((directory-name (ensure-directory-name dirname))
       (data-name      (make-db-filename +data-file-name+ directory-name)))
    (open-db-returning-database directory-name data-name use-heap uuid force)))

;; --------------------------------------------------------------
