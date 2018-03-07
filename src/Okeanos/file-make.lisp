;; file-make.lisp
;; --------------------------------------------------------------------------------------
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:com.sd.okeanos.int)
;; -------------------------------------------

;; --------------------------------------------

(defun ensure-directory-name (dirname)
  (if (pathname-name dirname)
      (mkstr dirname #\/)
    dirname))

;; -------------------------------------------

(defun make-db-filename (file-name directory-name)
  (merge-pathnames (make-pathname
                    :directory (pathname-directory directory-name)
                    :name      (pathname-name file-name)
                    :type      (pathname-type file-name))
                   #+:LISPWORKS
                   (hcl:get-working-directory)
                   #+:ALLEGRO
                   (excl:current-directory)))

;; -------------------------------------------

(defun with-new-db-file (file-name directory-name fn)
  (bind*
      ((fname (make-db-filename file-name directory-name)))
    
    (ensure-directories-exist fname)
    (with-open-file (f fname
                       :direction :io
                       :if-exists :rename
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
      (mmf:with-mmapper (mmptr f)
        (hcl:unwind-protect-blocking-interrupts-in-cleanups
            (funcall fn mmptr)
          (mmf:close-mmapper mmptr))))
    ))

#+:LISPWORKS
(editor:setup-indent "with-new-db-file" 2 2)

;; -------------------------------------------

(defun write-magic-header-slots (mm magic)
  (bind*
      ((mmptr (make-magic-file-header mm))
       (:struct-accessors magic-file-header
        (version-major
         version-minor
         byte-order-s
         hdr-size) mmptr))

    (setf (mmf:fetch-array (mmf:foreign-slot-pointer mmptr 'magic)) magic
          version-major $version-major
          version-minor $version-minor
          byte-order-s  1
          hdr-size      (align-pwr2 (mmf:size-of 'magic-file-header_t)
                                    (* 2 (mmf:size-of 'memory-block-header_t)))
          )))

;; -------------------------------------------

(defun make-data-file (filename dirname use-heap)
  (with-new-db-file filename dirname
    #'(lambda (mmptr)
      (bind*
          ((mapper (make-data-file-header dirname mmptr))
           (:struct-accessors data-file-header
            (next-avail
             hdr-size
             free-list) mapper)
           (hdrsize (align-pwr2 (mmf:size-of 'data-file-header_t)
                                (* 2 (mmf:size-of 'memory-block-header_t)))))
        
        (write-magic-header-slots mapper "OKNO")
        (setf next-avail    hdrsize
              free-list     0
              hdr-size      hdrsize)
        (get-tls) ;; make initial version of TLS
        (let* ((*current-okeanos-db*
                (make-instance 'file-database
                 :dirname  dirname
                 :mapper   mapper))
               (db-uuid (make-uuid))
               (*current-connection*
                (make-cnx-info
                 :db-state db-uuid)))

          (with-write-lock ()
            (create-new-string-pool dirname (string-pool-pointer mapper))
            
            (ensure-oid-pointers-schema)
            (ensure-oid-index-entry-schema)
            (ensure-oid-index-collection-schema)
            (ensure-trans-dir-schema)
            (ensure-user-mappings-schema)
            
            (create-new-file-table +oid-mappings-path+
                                   +oid-pointers-schema-path+
                                   :use-heap use-heap)
            (create-new-file-table +transaction-directory-path+
                                 +transaction-directory-schema-path+
                                 :use-heap 1000)
            (create-new-file-table +user-mappings-path+
                                   +user-mappings-schema-path+
                                   :use-heap 100)
            (create-new-log-file)
            (create-folder-item +db-uuid-path+ db-uuid)

            (get-user-oid)
            (commit) ;; also sets the last-ts in header
            )))
      )))

;; -------------------------------------------------------

(defun db-path (name &optional dir)
  (merge-pathnames name
                   (ensure-directory-name
                    (or dir
                        (database-dirname *current-okeanos-db*)))))

(defun create-folder-item (name val &key
                                directory
                                (if-exists :supersede)
                                (if-does-not-exist :create))
  (let ((fname (db-path name directory)))
    (with-write-lock ()
      (ensure-directories-exist fname :verbose t)
      (with-open-file (f fname
                         :direction :output
                         :if-exists if-exists
                         :if-does-not-exist if-does-not-exist
                         :element-type '(unsigned-byte 8))
        (loenc:serialize val f))
      val)))

(defun retrieve-folder-item (name &optional dir)
  (let ((fname (db-path name dir)))
    (with-read-lock ()
      (with-open-file (f fname
                         :direction :input
                         :element-type '(unsigned-byte 8))
        (loenc:deserialize f))
      )))

(defun delete-folder-item (name &optional dir)
  (let ((fname (db-path name dir)))
    (with-write-lock ()
      (delete-file fname)
      )))

;; -------------------------------------------

;; -------------------------------------------

(defun make-okeanos-file (dirname use-heap)
  (bind*
      ((directory-name (ensure-directory-name dirname)))
    (make-data-file +data-file-name+ directory-name use-heap)))
  
