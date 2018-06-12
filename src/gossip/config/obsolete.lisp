; notional code to read configuration files and generate
;   representative examples of them
; (ql:quickload :illogical-pathnames)
; (ipath:define-illogical-host :CONFIG #P(:EMOTIQ ("emotiq" "src" "config")))

;;; THIS FILE IS OBSOLETE. DO NOT USE IN PRODUCTION.
;;; But code herein may need to be lifted and used elsewhere.

(defparameter *keypair-db-file* #P(:config () "keypairs.conf"))
(defparameter *hosts-db-file*   #P(:config () "hosts.conf"))
(defparameter *pubkeys-db-file* #P(:config () "pubkeys.conf"))
(defparameter *gossip-db-file*  #P(:config () "gossip.conf"))

(defun make-key-integers ()
  "Makes a public/private keypair and returns a list of two integers thereof"
  (let ((keypair (pbc:make-key-pair (list :lisp-authority (uuid:make-v1-uuid)))))
    (list
     ; public is first
     (vec-repr::convert-vec-to-int
      (vec-repr::bev-vec
       (pbc::public-key-val
        (pbc::keying-triple-pkey keypair))))
     ; private is second
     (vec-repr::convert-vec-to-int
      (vec-repr::bev-vec
       (pbc::secret-key-val
        (pbc::keying-triple-skey keypair)))))))

(defun make-example-keypairs-file (n pathname)
  "Makes an example keypairs.conf file with n keypairs"
  (with-open-file (s pathname :direction :output :if-exists :supersede)
    (format s ";;; Example keypairs config file~%")
    (format s ";;; Should contain only s-expressions: one 2-list per public/private pair~%")
    (dotimes (i n)
      (format s "~S~%" (make-key-integers)))))

; (make-example-keypairs-file 10 #P(:config () "keypairs.conf"))

(defun read-keypairs-database (pathname)
  (when (probe-file pathname)
    (let ((pair nil)
          (pairs nil))
      (with-open-file (s pathname)
        (setf pairs
              (loop while (setf pair (read s nil nil nil)) collect pair)))
      pairs)))

; (read-keypairs-database *keypair-db-file*)

(defun make-example-pubkeys-file (pathname)
  "Makes an example pubkeys.conf file from keypairs database"
  (let ((db (read-keypairs-database *keypair-db-file*)))
    (when db
      (with-open-file (s pathname :direction :output :if-exists :supersede)
        (format s ";;; Example pubkeys config file~%")
        (format s ";;; One integer per line~%")
        (dolist (pair db)
          (format s "~D~%" (first pair)))
        )))

; (make-example-pubkeys-file *pubkeys-db-file*)

(defun lookup-private-key (pubkey)
  (let ((db (read-keypairs-database *keypair-db-file*)))
    (cdr (assoc pubkey db))))

; (lookup-private-key 37398122993982683928892421392922194663698236061553046828180042060254159474743680839722135694893904286906573452713293253856697101304531652330060477544645324064368690758628224618216862414461053990953279470172750344093696075445387264727564083265473024399826129282809206956850688)
; (lookup-private-key 17) ; --> NIL. No such public key.

(defparameter *whitespace* (list #\space #\tab #\newline #\return #\backspace #\Page))

; obsolete. Moved to gossip-startup.lisp
(defun read-hosts-database (pathname)
  "Returns a list of strings, one per host address or domain name"
  (when (probe-file pathname)
    (let ((host nil)
          (hosts nil))
      (with-open-file (s pathname)
        (setf hosts
              (loop while (setf host (read-line s nil nil nil)) collect host)))
      (setf hosts (mapcar (lambda (host)
                            (string-trim *whitespace* host))
                          hosts))
      (when hosts
        (setf hosts
              (remove-if (lambda (host)
                           (or (zerop (length host))
                               (eql 0 (position #\; host))))
                         hosts))
        hosts))))

; (read-hosts-database *hosts-db-file*)