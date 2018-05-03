;;;; notional code to read configuration files and generate
(in-package :emotiq/config)

(eval-when (:load-toplevel :execute)
  (ipath:define-illogical-host
      :emotiq (asdf:system-relative-pathname :emotiq ""))
  (ipath:define-illogical-host
      :var/etc (asdf:system-relative-pathname :emotiq "../var/etc/"))
  (ipath:define-illogical-host
      :config #p(:var/etc ("config"))))

(defparameter *keypair-conf* #P(:config () "keypairs.conf"))
(defparameter *hosts-conf*   #P(:config () "hosts.conf"))
(defparameter *pubkeys-conf* #P(:config () "pubkeys.conf"))
(defparameter *gossip-conf*  #P(:config () "gossip.conf"))

(defun make-key-integers ()
  "Makes a public/private keypair and returns a list of two integers thereof"
  (let ((keypair (pbc:make-key-pair (list :lisp-authority (uuid:make-v1-uuid)))))
    (list
     ;; public is first
     (vec-repr::convert-vec-to-int
      (vec-repr::bev-vec
       (pbc::public-key-val
        (pbc::keying-triple-pkey keypair))))
     ;; private is second
     (vec-repr::convert-vec-to-int
      (vec-repr::bev-vec
       (pbc::secret-key-val
        (pbc::keying-triple-skey keypair)))))))

(defun make-keypairs-conf (n &key (pathname *keypair-conf*))
  "Makes an example keypairs.conf file with N keypairs at PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (s pathname :direction :output :if-exists :supersede)
    (format s ";;; Example keypairs config file~%")
    (format s ";;; Should contain only s-expressions: one 2-list per public/private pair~%")
    (dotimes (i n)
      (format s "~S~%" (make-key-integers))))
  pathname)

;;; (make-keypairs-conf 10)

(defun read-keypairs-conf (&key (pathname *keypair-conf*))
  (when (probe-file pathname)
    (let ((pair nil)
          (pairs nil))
      (with-open-file (s pathname)
        (setf pairs
              (loop while (setf pair (read s nil nil nil)) collect pair)))
      pairs)))

;;; (read-keypairs-conf :pathname *keypair-db-file*)

(defun make-pubkeys-conf (&key
                            (input *keypair-conf*)
                            (output *pubkeys-conf*))
  "Makes an pubkeys.conf OUTPUT from keypairs database INPUT."
  (ensure-directories-exist output)
  (let ((db (read-keypairs-conf input)))
    (when db
      (with-open-file (s output :direction :output :if-exists :supersede)
        (format s ";;; Example pubkeys config file~%")
        (format s ";;; One integer per line~%")
        (dolist (pair db)
          (format s "~D~%" (first pair))))
      output)))

;;; (make-pubkeys-conf)

(defun lookup-private-key (pubkey)
  (let ((db (read-keypairs-conf)))
    (cdr (assoc pubkey db))))

;;; (lookup-private-key 37398122993982683928892421392922194663698236061553046828180042060254159474743680839722135694893904286906573452713293253856697101304531652330060477544645324064368690758628224618216862414461053990953279470172750344093696075445387264727564083265473024399826129282809206956850688)
;;; (lookup-private-key 17) ; --> NIL. No such public key.

(defparameter *whitespace* (list #\space #\tab #\newline #\return #\backspace #\Page))

(defun read-hosts-conf (&key (pathname *hosts-conf*))
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

;;; (read-hosts-conf)

