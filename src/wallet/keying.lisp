(in-package emotiq/wallet)

(defvar *wordlist-folder*  (asdf:system-relative-pathname :emotiq/wallet "wallet/wordlists/"))
(defvar *ws* '(#\space #\tab #\linefeed #\newline #\page))

(defun import-wordlist (filename)
  ;; import a 2048 word list for use in wordlist encoding/decoding
  ;; E.g., (import-wordlist "english.txt")
  (let ((wvec (coerce
               (um:accum acc
                 (with-open-file (f (merge-pathnames
                                     *wordlist-folder*  
                                     filename)
                                    :direction :input)
                   (um:nlet-tail iter ()
                     (let ((wrd (read-line f nil f)))
                       (unless (eql wrd f)
                         (let ((trimmed (string-trim *ws* wrd)))
                           (when (plusp (length trimmed))
                             (acc trimmed)))
                         (iter))))))
               'vector)))
    (unless (= 2048 (length wvec))
      (error "Invalid master word list"))
    wvec))

(defvar *english*  (import-wordlist "english.txt"))

(defun convert-int-to-wordlist (val &optional (wref *english*))
  ;; convert a positive, or zero, 256-bit integer value to a list of
  ;; words representing little-endian encoding in 11-bit groups. The
  ;; integer has another MSB of 8 bits prepended from SHA3/256 of its
  ;; value, to make it a multiple of 11 bits wide, for 24 words in the
  ;; final wordlist.
  ;;
  ;; wref is a vector of 2048 words chosen from some language wordlist.
  ;;
  (assert (= 2048 (length wref)))
  (check-type val (integer 0))
  (assert (<= (integer-length val) 256))
  (let* ((h  (hash:hash/256 (vec-repr:levn val 32)))
         (v  (dpb (aref (vec-repr:lev-vec h) 0) (byte 8 256) val)))
    (loop for ct from 0 below 24
          for pos from 0 by 11
          collect (aref wref (ldb (byte 11 pos) v)))
    ))

(defun convert-wordlist-to-int (wlist &optional (wref *english*))
  ;; convert a list of 24 words from a wordlist into an integer with
  ;; each word representing an 11-bit group presented in little-endian
  ;; order. The result is a 264-bit integer, which is a 256-bit
  ;; integer plus a randomized top 8 bits. The MSB byte (8 bits) must
  ;; match the SHA3/256 of the final 256-bit value.
  ;;
  ;; wref is a vector of 2048 words chosen from some language wordlist.
  ;;
  (assert (= 2048 (length wref)))
  (assert (and (consp wlist)
               (every 'stringp wlist)
               (= 24 (length wlist))))
  (let ((v 0))
    (loop for wrd in wlist
          for pos from 0 by 11
          do
          ;; this will error if word isn't found in list...
          (setf (ldb (byte 11 pos) v) (position wrd wref
                                                :test 'string-equal)))
    (let* ((val (ldb (byte 256 0) v))
           (h   (hash:hash/256 (vec-repr:levn val 32))))
      (unless (= (aref (vec-repr:lev-vec h) 0) (ldb (byte 8 256) v))
        (error "Invalid wordlist"))
      val)))
