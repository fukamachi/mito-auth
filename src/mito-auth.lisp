(in-package :cl-user)
(defpackage mito-auth
  (:nicknames :mito.auth)
  (:use #:cl)
  (:import-from #:ironclad
                #:byte-array-to-hex-string
                #:digest-sequence
                #:hex-string-to-byte-array
                #:*prng*
                #:make-prng
                #:make-random-salt)
  (:import-from #:babel
                #:string-to-octets)
  (:export #:has-secure-password
           #:auth
           #:password
           #:password-hash
           #:password-salt))
(in-package :mito-auth)

;; from cl-str
(defvar *whitespaces* (list #\Backspace #\Tab #\Linefeed #\Newline #\Vt #\Page
                            #\Return #\Space #\Rubout
                            #+sbcl #\Next-Line #-sbcl (code-char 133)
                            #+(or abcl gcl lispworks ccl) (code-char 12288) #-(or abcl gcl lispworks ccl) #\Ideographic_space
                            #+lispworks #\no-break-space #-lispworks #\No-break_space)
  "On some implementations, linefeed and newline represent the same character (code).")

(defun trim (s &key (char-bag *whitespaces*))
  "Removes all characters in `char-bag` (default: whitespaces) at the beginning and end of `s`.
   If supplied, char-bag has to be a sequence (e.g. string or list of characters).

   Examples: (trim \"  foo \") => \"foo\""
  (when s
    (string-trim char-bag s)))

(defclass has-secure-password ()
  ((password-hash :col-type (:char 64)
                  :initarg :password-hash
                  :reader password-hash)
   (password-salt :col-type (:char 64)
                  :initarg :password-salt
                  :initform
                  ;; Use /dev/urandom seed for portability.
                  (let ((*prng* (make-prng :fortuna :seed :urandom)))
                    (make-random-salt 20))
                  :reader password-salt))
  (:metaclass mito:dao-table-mixin))

(defun make-password-hash (password salt)
  (byte-array-to-hex-string
   (digest-sequence
    :sha256
    (concatenate '(vector (unsigned-byte 8))
                 (babel:string-to-octets password)
                 (if (stringp salt)
                     (hex-string-to-byte-array salt)
                     salt)))))

(defgeneric (setf password) (password auth)
  (:method (password (object has-secure-password))
    (let ((salt-bytes (make-random-salt 20)))
      (setf (slot-value object 'password-salt) (byte-array-to-hex-string salt-bytes))
      (setf (slot-value object 'password-hash) (make-password-hash password salt-bytes)))))

(defmethod initialize-instance :after ((object has-secure-password) &rest initargs
                                       &key password &allow-other-keys)
  (declare (ignore initargs))
  (when password
    (setf (password object) password)))

(defun normalize-password-salt (password-salt)
  (let ((trimmed-password-salt (trim password-salt)))
    (if (uiop:string-prefix-p "\\x" trimmed-password-salt)
        (subseq trimmed-password-salt 2)
        trimmed-password-salt)))

(defun auth (object password)
  (string= (password-hash object)
           (make-password-hash password
                               (normalize-password-salt (password-salt object)))))
