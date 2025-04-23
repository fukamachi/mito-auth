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

(defun auth (object password)
  (string= (password-hash object)
           (make-password-hash password
                               (password-salt object))))
