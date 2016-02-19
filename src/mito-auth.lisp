(in-package :cl-user)
(defpackage mito-auth
  (:nicknames :mito.auth)
  (:use #:cl)
  (:import-from #:ironclad
                #:byte-array-to-hex-string
                #:digest-sequence
                #:make-random-salt)
  (:import-from #:babel
                #:string-to-octets)
  (:export #:auth
           #:password-hash
           #:password-salt))
(in-package :mito-auth)

(defclass auth ()
  ((password-hash :col-type (:char 64)
                  :initarg :password-hash
                  :reader password-hash)
   (password-salt :col-type (:binary 20)
                  :initarg :password-salt
                  :initform (make-random-salt 20)
                  :reader password-salt))
  (:metaclass mito:dao-table-mixin))

(defun make-password-hash (password salt)
  (ironclad:byte-array-to-hex-string
   (digest-sequence
    :sha256
    (concatenate '(vector (unsigned-byte 8))
                 (babel:string-to-octets password)
                 salt))))

(defgeneric (setf password) (password auth)
  (:method (password (auth auth))
    (let ((password-hash
            (make-password-hash password
                                (slot-value auth 'password-salt))))
      (setf (slot-value auth 'password-hash) password-hash))))

(defmethod initialize-instance :after ((auth auth) &rest initargs
                                       &key password &allow-other-keys)
  (declare (ignore initargs))
  (when password
    (setf (password auth) password)))

(defun auth (object password)
  (string= (password-hash object)
           (make-password-hash password
                               (password-salt object))))
