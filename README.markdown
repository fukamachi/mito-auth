# mito-auth

Mito-auth provides a Mito mixin class for user authorization.

## Usage

```common-lisp
(use-package :mito-auth)

;; Inherit mito-auth:has-secure-password.
;; It adds password_hash and password_salt.
(defclass user (has-secure-password)
  ((name :col-type (:varchar 60)
         :initarg :name
         :accessor user-name)
   (email :col-type (:varchar 255)
          :initarg :email
          :accessor user-email))
  (:metaclass mito:dao-table-class))

;; Connect to the DB
(mito:connect-toplevel :sqlite3 :database-name #P"/tmp/mito-auth.db")

;; Enable logging
(setf mito:*mito-logger-stream* t)

;; Ensure the table "user" exists
(mito:ensure-table-exists 'user)
;-> ;; CREATE TABLE IF NOT EXISTS "user" (
;       "id" INTEGER PRIMARY KEY AUTOINCREMENT,
;       "name" VARCHAR(60) NOT NULL,
;       "email" VARCHAR(255) NOT NULL,
;       "password_hash" CHAR(64) NOT NULL,
;       "password_salt" BYTEA NOT NULL,
;       "created_at" TIMESTAMP,
;       "updated_at" TIMESTAMP
;   ) () [0 rows] | MITO.DAO:ENSURE-TABLE-EXISTS

(mito:create-dao 'user
                 :name "Eitaro Fukamachi"
                 :email "e.arrows@gmail.com"
                 :password "c0mmon-l1sp")
;-> ;; INSERT INTO "user" ("name", "email", "password_hash", "password_salt", "created_at", "updated_at") VALUES (?, ?, ?, ?, ?, ?) ("Eitaro Fukamachi", "e.arrows@gmail.com", "63ab35de18dffd24e51a98d5f9c4cd82e665053a73efbe7e006dc37e922d7949", "LºÅLm&[FÂÍüîn24¾", "2016-02-19 17:47:10", "2016-02-19 17:47:10") [0 rows] | MITO.DAO:INSERT-DAO
;=> #<USER {100461A363}>

(defvar *user* (mito:find-dao 'user :email "e.arrows@gmail.com"))

(auth *user* "c0mmon-l1sp")
;=> T

(auth *user* "wrong-password")
;=> NIL

;; To change the password do
(setf (password *user*) “new-password”)
;; Then on `mito:save-dao` it will take care of the hashing.
(mito:save-dao *user*)
```

## Installation

```common-lisp
(ql:quickload :mito-auth)
```

## See Also

* [Mito](https://github.com/fukamachi/mito)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
