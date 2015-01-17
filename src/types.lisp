(in-package :cl-user)
(defpackage crane.types
  (:use :cl)
  (:export :sql-type
           :int
           :bigint
           :smallint
           :numeric
           :double
           :text
           :varchar
           :timestamp
           :datetime
           :sql-type-name)
  (:documentation "Implements the database types."))
(in-package :crane.types)

(defclass sql-type ()
  ()
  (:documentation "The base class of all SQL types."))

(defgeneric sql-type-name (type)
  (:documentation "Return the SQL name of a type."))

;;; Types

;; Numeric types

(defclass int ()
  ()
  (:documentation "An integer"))

(defclass bigint ()
  ()
  (:documentation "A big integer."))

(defclass smallint ()
  ()
  (:documentation "A small integer."))

(defclass numeric ()
  ()
  (:documentation "A number."))

(defclass double ()
  ()
  (:documentation "A double-precision floating-point number."))

;; Text

(defclass text ()
  ()
  (:documentation "A piece of text."))

(defclass varchar ()
  ()
  (:documentation "A variable-length string."))

;; Extra

(defclass timestamp ()
  ()
  (:documentation "A timestamp."))

(defclass datetime ()
  ()
  (:documentation "A date/time value."))

;;; Type names

(macrolet ((type-name (class name)
             `(defmethod sql-type-name ((type ,class))
                ,(format nil "Type name of the ~A type." (string-downcase class))
                (declare (ignore type))
                ,name)))
  (type-name int "INTEGER")
  (type-name bigint "BIGINT")
  (type-name smallint "SMALLINT")
  (type-name numeric "NUMERIC")
  (type-name double "DOUBLE")
  (type-name text "TEXT")
  (type-name varchar "VARCHAR")
  (type-name timestamp "TIMESTAMP")
  (type-name datetime "DATETIME"))
