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
           :datetime)
  (:documentation "Implements the database types."))
(in-package :crane.types)

(defclass sql-type ()
  ()
  (:documentation "The base class of all SQL types."))

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
