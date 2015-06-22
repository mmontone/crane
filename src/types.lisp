(in-package :cl-user)

(defpackage crane.types
  (:use :cl)
  (:import-from :cl
                :deftype)
  (:export :int
           :bigint
           :smallint
           :numeric
           :double
           :text
           :varchar
           :timestamp
           :datetime
	   :sql-type-definition
	   :def-sql-type)
  (:documentation "Implements the database types."))

(in-package :crane.types)

(defvar *sql-types* nil)

(defstruct sql-type
  documentation)

(defun struct-args-accessors (struct-name args)
  (loop 
     :for arg :in args
     :collect (intern (format nil "~A-~A" struct-name arg))))

(defmacro def-sql-type (name args &body options)
  `(progn
     (defstruct (,name (:include sql-type))
       ,@args)
     (pushnew ',name *sql-types*)
     ,@(loop 
	  :for option :in options
	  :collect
	  (case (first option)
	    (:documentation nil)
	    (:define
	     (destructuring-bind (_ (type &optional db-type) &body body)
		 option
	       (declare (ignore _))
	       `(defmethod sql-type-definition ((,type ,name) 
						(db-type ,(if db-type
							      `(eql ,db-type)
							      't)))
		  ,@body)))
	    (:read 
	     (destructuring-bind (_ (spec) &body body)
		 option
	       (declare (ignore _))
	       `(defmethod %read-sql-type ((sql-type (eql ',name))
					   ,spec)
		  ,@body)))
	    (t (error "Invalid option: ~A" (first option)))))))

(defgeneric sql-type-definition (sql-type db-type))

(defmethod sql-type-definition (sql-type db-type)
  (error "SQL type ~A is not supported for database type ~A"
	 sql-type db-type))

(defmethod sql-type-definition ((sql-type symbol) db-type)
  (symbol-name sql-type))

(defgeneric %read-sql-type (sql-type spec))

(defmethod %read-sql-type (sql-type spec)
  ;; The type is not read by default
  nil)

(defun read-sql-type (spec)
  (loop 
     :for sql-type :in *sql-types*
     :for type = (%read-sql-type sql-type spec)
     :when type
     :do (return-from read-sql-type type))
  ;; Return the original type spec if it could not be transformed to an sql-type
  spec)

;; SQL-types

(def-sql-type varchar
    (size)
  (:define (type)
      (if (varchar-size type)
	  (format nil "VARCHAR(~A)" (varchar-size type))
	  "VARCHAR"))
  (:read (spec)
	 (cond
	   ((equalp spec 'varchar)
	    (make-varchar))
	   ((and (listp spec)
		 (equalp (first spec) 'varchar))
	    (make-varchar :size (second spec))))))
  

;; Numeric types

(deftype int () `cl:integer)
(deftype bigint () `cl:integer)
(deftype smallint () `cl:integer)
(deftype numeric () `cl:ratio)
(deftype double () `cl:double-float)

;; Text

(deftype text () `cl:string)
;(deftype varchar () `cl:string)

;; Extra

(deftype timestamp () `cl:string)
(deftype datetime () `cl:string)
