;;;; Metaclass to represent classes that are associated to an SQL table
(in-package :crane.meta)

(defclass <table-class> (closer-mop:standard-class)
  ((abstractp :reader abstractp
              :initarg :abstractp
              :initform nil
              :documentation "Whether the class corresponds to an SQL table or not.")
   (deferredp :reader deferredp
              :initarg :deferredp
              :initform nil
              :documentation "Whether the class should be built only when explicitly calling build.")
   (database :reader %table-database
             :initarg :database
             :initform nil
             :documentation "The database this class belongs to."))
  (:documentation "A table metaclass."))

(defmethod table-name ((class <table-class>))
  "Return the name of a the class, a symbol."
  (class-name class))

(defmethod table-database ((class <table-class>))
  "The database this class belongs to."
  (aif (%table-database class)
       it
       crane.connect:*default-db*))

(defclass <table-class-direct-slot-definition>
    (closer-mop:standard-direct-slot-definition)
  ((col-type :reader col-type
             :initarg :col-type)
   (col-null-p :reader col-null-p
               :initarg :col-null-p
               :initform t
               :type boolean)
   (col-unique-p :reader col-unique-p
                 :initarg :col-unique-p
                 :initform nil
                 :type boolean)
   (col-primary-p :reader col-primary-p
                  :initarg :col-primary-p
                  :initform nil
                  :type boolean)
   (col-index-p :reader col-index-p
                :initarg :col-index-p
                :initform nil
                :type boolean)
   (col-foreign  :reader col-foreign
                 :initarg :col-foreign
                 :initform nil)
   (col-autoincrement-p :reader col-autoincrement-p
                        :initarg :col-autoincrement-p
                        :initform nil
                        :type boolean))
  (:documentation "The direct slot definition class of <table-class> slots."))

(defclass <table-class-slot>
    (closer-mop:standard-effective-slot-definition)
  ((col-type :reader col-type
             :initarg :col-type
             :documentation "The type of the column.")
   (col-null-p :reader col-null-p
               :initarg :col-null-p
               :type boolean
               :documentation "Whether the column is nullable.")
   (col-unique-p :reader col-unique-p
                 :initarg :col-unique-p
                 :type boolean
                 :documentation "Whether the column is unique.")
   (col-primary-p :reader col-primary-p
                  :initarg :col-primary-p
                  :type boolean
                  :documentation "Whether the column is a primary key.")
   (col-index-p :reader col-index-p
                :initarg :col-index-p
                :type boolean
                :documentation "Whether the column is an index in the database.")
   (col-foreign :reader col-foreign
                :initarg :col-foreign
                :type (or null <foreign>)
                :documentation "Describes a foreign key relationship.")
   (col-autoincrement-p :reader col-autoincrement-p
                        :initarg :col-autoincrement-p
                        :type boolean
                        :documentation "Whether the column should be autoincremented."))
  (:documentation "A slot of a <table-class>."))

(defmethod closer-mop:compute-effective-slot-definition ((class <table-class>)
                                                         slot-name direct-slot-definitions)
  (declare (ignore slot-name))
  (let ((direct-slot (first direct-slot-definitions))
        (effective-slot-definition (call-next-method)))
    (setf (slot-value effective-slot-definition 'col-type)
          (col-type direct-slot)

          (slot-value effective-slot-definition 'col-null-p)
          (col-null-p direct-slot)

          (slot-value effective-slot-definition 'col-unique-p)
          (col-unique-p direct-slot)

          (slot-value effective-slot-definition 'col-primary-p)
          (if (and (eq (database-type (get-db (table-database class)))
                       :sqlite3)
                   (eq (col-autoincrement-p direct-slot)
                       t))
              nil
              (col-primary-p (first direct-slot-definitions)))

          (slot-value effective-slot-definition 'col-index-p)
          (col-index-p direct-slot)

          (slot-value effective-slot-definition 'col-foreign)
          (aif (col-foreign direct-slot)
               (apply #'make-foreign it)
               nil)

          (slot-value effective-slot-definition 'col-autoincrement-p)
          (col-autoincrement-p direct-slot))
    effective-slot-definition))

;;; Assorted MOPery

(defmethod closer-mop:validate-superclass ((class <table-class>)
                                           (super closer-mop:standard-class))
  "Validate that a table class can be a subclass of a standard-class."
  t)

(defmethod closer-mop:validate-superclass ((class standard-class)
                                           (super <table-class>))
  "Validate that a standard-class can be the subclass of a table-class."
  t)

(defmethod closer-mop:direct-slot-definition-class ((class <table-class>)
                                                    &rest initargs)
  (declare (ignore class initargs))
  (find-class '<table-class-direct-slot-definition>))

(defmethod closer-mop:effective-slot-definition-class ((class <table-class>)
                                                       &rest initargs)
  (declare (ignore class initargs))
  (find-class '<table-class-slot>))
