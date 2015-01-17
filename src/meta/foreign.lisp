;;;; A class to represent foreign key actions
(in-package :crane.meta)

(defparameter +referential-actions+
  (list :cascade "CASCADE"
        :restrict "RESTRICT"
        :no-action "NO ACTION"
        :set-null "SET NULL"
        :set-default "SET DEFAULT")
  "A map of allowed referential actions to their SQL names.")

(defun referential-action-name (action)
  "Find the SQL string of a referential action or signal a condition."
  (aif (getf +referential-actions+ action)
       it
       (error "No such referential action: ~A" action)))

(defclass <foreign> ()
  ((foreign-table :reader foreign-table
                  :initarg :foreign-table
                  :type symbol
                  :documentation "The name of the table referenced by this relation.")
   (on-delete-action :reader on-delete-action
                     :initarg :on-delete-action
                     :initform :no-action
                     :type keyword
                     :documentation "The action to take on deletion.")
   (on-update-action :reader on-update-action
                     :initarg :on-update-action
                     :initform :no-action
                     :type :keyword
                     :documentation "The action to take on updates."))
  (:documentation "A foreign key relationship."))

(defmethod initialize-instance :after ((foreign <foreign>) &key)
  "Verify that the referential actions are allowed."
  (referential-action-name (on-delete-action foreign))
  (referential-action-name (on-update-action foreign)))

(defun make-foreign (foreign-table-name &key (on-delete :no-action)
                                          (on-update :no-action))
  (make-instance '<foreign>
                 :foreign-table foreign-table-name
                 :on-delete-action on-delete
                 :on-update-action on-update))
