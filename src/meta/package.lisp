(in-package :cl-user)
(defpackage crane.meta
  (:use :cl :anaphora :iter)
  (:import-from :crane.connect
                :database-type
                :get-db)
  (:export :table-class
           :table-class-slot
           :foreign-key
           :table-name
           :abstractp
           :deferredp
           :table-database
           :col-type
           :col-null-p
           :col-unique-p
           :col-primary-p
           :col-index-p
           :col-foreign
           :col-autoincrement-p
           :foreign-table
           :up-delete-action
           :on-update-action
           :digest
           :diff-digest)
  (:documentation "This package defines the metaclasses that map CLOS objects to
  SQL tables, and some basic operations on them."))
