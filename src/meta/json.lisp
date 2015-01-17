;;;; JSON serialization/deserialization of classes and class slots
(in-package :crane.meta)

(defmethod yason:encode ((slot <table-class-slot>) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "type" (col-type slot))
      (yason:encode-object-element "nullp" (col-null-p slot))
      (yason:encode-object-element "uniquep" (col-unique-p slot))
      (yason:encode-object-element "primaryp" (col-primary-p slot))
      (yason:encode-object-element "indexp" (col-index-p slot))
      (yason:encode-object-element "foreign" (col-foreign slot))
      (yason:encode-object-element "autoincrementp" (col-autoincrement-p slot)))))
