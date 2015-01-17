;;;; JSON serialization/deserialization of classes and class slots
(in-package :crane.meta)

(defun serialize-symbol (symbol)
  "Return a serializable representation from a symbol."
  (let* ((package (symbol-package symbol))
         (package-externals (loop for sym being the external-symbols of package
                                  collecting sym))
         (externalp (if (member symbol package-externals)
                        t))
         (package-name (package-name package))
         (symbol-name (symbol-name symbol)))
    (concatenate 'string
                 package-name
                 (if externalp ":" "::")
                 symbol-name)))

(defun find-serialied-symbol (string)
  "Load a symbol from the string created by serialize-symbol."
  (intern string))

(defmethod yason:encode ((slot table-class-slot) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "type" (col-type slot))
      (yason:encode-object-element "nullp" (col-null-p slot))
      (yason:encode-object-element "uniquep" (col-unique-p slot))
      (yason:encode-object-element "primaryp" (col-primary-p slot))
      (yason:encode-object-element "indexp" (col-index-p slot))
      (yason:encode-object-element "foreign" (col-foreign slot))
      (yason:encode-object-element "autoincrementp" (col-autoincrement-p slot)))))
