(in-package :cl-user)

(defpackage crane.types.postgresql
  (:use :cl)
  (:import-from :crane.types :def-sql-type)
  (:export :timestamp
	   :array)
  (:documentation "Implements Postgresql specific database types."))

(in-package :crane.types.postgresql)

(def-sql-type timestamp
    (timezone)
  (:define (type :postgres)
      (if (timestamp-timezone type)
	  "TIMESTAMP WITH TIMEZONE"
	  "TIMESTAMP"))
  (:read (spec)
	 (cond
	   ((equalp spec 'timestamp)
	    (make-timestamp))
	   ((and (listp spec)
		 (equalp (first spec) 'timestamp))
	    (apply #'make-timestamp (rest spec))))))

(def-sql-type pg-array
    (type size)
  (:define (type :postgres)
      (format nil "~A ARRAY~@[[~A]~]"
	      (or (pg-array-type type)
		  (error "~A type was not specified" type))
	      (pg-array-size type)))
  (:read (spec)
	 (when (and (listp spec)
		    (equalp (first spec) 'array))
	   (destructuring-bind (_ type &optional size) spec
	     (declare (ignore _))
	     (make-pg-array :type type :size size)))))

(defmethod crane.inflate-deflate:inflate (thing (type pg-array))
  (:inflate (thing type)
	    (case (pg-array-type type)
	      (crane.types:int 
	       (funcall 
		(read-array-value #'parse-integer) thing))
	      (crane.types:varchar 
	       (funcall
		(read-array-value #'identity) thing))
	      (t (error "Not supported")))))

(DEFMETHOD CRANE.INFLATE-DEFLATE:DEFLATE ((THING LIST))
   (WITH-OUTPUT-TO-STRING (OUT)
     (WRITE-CHAR #\{ OUT)
     (LOOP :FOR SEP := "" :THEN #\,
           :FOR ELEM IN THING
           :DO (PRINC SEP OUT) (FORMAT OUT "~A" ELEM))
     (WRITE-CHAR #\} OUT)))

;; Copied from postmodern
;; Readers for a few of the array types

(defun read-array-value (transform)
  (lambda (value)
    (declare (type string value))
    (let ((pos 0))
      (declare (type fixnum pos))
      (labels ((readelt ()
                 (case (char value pos)
                   (#\" (interpret
                         (with-output-to-string (out)
                           (loop :with escaped := nil :for ch := (char value (incf pos)) :do
                              (when (and (char= ch #\") (not escaped)) (return))
                              (setf escaped (and (not escaped) (char= ch #\\)))
                              (unless escaped (write-char ch out)))
                           (incf pos))))
                   (#\{ (incf pos)
                        (unless (char= (char value pos) #\})
                          (loop :for val := (readelt) :collect val :into vals :do
                             (let ((next (char value pos)))
                               (incf pos)
                               (ecase next (#\,) (#\} (return vals)))))))
                   (t (let ((start pos))
                        (loop :for ch := (char value pos) :do
                           (when (or (char= ch #\,) (char= ch #\}))
                             (return (interpret (subseq value start pos))))
                           (incf pos))))))
               (interpret (word)
                 (if (string= word "NULL") :null (funcall transform word))))
        (let* ((arr (readelt))
               (dim (if arr (loop :for x := arr :then (car x) :while (consp x) :collect (length x)) '(0))))
          (cl:make-array dim :initial-contents arr))))))
