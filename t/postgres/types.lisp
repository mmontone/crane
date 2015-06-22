(in-package :crane-test.postgres)

(in-suite postgres)

(test postgresql-timestamp
  (let ((type 
	 (crane.types::read-sql-type 
	  'crane.types.postgresql:timestamp)))
    (signals error
      (crane.types::sql-type-definition type t))
    (signals error
      (crane.types::sql-type-definition type :sqlite))
    (is (equalp (crane.types::sql-type-definition type :postgres)
		"TIMESTAMP")))
  (let ((type 
	 (crane.types::read-sql-type 
	  '(crane.types.postgresql:timestamp :timezone t))))
    (signals error
      (crane.types::sql-type-definition type t))
    (signals error
      (crane.types::sql-type-definition type :sqlite))
    (is (equalp (crane.types::sql-type-definition type :postgres)
		"TIMESTAMP WITH TIMEZONE"))))

(test postgresql-array
  (let ((type 
	 (crane.types::read-sql-type 
	  '(crane.types.postgresql:array crane.types::string))))
    (signals error
      (crane.types::sql-type-definition type t))
    (signals error
      (crane.types::sql-type-definition type :sqlite))
    (is (equalp (crane.types::sql-type-definition type :postgres)
		"STRING ARRAY")))
  (let ((type 
	 (crane.types::read-sql-type 
	  '(crane.types.postgresql:array crane.types::integer))))
    (signals error
      (crane.types::sql-type-definition type t))
    (signals error
      (crane.types::sql-type-definition type :sqlite))
    (is (equalp (crane.types::sql-type-definition type :postgres)
		"INTEGER ARRAY")))

  (let ((type 
	 (crane.types::read-sql-type 
	  '(crane.types.postgresql:array crane.types::integer 10))))
    (signals error
      (crane.types::sql-type-definition type t))
    (signals error
      (crane.types::sql-type-definition type :sqlite))
    (is (equalp (crane.types::sql-type-definition type :postgres)
		"INTEGER ARRAY[10]"))))
