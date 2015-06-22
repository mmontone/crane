(in-package :crane-test.types)

(def-suite types-tests)
(in-suite types-tests)

(test read-type
  (is (equal
       (crane.types::read-sql-type 'varchar)
       'varchar))
  (is (equalp
       (crane.types::read-sql-type 'crane.types::varchar)
       #S(CRANE.TYPES:VARCHAR :DOCUMENTATION NIL :SIZE NIL)))
  (is (equalp
       (crane.types::read-sql-type '(crane.types::varchar 80))
       #S(CRANE.TYPES:VARCHAR :DOCUMENTATION NIL :SIZE 80)))
  (is (equal
       (crane.types::read-sql-type 'crane.types::integer)
       'crane.types::integer)))

(test define-type
  (is (equal
       (crane.types::sql-type-definition 'varchar t)
       "VARCHAR"))
  (is (equal
       (crane.types::sql-type-definition 'crane.types::varchar t)
       "VARCHAR"))
  (is (equal
       (crane.types::sql-type-definition 
	(crane.types::read-sql-type 'crane.types::varchar)
	t)
       "VARCHAR"))
  (is (equal
       (crane.types::sql-type-definition 
	(crane.types::read-sql-type '(crane.types::varchar 80))
	t)
       "VARCHAR(80)")))
