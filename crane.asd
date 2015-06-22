(defsystem crane
  :version "0.4"
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :homepage "http://eudoxia.me/crane/"
  :depends-on (:closer-mop
               :anaphora
               :sxql
               :dbi
               :iterate
               :cl-fad
               :clos-fixtures
               :uiop
               :local-time)
  :components ((:module "src"
                :serial t
                :components
                ((:file "errors")
                 (:file "config")
                 (:file "util")
                 (:file "connect")
		 (:file "types")
		 (:file "sql")
                 (:file "meta")
                 (:file "query")
                 (:file "migration")
                 (:file "table")                 
                 (:file "inflate-deflate")
		 (:file "types.postgresql")
                 (:file "interface")
                 (:file "fixture")
                 (:file "transaction")
                 (:file "crane"))))
  :description "An ORM for Common Lisp."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (load-op crane-test))))
