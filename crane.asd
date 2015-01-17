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
               :yason)
  :components ((:module "src"
                :serial t
                :components
                ((:file "errors")
                 (:file "config")
                 (:file "util")
                 (:file "types")
                 (:file "connect")
                 (:module "meta"
                  :serial t
                  :components
                  ((:file "package")
                   (:file "foreign")
                   (:file "mop")
                   (:file "json")
                   (:file "diff"))))))
  :description "An ORM for Common Lisp."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (load-op crane-test))))
