;;;; Computing differences between slots for migrations
(in-package :crane.meta)

(defmethod digest-slot ((slot table-class-slot))
  (list :name (closer-mop:slot-definition-name slot)
        :type (col-type slot)
        :nullp (col-null-p slot)
        :uniquep (col-unique-p slot)
        :primaryp (col-primary-p slot)
        :indexp (col-index-p slot)
        :autoincrementp (col-autoincrement-p slot)
        :foreign (col-foreign slot)))

(defmethod digest ((class table-class))
  "Serialize a class's options and slots' options into a plist"
  (list :table-options
        (list :database (table-database class))
        :columns
        (let ((slots (closer-mop:class-slots class)))
          (if slots
              (mapcar #'digest-slot
                      (closer-mop:class-slots class))
              (error 'crane.errors:empty-table
                     :text "The table ~A has no slots."
                     (table-name class))))))

(defmethod diff-slot ((slot-a table-class-slot) (slot-b table-class-slot))
  "Compute the difference between two slot digests.
See DIGEST."
  (append (list :name (getf slot-a :name) :diff)
          (list
           (crane.util:diff-plist slot-a slot-b :test #'equal))))

(defun sort-slot-list (list)
  list)

(defun diff-digest (digest-a digest-b)
  "Compute the difference between two digests.
See DIGEST."
  (flet ((find-slot-definition (slot-name digest)
           (iter (for slot in (getf digest :columns))
             (if (eql slot-name (getf slot :name))
                 (return slot)))))
    (let* ((slot-names-a
             (iter (for slot in (getf digest-a :columns))
               (collecting (getf slot :name))))
           (slot-names-b
             (iter (for slot in (getf digest-b :columns))
               (collecting (getf slot :name))))
           (changes
             (intersection slot-names-a slot-names-b))
           (additions
             (set-difference slot-names-b slot-names-a))
           (deletions
             (set-difference slot-names-a slot-names-b))
           (changes-a
             (iter (for slot-name in changes)
               (collecting (find-slot-definition slot-name digest-a))))
           (changes-b
             (iter (for slot-name in changes)
               (collecting (find-slot-definition slot-name digest-b)))))
      (list :additions (mapcar #'(lambda (slot-name)
                                   (find-slot-definition slot-name digest-b))
                               (remove-if #'null additions))
            :deletions (remove-if #'null deletions)
            :changes
            (remove-if-not #'(lambda (slot) (getf slot :diff))
                           (remove-if #'null
                                      (mapcar #'diff-slot
                                              (sort-slot-list changes-a)
                                              (sort-slot-list changes-b))))))))
