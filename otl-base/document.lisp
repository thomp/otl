(in-package :otlb)

(defun document-page-orientation (item-kvs)
  (getf item-kvs :page-orientation))

(defun document-item-kvs-as-values (item-kvs &key title-default)
  "ITEM-KVS corresponds to a document item. Return, as multiple values, author, page-size, and title attributes."
  (declare (special %obj-seq%))
  (values 
   (getf item-kvs :author)
   ;; dates/times: expressed as lisp universal time
   (getf item-kvs :date-last-modified)
   (getf item-kvs :date-created)
   ;; page size: specified as a CLPS keyword
   (getf item-kvs :page-size)
   (or (getf item-kvs :title)
	      title-default))) 