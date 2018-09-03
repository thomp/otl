(in-package :otlr)
;;;
;;; render-list.lisp
;;;

;; OBJECT-SEQUENCE is a sequence of list items
(defun list-x (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  (let* ((first-row (first object-sequence))
	 ;; ITEM-SPEC example: (:LIST NIL 0 "[0-9]+\\.")
	 (item-spec (otlb::get-item-spec first-row))
	 (type (first item-spec)))
    (assert (eq type :listitem))
    (let ((list-order-type (otlb::item-spec-kvs-val :order-type item-spec))
	  (list-symbol-type (otlb::item-spec-kvs-val :symbol-type item-spec)))
      ;; map order/symbol types to corresponding *RENDER2* entry
      (let ((render2-list-entry
	     (*render2*-list-entry list-order-type list-symbol-type)))
	(assert render2-list-entry)
	(let ((list-start-tag (fifth render2-list-entry))
	      (list-end-tag (sixth render2-list-entry))
	      (list-item-start-tag (third render2-list-entry))
	      (list-item-end-tag (fourth render2-list-entry)))
	  (write-string list-start-tag stream) 
	  ;; OBJECT-SEQUENCE is a sequence which corresponds to a sequence of list items
	  (dolist (row object-sequence)
	    ;; if this is a list item, the object sequence may have list item designator present

	    ;; lexing/parsing provides CUT and REST values
	    ;; - CUT is a string (unparsed material at the start of the line)
	    ;; - REST is a string (unparsed material matching the portion of the line succeeding the list item designator)
	    ;; - the object sequence itself is the parsed version of REST

	    (let ((item-string 	 
		   ;; two options:
		   ;; 1. use this if object sequence contains remainder of the line
		   (let ((row-object-sequence (second row))) (obj-seq-to-string row-object-sequence)) 
		   ;; 2. use this if REST contains parsed remainder of line
		   ;; (or (otlb::item-spec-kvs-val :rest (otlb::get-item-spec row))
		   ;;     ;; handle empty list item
		   ;;     "")
		    
		    ))
	      (write-string list-item-start-tag stream)
	      (write-string item-string stream)
	      (write-string list-item-end-tag stream))) 
	  (write-string list-end-tag stream))))))

(defun *render2*-list-entry (order-type symbol-type)
  (dolist (x (get-*render2*-value :list))
    (when (and (eq (first x) order-type)
	       (eq (second x) symbol-type))
      (return x))))