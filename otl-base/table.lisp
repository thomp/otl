(in-package :otlb)
;;;
;;; table.lisp: otl table parse trees
;;;

#|

the structure of otl parse tree representation of a table:

:TABLE
- contains one or more of the following objects

:TCAPTION
- the caption typically will be longer than the title, containing additional information pertinent to the table

:TTITLE
- contains a short title, suitable for use in a table of contents or list of tables

Column labels are described by an object :TCOLLBLS. :TCOLLBLS contains a list/series of :TROW objects.

:TROW 
- contains one or more :TCELL objects

:TCELL 
- contains an object sequence

|#

(defun ctable-partree-spec ()
  (partree-item-list :ctable))

(defun table-otl (rows &key caption columnlabels title)
  "A utility function for building an OTL table. ROWS is a list of lists. Each sublist is a list of strings. COLUMNLABELS is a list of lists. Each sublist is a list of strings. TITLE is a string."
  (table-otl-with-otl-rows
   ;; convert generic 'rows' to otl table row objects
   (mapcar #'(lambda (row)
	       (list '(:TROW)
		     (mapcar #'(lambda (cell)
				 (list '(:TCELL)
				       (to-charlist cell)))
			     row)))
	   rows)
   caption columnlabels title))

(defun table-otl-with-otl-rows (otl-rows caption columnlabels title)
  "A utility function for building an OTL table. ROWS is a list of lists. Each sublist is a list of strings. COLUMNLABELS is a list of lists. Each sublist is a list of strings. TITLE is a string."
  (list (list :TABLE nil 
	      :title (when title (string-to-charlist title)) 
	      :captiontext (when caption (string-to-charlist caption)) 
	      :header-rows (mapcar #'(lambda (col-labels)
				       (list '(:TROW)
					     (mapcar #'(lambda (col-label)
							 (list '(:TCELL)
							       (string-to-charlist col-label)))
						     col-labels)))
				   columnlabels))
	otl-rows))