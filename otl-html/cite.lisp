(in-package :otlr)

(defparameter *citations-encountered* nil "List of citation entries encountered in current run.")

;; FIXME: use bibtex style
(defun bibliography-html (entries stream)
  (dolist (entry entries)
    (cond (entry
	   (dxh:div
	    (concatenate 'string 
			 (otl-cite::entry-author entry) 
			 (otl-cite::entry-year entry)
			 ". "
			 (otl-cite::entry-title entry))
	    stream)
	   )
	  (t (let ((warn-string "Encountered empty reference when constructing bibliography"))
	       (warn warn-string)
	       (write-string (concatenate 'string
					  "( "
 					  warn-string
					  " )")
			     stream))))))

(defun citation-html (object-sequence item-id item-kvs stream &key (refdbp nil))
  (declare (ignore object-sequence item-id refdbp))
  (let ((key (otlb::charlist-to-string (otlb::item-kvs-val :cid item-kvs))))
    (let ((entry (otl-cite::get-entry key)))
      (cond (entry
	     (push entry *citations-encountered*)
	     (dxh:div
	      (concatenate 'string
			   "("
			   (otl-cite::entry-author entry) " " (otl-cite::entry-year entry)
			   ")")
	      stream))
	    (t
	     (let ((warn-string (format nil "Unable to find record for citation with key ~A" key)))
	       (warn warn-string)
	       (write-string
		(concatenate 'string
			     "( "
 			     warn-string
			     " )")
		stream)))))))
