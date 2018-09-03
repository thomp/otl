(in-package :otlr)

;; - use *RENDER2* :TABLE values
;; - alternatively, a custom table rendering function can be specified in *RENDER* for :TABLE
(defun render-table-generic (rows-otl &key id stream caption title tcollbls )
  (declare (ignore id))
  ;; TTITLE TCOLLBLS TCAPTION are the corresponding otl ITEMs
  (destructuring-bind (cell-spec row-start row-end-string table-body-start table-body-end-string table-start table-end-string head-cell-spec)
      (get-*render2*-value :table)
    ;; use title as default for ID 
    (let (;(id-clean (otlb::acceptable-id (otlb::obj-seq->item-id-string (otlb:get-item-obj-seq ttitle))))
	  )

      ;; render table title
      ;; - for languages (e.g., OpenDocument) which don't explicitly describe table title, title precedes table
      (render-obj-seq title stream)

      ;; table start
      (if (stringp table-start)
	  (write-string table-start stream) 
	  (funcall table-start rows-otl stream))
      ;; beginning of table body
      (if (stringp table-body-start)
	  (write-string table-body-start stream) 
	  (funcall table-body-start rows-otl stream))
      ;; render table column labels
      ;; - for languages (e.g., OpenDocument, LaTeX) which don't explicitly describe table header data, it makes sense to include the column 'labels' as additional rows at the top of the table
      (when tcollbls
	(let ((tcollbls-rows tcollbls))
	  (map nil
	       #'(lambda (tcollbls-row)
		   (write-string row-start stream)
		   (let ((tcollbl-cells (otlb:get-item-obj-seq tcollbls-row))) 
		     (cond ((consp head-cell-spec)
			    (let ((head-cell-start-string (first head-cell-spec))
				  (head-cell-end-string (second head-cell-spec)))
			      (dolist (tcollbl-cell tcollbl-cells)
				(write-string head-cell-start-string stream)
				(render-obj-seq (otlb:get-item-obj-seq tcollbl-cell) stream)
				(write-string head-cell-end-string stream))))
			   ((stringp head-cell-spec)
			    (let ((n-tcollbl-cells (length tcollbl-cells)))
			      (dotimes (i (1- n-tcollbl-cells))
				(render-obj-seq (otlb:get-item-obj-seq 
						 (elt tcollbl-cells i)) stream)
				(write-string head-cell-spec stream)) 
			      (render-obj-seq (otlb:get-item-obj-seq 
					       (elt tcollbl-cells (1- n-tcollbl-cells))) stream)))))
		   (write-string row-end-string stream))
	       tcollbls-rows))
	)
      ;; render table rows
      (dolist (row-otl rows-otl) 
	(write-string row-start stream)
	(let ((tcells (otlb:get-item-obj-seq row-otl)))
	  (cond ((consp cell-spec)
		 (let ((cell-start-string (first cell-spec))
		       (cell-end-string (second cell-spec)))
		   ;; process cells in order
		   (dolist (tcell tcells)
		     (write-string cell-start-string stream)
		     (render-obj-seq (otlb:get-item-obj-seq tcell) stream)
		     (write-string cell-end-string stream))))
		((stringp cell-spec)
		 (let ((n-tcells (length tcells)))
		   (dotimes (i (1- n-tcells))
		     (render-obj-seq (otlb:get-item-obj-seq (elt tcells i)) stream)
		     (write-string cell-spec stream))
		     
		   (render-obj-seq (otlb:get-item-obj-seq (elt tcells (1- n-tcells))) stream)))))
	(write-string row-end-string stream))
      ;; end table body
      (write-string table-body-end-string stream) 
      (write-string table-end-string stream)
      ;; render caption
      ;; - for languages (e.g., OpenDocument, LaTeX) which don't explicitly describe table caption, render it after the table
      (if caption
	  (write-string (obj-seq-to-string caption) stream)))))

(defun print-table-otl (table &key columnlabels (output-spec :text) stream title)
  "A utility function for generating a stand-alone document containing solely a table. TABLE is a list of lists of strings."
  (render-document 
   (list (otlb::table-otl table :columnlabels columnlabels :title title))
   :output-spec output-spec
   :stream stream))

(defun table-x (object-sequence item-id item-kvs stream) 
  (declare (optimize (debug 3)))
  ;; grab title, caption, column labels, row objects
  (let ((title (otlb::item-kvs-val :title item-kvs))
	(caption (otlb::item-kvs-val :caption item-kvs))
	(col-lbls (otlb::item-kvs-val :header-rows item-kvs))
	(rows-otl (otlb:elts-with-item-key object-sequence :trow)))
    (render-table-generic rows-otl :title title :tcollbls col-lbls :caption caption :id item-id :stream stream)))
