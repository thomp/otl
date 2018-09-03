(in-package :otlr)
;;;
;;; render-table
;;;
(defun table-html (object-sequence item-id item-kvs stream)
  (declare (optimize (debug 3)
		     (safety 3)))
  ;; grab title, caption, column labels, row objects
  (let ((title (otlb::item-kvs-val :title item-kvs))
	(caption (otlb::item-kvs-val :caption item-kvs))
	(col-lbls (otlb::item-kvs-val :header-rows item-kvs))
	(rows-otl (otlb:elts-with-item-key object-sequence :trow)))
    (render-table-html rows-otl
		       :title title
		       :header-rows (otlb:get-item-obj-seq col-lbls)
		       :caption caption
		       :id item-id
		       :stream stream)))

;; TITLE: an object sequence
;; CAPTION: an object sequence
;; HEADER-ROWS: an object sequence
;; ROWS-OTL: an object sequence containing exclusively items where the item-spec itemkey is :TROW
(defun render-table-html (rows-otl &key header-rows id stream title caption)
  (declare (ignore id))
  (if title
      (dxh:b
       #'(lambda (s)
	   (render-obj-seq title s))
       stream))
  (write-string "<table>" stream)
  (if caption
      (dxh:caption
       #'(lambda (cs) (render-obj-seq caption cs))
       stream))
  ;; render table header/columns
  ;; ! including an empty THEAD element seems to confuse fop 0.95
  (when header-rows
    (dxg:xmlc "thead"
	      (with-output-to-string (head-stream) 
		(dolist (tcollbls-row header-rows)
		  (table-row-start-html nil head-stream)
		  (let ((tcollbl-cells (otlb:get-item-obj-seq tcollbls-row)))
		    (dolist (tcollbl-cell tcollbl-cells)
		      (render-table-head-cell-html tcollbl-cell head-stream)))
		  (write-string "</tr>" head-stream)))
	      :stream stream)) 
  (write-string "<tbody>" stream)
  ;; render table rows
  ;; ! FIXME: this should be done in order if writing to stream - use DO instead of MAP
  (dolist (row-otl rows-otl)
    (table-row-start-html (otlb::get-item-item-kvs row-otl) stream)
    (let ((tcells (otlb:get-item-obj-seq row-otl)))
      ;; process cells in order
      (dolist (tcell tcells)
	(render-table-body-cell-html tcell stream)))
    (write-string "</tr>" stream))
  (write-string "</tbody></table>" stream))

(defun render-table-body-cell-html (tcell stream)
  (dxh:td #'(lambda (s) (render-obj-seq (otlb:get-item-obj-seq tcell) s)) stream))

(defun render-table-head-cell-html (tcell stream)
  (dxh:th #'(lambda (s) (render-obj-seq (otlb:get-item-obj-seq tcell) s)) stream))

(defun table-row-start-html (item-kvs stream)
  ;; (MAKE-HTML-ATTRIBUTES item-kvs)
  ;; - look for attribute material
  ;; (MAKE-CSS ...)
  ;; - look for CSS material and prepare string with the form color=blue;font-size=small;...
  (let ((style-string
	 (when item-kvs
	   (with-output-to-string (sss)
	     (map nil #'(lambda (key)
			  (let ((val? (otlb::item-kvs-val key item-kvs)))
			    (when val?
			      (write-string (string key) sss)
			      (write-string ": " sss)
			      (write-string val? sss)
			      (write-char #\; sss))))
		  '(:color)
		  )))))
    (dxg:start-tag
     "tr"
     :attributes (if style-string
		     (list (list "style" style-string)))
     :stream stream)))
