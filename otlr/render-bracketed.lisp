(in-package :otlr)

(defun bracketed-x (object-sequence item-id item-kvs stream)
  (declare (ignore item-id))
  (let ((bstart (otlb::item-kvs-val :bstart item-kvs))
	(bend (otlb::item-kvs-val :bend item-kvs)))
    (assert (or bstart bend))
    (when object-sequence
      (let ((br-start-br-end (bracketed-render-pair bstart bend)))
	(let ((br-start (first br-start-br-end)) 
	      (br-end (second br-start-br-end)))
	  (when br-start (write-string br-start stream))
	  (render-obj-seq object-sequence stream)
	  (when br-end (write-string br-end stream)))))))

(defun bracketed-render-pair (bstart bend)
  "Return corresponding markup start object and end object as list of form (mstart mend)."
  ;; supporting bstart ≠ bend is costly since match must be made against the string pair
  ;; ;; assume bstart = bend -- and simple entries of the form ("**" "<span style=\"font-weight: bold;\">" "</span>"): (assert (string= bstart bend))
  ;; partree: each member has form (bracketmarkup-startN bracketmarkup-endN markupstart markupend)
  (let ((bracketed-tree (otlb:partree-item-list :bracket)))
    ;; bstart = bend: (otlb::aval bstart bracketed-tree #'string=)
    (first				; if there are duplicate pairs, use first pair
     (otlb::avals-deep (list bstart bend) bracketed-tree #'string=))))
