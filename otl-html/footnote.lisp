(in-package :otlr)
;;;
;;; footnotes
;;;
(defun footnotes-html (object-sequence item-id item-kvs stream)
  (declare (ignore object-sequence item-id item-kvs))
  (let ((fs-length (length *footnote-stack*)))
    (cond ((> fs-length 0)
	   (dxh:p #'(lambda (s) (dxh:b "Footnotes" s)) stream)
	   (do ((i 0 (+ i 1)))
	       ((>= i (length *footnote-stack*)))
	     (let* ((footnote-pos-in-stack (1- (- fs-length i)))
		    (footnote-number (1+ i))	; number from 1
		    (footnote (nth footnote-pos-in-stack *footnote-stack*)))
	       (cond ((eq *footnote-style* :numeric)
		      ;; enclose in simple container
		      (textblock-x 
		       (append (otlb::string-to-charlist
				(format nil "~A. " footnote-number))
			       footnote)
		       nil		; item-id
		       '(:tablevel 0)
		       stream)) 
		     (t (error "Unsupported footnote style (RENDER-FOOTNOTES-TEXT)")))))))))

(defun footnote-transform-html ()
  (declare (special %obj-seq% %obj-seq-pointer%))
  ;; some markup languages aren't too bright/aesthetic when it comes to whitespace and footnote markers
  ;; modify previous item if it is space and is succeeded by a footnote item
  (let ((previtem (otlb::prev-obj %obj-seq% %obj-seq-pointer%)))
    (cond ((eq previtem #\Space)
	   (setf (nth (1- %obj-seq-pointer%) %obj-seq%)
		 nil))
	  ;; only for numeric lists... add comma to footnotes trailing other footnotes
	  ((eq (otlb:get-item-key previtem) :footnote)
	   (setf (second (nth %obj-seq-pointer% %obj-seq%))
		 ;; push comma on to end
		 (append
		  (second (nth %obj-seq-pointer% %obj-seq%))
		  '(#\, #\Space)))))))

(defun footnote-html (object-sequence item-id item-kvs stream)
  (declare (special %obj-seq% %obj-seq-pointer% %output-lang%)
	   (ignore item-kvs))
  ;; ?? where is *FOOTNOTE-STACK* handled ?? 
  (push object-sequence *footnote-stack*)
  (let ((footnote-number (length *footnote-stack*)))
    ;; FIXME: get markup from bracket parfile stuff? - get superscript by extending bracket spec so that each item has an optional label - e.g., ("<sup>" </sup>" :superscript)
    (cond ((eq *footnote-style* :numeric)	 
	   (let ((footnote-string
		  (concatenate 'string
			       (otlb::tostring footnote-number) 
			       ;; detect if this footnote precedes another footnote
			       ;; - precedes includes
			       ;;   - next item is a footnote
			       ;;   - next items are a series of NILs followed by a footnote
			       ;;     -> easiest to remove NILs earlier? have a 'clean object sequence' -- no NIL objects...
			       (let ((next-obj (otlb:next-obj 
						%obj-seq% %obj-seq-pointer%
						#'(lambda (x) 
						    (not (eql x #\ ))))))
				 ;; if next object is a footnote, a comma should succeed this item
				 (cond ((and (otlb:itemp next-obj) 
					     (eq (otlb:get-item-key next-obj)
						 :footnote))
					", ")
				       (t ""))))))
	     (dxh:sup
	      ;; start counting at one 
	      footnote-string stream
	      :id item-id)))
	  (t (error "Unknown footnote style")))))
