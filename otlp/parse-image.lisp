(in-package :otlp)
;;;
;;; parse image specifier
;;;
(defun image?-otl ()
  "An image can be referred to using 'image:[PATH\TITLE\CAPTION\ID]' or 'figure:[PATH\TITLE\CAPTION\ID]'. PATH is a URI (for files, only absolute paths are supported). TITLE is the figure or image title (if a title contains complex content, otl generates best-guess text equivalent). CAPTION is the image or figure caption. If ID isn't specified, otl attempts to use the figure title as value of the id attribute of the figure.
"
  (pc:mdo (pc:<- type 
	   (pc:choice1 "figure:["
		    "image:["))
       ;; FOO: list with form (path title caption id)
       (pc:<- foo (image-internal-parser?))
       #\]
       (pc:result 
	(progn
	  (let ((id-charlist? (fourth foo))
		(title? (second foo))	; TITLE?: an object sequence
		(caption? (third foo)))
	    ;; ID: an object sequence
	    (let ((id (or
		       id-charlist?
		       ;; use characters-only version of title if ID doesn't appear to have been specified 
		       (otlb::obj-seq->item-id-string title?))))
	      (when (first foo)
		(push (first foo) *parse-uris*))
	      ;; return list of form (:image id path title caption type)
	      (list (list :IMAGE 
			  id
			  :uri (first foo)
			  :title (if title? (parse-textline-obj-seq title?))
			  :caption (if caption? (parse-textline-obj-seq caption?))
			  :type (cond ((string= type "figure:[") :figure)
				      ((string= type "image:[") :image))))))))))

(defun image-internal-parser? ()
  "Return list of form (image-path title caption id)"
  (pc:mdo (pc:<- uri (items-or-chars-except-char? #\\))
       #\\
       (pc:<- title (items-or-chars-except-char? #\\))
       ;; all this is optional.. 
       (pc:<- caption 
	   (pc:atmost?
	    (pc:mdo "\\"
		 (pc:<- x (pc:atleast?
			(pc:choices
			 (cons?)
			 ;; foo:[glurp] might be nested...
			 ;; - exclude image:[...], figure:[...], gloss:[...] 
			 ;; - admonitions excluded by definition since admonition must reside on a separate line 
			 (citations?)
			 (math-expr?)
			 (not-chars? '(#\Newline #\\ #\]))) 1))
		 (pc:result x))
	    1))
       (pc:<- id 
	   (pc:atmost? (pc:mdo "\\"
			 (pc:<- x (pc:atleast?
				(pc:choices
				 (cons?)
				 (not-chars? '(#\Newline #\]))) 1))
			 (pc:result x))
		    1))
       (pc:result 
	(progn
	  (list (otlb::charlist-to-string uri)
		;; parse TITLE for bracketed content, etc...
		title
		(if caption (first caption))
		(if id (otlb::obj-seq->item-id-string (first id))))))))
