(in-package :otlr)
;;;
;;; render-html.lisp: HTML rendering
;;;
;;;==========================================
;;;
;;; define output markup-generating functions
;;;

;; FIXME: update from ./default.css
(defparameter *css-default*
  (dfile::insert-system-file-as-string "default" :otl-html :type "css"))

(defparameter *RENDER-HTML*
  '((:ADMONITION . otlr::admonition-html)
    (:BLOCKQUOTE . otlr::blockquote-html)
    (:BOLD . otlr::bold-html)
    (:BRACKETED . otlr::bracketed-x)
    (:CHAR . otlr::lit-char-html)
    (:CHEM . otlr::chem-expr-html)
    (:CITE . otlr::citation-html)
    (:CODE . otlr::code-html)
    (:COMMENT . otlr::comment-x)
    (:DEFINITION . otlr::definition-html)
    (:DOCBOTTOM . otlr::docbottom-html)
    (:DOCTOP . otlr::doctop-html)
    (:DOCUMENT . otlr::document-html)
    (:EMPHASIS . otlr::emphasis-html)
    (:EMPTYLINE . otlr::emptyline-x)
    (:ESC . otlr::escaped-x)
    ;;(:FOOTER . otlr::footer-html)
    (:FOOTNOTE . otlr::footnote-html)
    (:FOOTNOTES . otlr::footnotes-html)
    (:FUNCTION . otlr::function-x)
    (:GLOSSARY . otlr::glossary-html)
    (:GLOSSTERM . otlr::glossterm-x)
    (:HORIZONTAL-RULE . otlr::horizontal-rule-html)
    (:IMAGE . otlr::image-html) 
    (:INDEXTERM . otlr::indexterm-html)
    (:ITALIC . otlr::italic-html)
    (:LINKPOINTER . otlr::linkpointer-html)
    ;; LINKTARGET items are dealt with prior to rendering (see RENDER-OBJECT-SEQUENCE)
    (:LINKTARGET . nil)
    (:LIST . otlr::list-x)
    (:LIT . otlr::asis-x)
    (:LIT-CHAR . otlr::lit-char-html)
    (:MATH . otlr::math-expr-html)
    (:MONOSPACE . otlr::monospace-html)
    (:NEWLINE . otlr::newline-html)
    (:NOOP . otlr::noop-x)
    (:PAGEBREAK . otlr::pagebreak-html)
    (:PREFORMATTED . otlr::preformatted-x)
    (:SECTHEAD . otlr::secthead-html)
    (:SMALLCAPS . otlr::smallcaps-html)
    (:SMALLER . otlr::smaller-html)
    (:STRIKETHROUGH . otlr::strikethrough-html)
    (:STRONG . otlr::strong-html)
    (:SUB . otlr::sub-html)
    (:SUP . otlr::sup-html)
    (:TABLE . otlr::table-html)
    (:TEXTBLOCK . otlr::textblock-x)
    (:TEXTLINE . otlr::textline-html)
    (:TOC . otlr::toc-x)
    (:UNDER . otlr::under-x)
    (:UNDERLINE . otlr::underline-html)
    (:URI . otlr::uri-html)))

(add-to-*render-set* :html *render-html*)


(defparameter *render2-html*
  (list
   '(:default
     ("<p>" "</p>")
     ("<p style=\"margin-left: 2.5em;\">" "</p>")
     ("<p style=\"margin-left: 4em;\">" "</p>")
     ("<p style=\"margin-left: 5.5em;\">" "</p>")
     ("<p style=\"margin-left: 7.5em;\">" "</p>"))
   '(:empty "<div style=\"margin-top: 0.35em; margin-bottom: 0.35em; padding: 0em;\"></div>")
   '(:foot "</body></html>")
   '(:gloss-filespec-generator nil)

   ;; each element here should map to an element for :LIST in *PARSE*
   '(:list
     (:unordered :disc "<li>" "</li>" "<ul style=\"list-style-type: disc\">" "</ul>")
     (:ordered :numeric  "<li>" "</li>" "<ol>" "</ol>") 
     ;; [A-Z]+ vs. [A-Z]
     ;; -- if we allow arbitrary number of letters, then a line such as "appr. 40 people..." get's parsed as a list item
     (:ordered :alpha-upcase "<li style=\"list-style-type: upper-latin\">" "</li>" "<ol>" "</ol>")
     
     (:ordered :alpha-downcase "<li style=\"list-style-type: lower-latin\">" "</li>" "<ol>" "</ol>"))

   '(:preequivalent "<pre>" "</pre>")

   ;; SUFFIX: a string corresponding to a Common Lisp path type (the string can't contain a period character)
   '(:suffix "html")
   '(:toc-generator otlr::toc-generator-html)))

(add-to-*render2-set* :html *render2-html*)

;;;
;;; html render functions
;;;
(defun admonition-html (object-sequence item-id item-kvs stream)
  (declare (ignore item-id))
  (let* ((type (otlb::item-kvs-val :type item-kvs)))
    (dxh:p
     #'(lambda (s) (format s "! ~A: ~A"
			   type
			   (obj-seq-to-string object-sequence)))
     stream)))

(defun blockquote-html (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  (dxh:blockquote #'(lambda (s) (obj-seq-to-stream object-sequence s)) stream))

;; versus "<span style=\"font-weight: bold;\">" "</span>"? (see also italic-html, etc.)
(defun bold-html (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  (dxh:b #'(lambda (s) (obj-seq-to-stream object-sequence s)) stream))

(defun chem-expr-html (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  ;; assume LaTeX using chemfig package
  (chem-expr-html-chemfig object-sequence stream))

(defun chem-expr-html-chemfig (object-sequence stream)
  (declare (special %out-dir%))
  (latex-to-graphic-html
   object-sequence
   stream
   :generate-image-files-p %out-dir%
   :make-latex #'chem-expr-latex-chemfig
   :packages '("chemfig")))

;; OTL-LATEX/RENDER-LATEX.LISP/CHEM-EXPR-LATEX-CHEMFIG
(defun chem-expr-latex-chemfig (object-sequence stream)
  (write-string "\\chemfig{" stream)
  (otlb::charlist-to-stream object-sequence stream)
  (write-string "}
" stream))

(defun code-html (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  (dxh:code #'(lambda (s) (obj-seq-to-stream object-sequence s)) stream))

(defun definition-html (object-sequence item-id item-kvs stream)
  (declare (ignore object-sequence item-id))
  (dxh:dl
   #'(lambda (s) 
       (dxh:dt #'(lambda (dts) (obj-seq-to-stream
				(otlb::item-kvs-val :term item-kvs) dts)) s)
       (dxh:dd #'(lambda (dds)
		   (render-obj-seq
		    (otlb::item-kvs-val :definition item-kvs)
		    dds))
	       s))
   stream))

(defun docbottom-html (object-sequence item-id item-kvs stream)
  (declare (ignore object-sequence item-id item-kvs)
	   (special %output-subtype%))
  (when *citations-encountered*
    (bibliography-html *citations-encountered* stream))
  (cond ((not (eq %output-subtype% :section))
	 ;; output user-specified end-of-the-document material
	 (let ((tail-string? (first (get-*render2*-value :foot))))
	   (cond ((stringp tail-string?)
		  (write-string tail-string? stream))
		 ((ignore-errors (symbol-function tail-string?))
		  (write-string (funcall tail-string?) stream)))))))

(defun doctop-html (object-sequence item-id item-kvs stream)
  ;; %STYLE% holds the name of a style -- for HTML, this is the name (sans suffix) of a css file in ~/.otl
  (declare (ignore item-id item-kvs object-sequence)
	   (special %obj-seq% %style% %~%))
  )

(defun document-html (object-sequence item-id item-kvs stream)
  ;; %STYLE% holds the name of a style -- for HTML, this is the name (sans suffix) of a css file in ~/.otl
  (declare (ignore item-id)
	   (special %obj-seq% %output-subtype% %style% %~%))
  (cond ((eq %output-subtype% :section)
	 (render-obj-seq object-sequence stream)) 
	(t
	 (write-string "<html xmlns=\"http://www.w3.org/1999/xhtml\"
xmlns:svg=\"http://www.w3.org/2000/svg\">" stream)
	  (multiple-value-bind (author? date-created? date-last-modified? paper-size? title)
	      (otlb::document-item-kvs-as-values
	       item-kvs 
	       :title-default ;; fallback: grab title as the text of second object in the document object sequence
	       (otlb::top-level-chars-to-string (otlb:get-item-obj-seq (second %obj-seq%))))
	    (dxh:head 
	     (with-output-to-string (s)
	       (dxh::meta/ s :http-equiv "Content-Type" :content "text/html;charset=utf-8") 
	       (if author? (dxh::meta/ s :http-equiv nil :name "Author" :content author?))
	       (if date-created? (dxh::meta/ s :http-equiv "Date.Created" :content (otlb::iso8601-date-string date-created?)))
	       (if date-last-modified? (dxh::meta/ s :http-equiv "Date.Modified" :content (otlb::iso8601-date-string date-last-modified?)))
	       ;; CSS/style
	       (cond ((stringp %style%)
		      (dxh:style (style-string %style%) s))
		     (%style%
		      (dxh:style *css-default* s)))
	       (if paper-size?
		   (let ((width (clps::paper-size-width paper-size?))
			 (length (clps::paper-size-length paper-size?
							  )))
		     ;; note that PAPER-SIZES only accomodates units compatible with CSS
		     (dxh:style 
		      (format nil "@media print { min-width: ~A~A; max-width: ~A~A; min-length: ~A~A; min-length: ~A~A}
" 
			      (car width) (clps::unit-to-string (clps::distance-units width))
			      (car width) (clps::unit-to-string (clps::distance-units width))
			      (car length) (clps::unit-to-string (clps::distance-units length))
			      (car length) (clps::unit-to-string (clps::distance-units length)))
		      s :type "text/css"))))
	     :defaults-p nil
	     :title title
	     :stream stream)
	    (write-string "<body>" stream)
	    (render-obj-seq object-sequence stream)))))

(defun emphasis-html (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  (dxh:em (obj-seq-to-string object-sequence) stream))

(defun glossary-html (object-sequence item-id item-kvs stream)
  (declare (ignore object-sequence item-id item-kvs))
  (let ((gs-length (length *gloss-stack*)))
    (dxh:dl
     #'(lambda (s)
	 (do ((i 0 (+ i 1)))
	     ((>= i gs-length))
	   ;; TERM and DEFINITION are object sequences
	   (destructuring-bind (term definition)
	       (nth i *gloss-stack*)
	     (dxh::dt+ #'(lambda (str) (obj-seq-to-stream term str)) s)
	     (dxh::dd+ #'(lambda (str) (obj-seq-to-stream definition str)) s))))
     stream)))

(defun horizontal-rule-html (object-sequence item-id item-kvs stream)
  (declare (ignore object-sequence item-id item-kvs))
  (dxh::hr/ stream))

(defun image-html (object-sequence item-id item-kvs stream)
  (declare (ignore object-sequence))
  ;; FIXME: use parfile to specify image markup
  ;; PATH is a string or a path object corresponding to an image.
  ;; URI is a string
  (let ((uri (otlb::item-kvs-val :uri item-kvs))
	(title (otlb::item-kvs-val :title item-kvs))
	(caption (otlb::item-kvs-val :caption item-kvs))
	(type (or (otlb::item-kvs-val :type item-kvs)
		  :image)))
    (let (;;(path-string (if (pathnamep uri) (namestring uri) uri))
	  ;;(path-object (if (pathnamep path) path (pathname path)))
	  ;;(embed-encoded-p nil)
	  )
      ;; possibilities with images in HTML:
      ;;    1. link to image - <a href=...
      ;;    2. inline image - <img src=...
      (dxh:div
       #'(lambda (ss)
	   (dxh::img/ ss
	    ;; with HTML, include URI as-is
	    ;; ! relative file URIs will almost certainly be problematic...
	    :src ;; (if embed-encoded-p (concatenate 'string "data:image/" type ";base64," (cl-base64:string-to-base64-string (dat-file:file-to-string path-object))) ...
	    uri  ; path-string
	    )
	   (when (eq type :figure)
	     (dxh:p #'(lambda (pstream)
			(dxh:b
			 (concatenate 'string
				      "Figure. "
				      (obj-seq-to-string title))
			 pstream))
	      ss)
	     (if caption
		 (render-obj-seq caption ss))))
       stream
       :id (otlb::acceptable-id item-id)))))

(defun indexterm-html (object-sequence item-id item-kvs stream)
  (declare (ignore object-sequence item-id item-kvs stream))
  ;; (error "INDEXTERM not supported with OTL-HTML.")
  )

;; versus "<span style=\"font-style: italic;\">" "</span>" ?
(defun italic-html (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  (dxh:i #'(lambda (s) (obj-seq-to-stream object-sequence s)) 
	 stream))

;; GENERATE-IMAGE-FILES-P, if true, should be a pathname corresponding to a directory where image files are written
;; MAKE-LATEX is a funcallable object which accepts two arguments, an object sequence and a stream, and writes LaTeX markup corresponding to the object sequence to the stream
;; PACKAGES: list of LaTeX packages to use
(defun latex-to-graphic-html (object-sequence stream &key generate-image-files-p make-latex packages)
  "Send, to stream STREAM, markup corresponding to the bitmap or vector graphic described by object sequence OBJECT-SEQUENCE."
  (declare (optimize (debug 3) (safety 3)) (stream stream))
  ;; generate LaTeX document and then generate pdf document
  (let ((tmppath-tex (dfile:make-unique-path (dfile:tempdir)
					     :createp nil
					     :suffix "tex")))
    (let ((tmpfile-tex (namestring tmppath-tex))
	  (tmppath-pdf (merge-pathnames (make-pathname :type "pdf")
					tmppath-tex)))
      (let ((tmppath-pdf-cropped
	     (dfile:make-unique-path (dfile:tempdir)
				     :createp nil
				     :suffix "pdf"))
	    (tmppath-png (merge-pathnames (make-pathname :type "png")
					  tmppath-tex))
	    ;; (tmppath-svg (merge-pathnames (make-pathname :type "svg")
	    ;; 				  tmppath-tex))
	    )
	(dfile:string-to-file
	 (with-output-to-string (s)
	   (write-string "\\documentclass{article}
" s)
	   (dolist (p packages)
	     (format s "\\usepackage{~A}~%" p))
	   (write-string "\\begin{document}
\\thispagestyle{empty}
" s)
	   (funcall make-latex object-sequence s)
	   (write-string "
\\end{document}" s)
	   )
	 tmppath-tex)
	(otlb::pdflatex tmpfile-tex)	; latex document
	(otlb::pdfcrop tmppath-pdf tmppath-pdf-cropped) ; pdfcrop document
	;; ideally:
	;;   1. if GENERATE-IMAGE-FILES-P is true, generate a high-resolution (300 is about the minimal reasonable value) image or vector graphic suitable for use for presentations, etc.
	;;   2. embed, in the output document, an image which is scaled appropriately so that character height in image approximates character height on web page (issue: this data isn't readily available) 
	(otlb::convert-pdf->png tmppath-pdf-cropped tmppath-png "300")
	(dxh::img/ stream
		   :alt "graphic"
		   :src (concatenate 'string
				     "data:image/png;base64,"
				     (cl-base64:usb8-array-to-base64-string
				      (dfile:file-to-vector tmppath-png))))

	;; instead of generating bitmap, we can also generate SVG
	;; don't use cropped (tmppath-pdf-cropped) since [ true ? ] pdf2svg requires pdf with an explicit page
	;;(otlb::convert-pdf->svg tmppath-pdf-cropped tmppath-svg)
	;; include SVG directly (see DOCTOP-HTML for svg declaration)
	;;   - ? need to delete first line since pdf2svg puts a <?xml ...> declaration on top...
	;;(dfile:file-to-stream tmppath-svg stream)

	;; remove tmp files
	(if generate-image-files-p
	    (dfile::move-files-to-dir
	     (list tmppath-png tmppath-pdf tmppath-pdf-cropped 
		   ;tmppath-svg 
		   tmppath-tex)
	     (make-pathname :directory generate-image-files-p))
	    (dfile:delete-files (list tmppath-png tmppath-pdf tmppath-pdf-cropped 
					;tmppath-svg
				      tmppath-tex)))))))

;; HTML: <a href="#Sample lab safety handout">
;; DocBook: <xref xrefstyle="select: label" linkend="Sample lab safety handout" />
;; LaTeX: \cref{DSOcourseArticulation}
(defun linkpointer-html (object-sequence item-id item-kvs stream)
  "TO-ID, a string, represents the id of element linkpointer should point at."
  ;;(render-linkpointer-html object-seq to-id :id id :stream stream)
  (let ((to-id (otlb::item-kvs-val :to-id item-kvs))) 
    (dxh:a
     (if object-sequence
	 (obj-seq-to-string object-sequence) 
	 to-id)
     stream
     :href (concatenate 'string "#" to-id)
     :id item-id)))

(defun math-expr-html (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  ;; if first char is '$', assume LaTeX math
  ;; if first char is '<', assume MathML
  (cond ((eq #\$ (first object-sequence))
	 (math-expr-html-latex object-sequence stream
			       ;t	; generate image files
			       ))
	((eq #\< (first object-sequence))
	 (warn "MathML unsupported.")
	 (otlb::charlist-to-stream object-sequence stream))
	(t
	 (error "Unsupported math expression format in source document."))))

(defun math-expr-html-latex (object-sequence stream)
  (declare (special %out-dir%))
  (latex-to-graphic-html
   object-sequence
   stream
   :generate-image-files-p %out-dir%
   :make-latex #'(lambda (obj-seq str)
		   (otlb::charlist-to-stream obj-seq str))
   :packages nil))

(defun monospace-html (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  (dxh:span
   #'(lambda (s) (obj-seq-to-stream object-sequence s)) stream
   :style "font-family: monospace;")) 

(defun newline-html (object-sequence item-id item-kvs stream)
  (declare (ignore object-sequence item-kvs))
  (dxh::br/ stream :id item-id))

(defun pagebreak-html (object-sequence item-id item-kvs stream)
  (declare (ignore object-sequence item-kvs))
  (dxh::div/ stream :style "page-break-before: always;" :id item-id))

(defun render-pagebreak (&key id stream)
  (dxh::div/ stream :style "page-break-before: always;" :id id))

(defun secthead-html (object-sequence item-id item-kvs stream)
  (let ((sign (otlb::item-kvs-val :sign item-kvs)))
    (let ((hstring (otlb::map-from-sets-to
		    sign
		    '("H1" "H2" "H3" "H4" "H5" "H6")
		    '((1) (2) (3) (4) (5) (6)))))
      (dxh::xhc hstring
		;; OBJECT-SEQUENCE may have complex structure
		;;(otlb::char-list-to-string object-sequence)
		(obj-seq-to-string object-sequence)
		:id item-id
		:stream stream))))

(defun smallcaps-html (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  (dxh:span 
   #'(lambda (s) (obj-seq-to-stream object-sequence s)) stream
   :style "font-variant: small-caps;"))

(defun smaller-html (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  (dxh:span
   #'(lambda (s) (obj-seq-to-stream object-sequence s)) stream
   :style "font-size: smaller;"))

(defun strikethrough-html (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  (dxh:del #'(lambda (s) (obj-seq-to-stream object-sequence s)) stream))

(defun strong-html (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  (dxh:strong 
   #'(lambda (s) (obj-seq-to-stream object-sequence s)) 
   stream))

(defun sub-html (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  (dxh:sub
   #'(lambda (s) (obj-seq-to-stream object-sequence s))
   stream))

(defun sup-html (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  (dxh:sup
   #'(lambda (s) (obj-seq-to-stream object-sequence s))
   stream))

;; FIXME: unify w/TEXTBLOCK-LATEX -- all code common except language-specific line break
(defun textline-html (object-sequence item-id item-kvs stream)
  (textline-x object-sequence item-id item-kvs stream :line-break-string "<br />"))

(defun toc-generator-html ()
  (with-output-to-string (s)
    (dxh:h2 "Table of contents" s)
    (let (;; '(1) corresponds to first item, outermost list; '(1 2) corresponds to first item in an inner list - inner list attached to second item in outer list; etc...
	  (level (list 0))		; first match bumps to '1'
	  ;; CURRENT-INDEX is count of items at current sublevel
	  ;;(current-index 1)
	  )
      (dolist (x (reverse *toc-store*)) 
	(let ((priority (cdr x))
	      (prev-priority (length level)))
	  ;; if priority exceeds prev-priority then it's a subgroup 
	  (cond ((> priority prev-priority) 
		 (push 1 level))
		((= priority prev-priority)
		 (incf (first level)))
		((< priority)
		 (pop level)
		 (incf (first level))))
	  (format s "~S. ~S" (reverse level) (car x) )
	  (dxh::br/ s))))))

;; or "<span style=\"text-decoration: underline;\">" "</span>"
(defun underline-html (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  (dxh:u
   #'(lambda (s) (obj-seq-to-stream object-sequence s)) 
   stream))

(defun uri-html (object-sequence item-id item-kvs stream)
  (declare (list item-kvs) 
	   (sequence item-id object-sequence)
	   (stream stream))
  ;;(declare (special %uri-visible-p%))
  (let ((scheme (otlb::item-kvs-val :scheme item-kvs))
	;; ITEM-SPECIFIER-STUFF is an object sequence corresponding to the remainder of the URL (sans scheme)
	(item-specifier-stuff (otlb::item-kvs-val :item-specifier-stuff item-kvs)))
    (declare (sequence item-specifier-stuff))
    (let ((post-colon-string (otlb::charlist-to-string item-specifier-stuff)))
      (declare (string post-colon-string))
      (let ((label (if object-sequence 
		      (with-output-to-string (s)
			(render-obj-seq object-sequence s)) 
		      (concatenate 
		       'string scheme ":" 
		       (with-output-to-string (s)
			 (lit-chars-html post-colon-string s))))))
	(dxh::a (if nil			; %uri-visible-p% 
		   (concatenate 'string label "(" post-colon-string ")") 
		   label)
	       stream
	       :href (concatenate 'string scheme ":" post-colon-string)
	       :id item-id)))))
