(in-package :otlr)
;;;
;;; render-x.lisp: markup/language-agnostic render functions
;;;

;; passthrough (escape from otl processing)
(defun asis-x01 (object-sequence item-id item-kvs stream)
  "ASIS text should be rendered 'as-is' OBJECT-SEQ must be a sequence composed solely of character objects and/or ASIS items."
  (declare (ignore item-id item-kvs))
  (dolist (x object-sequence)
    (if (characterp x)
	(write-char x stream)
	(asis-x (second x) nil nil stream))))

;; literal (escape from otl processing and ensure char isn't treated as special char downstream)
(defun asis-x (object-sequence item-id item-kvs stream)
  "ASIS text should be rendered 'as-is' OBJECT-SEQ must be a sequence composed solely of character objects and/or ASIS items."
  (declare (ignore item-id item-kvs))
  (dolist (x object-sequence)
    (if (characterp x)
	;; transform characters as necessary to ensure they are not treated as special characters in the output language
	(progn
	  (let ((lit-char-renderer? (get-*render*-value :LIT-CHAR)))
	    (if lit-char-renderer?
		(funcall lit-char-renderer? x stream)
		(write-char x stream))))
	(asis-x (second x) nil nil stream))))

(defun char-x (char stream)
  (write-char char stream))

(defun comment-x (object-sequence item-id item-kvs stream)
  "Ignore comment."
  (declare (ignore object-sequence item-id item-kvs stream)))

(defun docbottom-x (object-sequence item-id item-kvs stream)
  (declare (ignore object-sequence item-id item-kvs))
  ;; output user-specified end-of-the-document material
  (let ((tail-string? (first (get-*render2*-value :foot))))
    (cond ((stringp tail-string?)
	   (write-string tail-string? stream))
	  ((ignore-errors (symbol-function tail-string?))
	   (write-string (funcall tail-string?) stream)))))

(defun doctop-x (object-sequence item-id item-kvs stream)
  (declare (ignore object-sequence item-id item-kvs))
  (let ((head-string? (first (otlb:partree-item-list :head))))
    ;; TODO: grab style information, if available...
    ;; (if style
    ;; (cond ((equalp (first (otlb:partree-item-list :suffix)) "html")
    ;; 	   (let ((insert-at (cl-ppcre:scan "</head>" head-string)))
    ;; 	     (write-string
    ;; 	      (dat-cl-utils::insert-in-sequence
    ;; 	       head-string
    ;; 	       (dxh:style-xhc (style-string style) :protect-with-cdata-p t :stream stream :type "text/css")
    ;; 	       insert-at)
    ;; 	      stream)))
    ;; 	  (t (error "Don't know what to do with style")))
    (cond ((stringp head-string?)
	   (write-string head-string? stream))
	  ((ignore-errors (symbol-function head-string?))
	   (write-string (funcall head-string?) stream)))))

(defun emptyline-x (object-sequence item-id item-kvs stream)
  (declare (ignore object-sequence item-id item-kvs))
  (write-string (first (get-*render2*-value :empty))
		stream))

(defun escaped-x (object-sequence item-id item-kvs stream)
  (declare (ignore item-id item-kvs))
  (otlb::charlist-to-stream object-sequence stream))

;; markup-agnostic as long as %output-lang% retained
(defun function-x (object-sequence item-id item-kvs stream)
  (declare (special %output-lang%)
	   (ignore object-sequence item-id))
  (let ((function (first item-kvs)))
    (let ((string (funcall function %output-lang%)))
      (write-string string stream))))

(defun noop-x (object-sequence item-id item-kvs stream)
  "Don't perform any sort of transformation on the text apart from transformations specified in items in OBJECT-SEQUENCE."
  (declare (ignore item-id item-kvs))
  (render-obj-seq object-sequence stream))

(defun preformatted-x (object-sequence item-id item-kvs stream)
  "Preformatted text. OBJECT-SEQUENCE must be composed exclusively of characters."
  (declare (ignore item-id item-kvs))
  (let ((preequiv-start-stop (get-*render2*-value :preequivalent)))
    (write-string
     (concatenate 'string
		  (first preequiv-start-stop)
		  (otlb::charlist-to-string object-sequence)
		  (second preequiv-start-stop))
     stream)))

(defun render-char-xml (char stream)
  (cond ((eq char #\<)
	 (write-string "&lt;" stream))
	((eq char #\>)
	 (write-string "&gt;" stream))
	((eq char #\&)
	 (write-string "&amp;" stream))
	(t
	 (write-char char stream))))

(defun textblock-pairs ()
  (get-*render2*-value :default))

(defun textblock-x (object-sequence item-id item-kvs stream)
  (declare (ignore item-id))
  (let ((tablevel? (otlb::item-kvs-val :tablevel item-kvs)))
    (let ((textblock-pairs (textblock-pairs))
	  (tablevel (if (integerp tablevel?) tablevel? 0)))
      (let ((tb-pair (if (>= (length textblock-pairs)
			     (1+ tablevel))
			 (nth tablevel textblock-pairs))))
	(if tb-pair (write-string (first tb-pair) stream))
	(render-obj-seq object-sequence stream)
	(if tb-pair (write-string (second tb-pair) stream))))))

(defun textline-x (object-sequence item-id item-kvs stream &key line-break-string) 
  (declare (ignore item-id)
	   (special %containing-obj-seq% %containing-obj-seq-pointer%))
  (render-obj-seq object-sequence stream)

  ;; handle a series of textlines of the same indent within a single textblock
  ;; ISSUE: this approach to handling multiple textlines of the same tablevel within a textlock only is aesthetically pleasing if paragraphs don't have the first line indented 
  ;; -->> use the parskip package
  ;; -->> must check if line break is warranted -- can't simply put this indiscriminately everywhere (textlines show up in all sorts of places) 
  (let ((tablevel (otlb::item-kvs-val :tablevel item-kvs))) 
    (if (and line-break-string
	     (let ((prev-item (if (> %containing-obj-seq-pointer% 0)
				  (nth (1- %containing-obj-seq-pointer%)
				       %containing-obj-seq%)))
		   (next-item (if (< %containing-obj-seq-pointer% (1- (length %containing-obj-seq%))) 
				  (nth (1+ %containing-obj-seq-pointer%)
				       %containing-obj-seq%))))
	       ;; if the prev-item test is necessary, document here the specific test case which demonstrates this
	       (or nil ; 
		   ;; (and prev-item
		   ;; 	(otlb::itemp prev-item :textline)
		   ;; 	(otlb::item-tablevel-p prev-item tablevel))
		   (and next-item
			(otlb::itemp next-item :textline)
			(otlb::item-tablevel-p next-item tablevel)))))
	(write-string line-break-string stream))))

;; UNDER-START, UNDER-END, and UNDER-PRIORITY are specified by the end-user in the PARTREE variable - see OTL-HTML/VARS-HTML.LISP for particulars...
(defun under-x (object-sequence item-id item-kvs stream)
  (let ((under-string (otlb::item-kvs-val :under-string item-kvs)))
    (destructuring-bind (under-start under-end under-priority)
	(otlb::aval under-string (otlb::under-tree))
      ;; table of contents support:
      (push (cons
	     (otlb::top-level-chars-to-string object-sequence)
	     under-priority)
	    *toc-store*)
      ;; now render item...
      ;; - for certain item types (:LIST), only consider the text
      (let ((custom (list :list
			  ;; L-OBJ-SEQ: object sequence of the list item
			  #'(lambda (l-obj-seq l-stream)
			      (obj-seq-to-stream l-obj-seq l-stream nil)))))
	;; start component and content
	(cond ((otlb::funcallablep under-start)
	       (write-string
		(funcall under-start
			 (obj-seq-to-string object-sequence custom)
			 item-id)
		stream))
	      (t
	       (write-string under-start stream)
	       ;; object-sequence: see docs/parser.txt
	       (render-obj-seq object-sequence stream :custom custom)))
	;; end component
	(write-string
	 (if (otlb::funcallablep under-end)
	     (funcall under-end
		      (obj-seq-to-string object-sequence custom)
		      item-id)
	     under-end)
	 stream)))))
