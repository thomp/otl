(in-package :otlp)
;;;
;;; parse.lisp: work with *PARSE*
;;;

;;
;; *PARSE* is an alist where each member has the form (parsewhere pair1 pair2 ... pairN)
;;   - PARSEWHERE is a keyword, one of :TEXTLINE or :DOCUMENT
;;
;;  A pair is a list with two members (PARSE-SPEC INTERNAL-SPEC)
;;
;;  A PARSE-SPEC is either a keyword PARSERTYPE or a list of the form (parsertype arg1 ... argN) where PARSERTYPE is a keyword
;;     - valid PARSERTYPE values include 
;;         1. :BRACKET       example: (:BRACKET "*" "*")
;;         2. :UNDER         example: (:UNDER "-" 1)
;;         3. :PARSER*       example: (:PARSER* foo? fooarg1 fooarg2)
;;
;;  An INTERNAL-SPEC is either an item-spec or a keyword
;;       example: :EMPHASIS
(defparameter *parse-otl*
  (list
   (list :textline
	 '((:literal "+++" "+++") :lit)
	 '((:bracket "**" "**") :bold) 
	 '((:bracket ";;" ";;") :italic)
	 '((:bracket "__" "__") :underline)
	 '((:bracket "``" "``") :monospace)
	 '((:bracket "&&" "&&") :smallcaps)
	 '((:bracket "~~" "~~") :strikethrough)
	 '((:bracket "^^" "^^") :sup)
	 '((:bracket ",," ",,") :sub)
	 '((:parser* image?-otl))
	 '((:parser* linkpointer?-otl))
	 '((:parser* uri?-otl))
	 '((:parser* uri?))
	 )
   ;; see DOCUMENT-COMPONENT?
   (list :document	 
	 '((:parser* definition?-otl))
	 ;; UNDER parse-spec has the form (:UNDER UNDER-STRING UNDER-SIGNIFICANCE)
	 ;;   - UNDER-SIGNIFICANCE is an integer (higher value reflects lower priority)
	 '((:under "=" 1) (:SECTHEAD nil :SIGN 1))
	 '((:under "-" 1) (:SECTHEAD nil :SIGN 2))
	 '((:under "~" 1) (:SECTHEAD nil :SIGN 3))
	 '((:under "'" 1) (:SECTHEAD nil :SIGN 4))

	 '((:parser* ctable?))
	 '((:parser* admonition?))
	 '((:parser* glossary?))
	 '((:parser* pagebreak?))
	 ;; tablevel +1 is aesthetically intuitive as a 'top-level' list
	 '((:parser* list? :tablevel 0))
	 '((:parser* list? :tablevel 1))
	 )

   ;;
   ;; components handled at lex level:
   ;;

   ;; comments are parsed at document level but identified by lexer
   '(:comment
     ((:comment "//") nil))
   '(:escape-char
     ((:escape-char #\SYMBOL_FOR_ESCAPE) nil))

   (list :list
	 ;; ((parsertype lexrule) list-spec) where LIST-SPEC has the form
	 ;;   (order-type symbol-type)
	 ;;   examples: (:unordered :disc), (:ordered :numeric) (:ordered :alpha-upcase) (:ordered :alpha-downcase) 
	 '((:list "-") (:unordered :disc))
	 '((:list "[0-9]+\\.") (:ordered :numeric))
	 ;; [A-Z]+ vs. [A-Z]
	 ;; -- if we allow arbitrary number of letters, then a line such as "appr. 40 people..." get's parsed as a list item
	 '((:list "[A-Z]\\.") (:ordered :alpha-upcase))
	 '((:list "[a-z]\\.") (:ordered :alpha-downcase)))
   (list :table
	 (list :ctable
	       ;; ([cell separator] [header character])
	       ;; CELL SEPARATOR: string used to separate cells
	       (list "|" 		; cell separator
		     "-" 		; header string
		     )))))

;; DEFPARAMETER -> encourge use of REFRESH-*PARSE*
(defparameter *parse* nil)

(defvar *parse-trees* nil
  "An alist of potential *PARSE* values, organized by keywords representing input languages.")

(defun *parse*-pairs (parserloc)
  (assert (keywordp parserloc))
  (otlb::aval parserloc *parse*))

(defun *parse*-parsers (parserloc)
  "Return a list of parsers."
  (let ((parse-pairs (*parse*-pairs parserloc)))
    (loop
       for pair in parse-pairs
       collect (pair->parser pair))))

(defun *parse*-document-parsers ()
  "Return a list of parsers."
  (append
   (*parse*-parsers :document)
   (*parse*-line-parsers)
   (*parse*-linesgroup-parsers)
   (list (*parse*-document-textblock-parser))))

(defun *parse*-line-parsers ()
  (mapcar #'(lambda (pair)
	      (let (;;(parse-spec (pair-parse-spec pair))
		    (internal-spec (pair-internal-spec pair)))
		(let ((parsertype (pair-parsertype pair))) 
		  (pc:mdo (pc:<- lline
				 (lline? (first internal-spec)))
			  (pc:result
			   (progn
			     (list (list (first internal-spec) nil)
				   ;; appropriate parser here varies
				   ;; 1. if PARSETYPE is :EXACT --> no parsing: (otlb::merge-obj-seqs ...
				   ;; 2. if PARSERTYPE is :PARSER -> let parser return value
				   ;; 3. if PARSERTYPE is :BEGINSWITH --> appropriate to parse each line individually and then merge object sequences 
				   (case parsertype
				     (:exact internal-spec)
				     ;;(:parser (error "Need to code/verify this"))
				     (:beginswith
				      (error "This is untested... Remove this error and test.")
				      (parse-llines (list lline)
						    (pc:atleast?
						     ;; FIXME: same issue as with LIST-ITEM?
						     (textblock?-otl 0 (first internal-spec)) 1)))))))))))
	  (*parse*-pairs :line)))

(defun *parse*-linesgroup-parsers ()
  (mapcar #'(lambda (pair)
	      (let ((internal-spec (pair-internal-spec pair))) 
		(pc:mdo (pc:<- llines
			       (pc:atleast? (lline? (first internal-spec)) 1))
			(pc:result 
			 (progn
			   (list (list (first internal-spec) nil)
				 ;; BLOCKQUOTE: can/should contain P-ish item(s) --> appropriate to parse each line individually and then merge object sequences 
				 (parse-llines llines 
					       (pc:atleast? 
						(textblock?-otl 0 (first internal-spec)) 1))
				 ))))
		))
	  (*parse*-linesgroup-pairs)))

(defun *parse*-textline-parsers ()
  "Return a list of parsers."
  (*parse*-parsers :textline))

(defun internal-spec-as-item-spec (internal-spec)
  (if (consp internal-spec)
      internal-spec
      (list internal-spec nil)))

(defun input-specs ()
  "Return a list of valid INPUT-SPEC values given the current value of *PARSE-TREES*."
  (mapcar #'(lambda (x) (first x))
	  *parse-trees*))

(defun pair->beginswith-parser (parse-spec internal-spec &optional (internal-parser-symbol 'textline-string-parser))
  (let ((bstart (second parse-spec)))
    ;; look for lline :nonemptyline beginning with...
    (pc:mdo (pc:<- lline 
	     (lline? :nonemptyline 
		     nil
		     ;; fn accepts sequence of characters and returns a true value or NIL
		     #'(lambda (char-seq) 
			 (otlb::seq= 
			  bstart
			  (subseq char-seq 
				  0 
				  (min (length bstart)
				       (length char-seq))) 
			  #'char=))))
	 (pc:result 
	  (let ((item-spec
		 (if (keywordp internal-spec)
		     (list internal-spec)
		     internal-spec)))
	    (list
	     item-spec
	     (parse-obj-seq (second lline) 
			    internal-parser-symbol)))))))

(defun pair->parser (pair)
  (let ((parsertype (pair-parsertype pair))
	(parse-spec (pair-parse-spec pair))
	(internal-spec (pair-internal-spec pair)))
    (cond ((eq parsertype :beginswith)
	   (pair->beginswith-parser parse-spec internal-spec))
	  ((eq parsertype :bracket)
	   (pair->bracketed-parser parse-spec internal-spec))
	  ((eq parsertype :literal)
	   (pair->bracketed-parser 
	    parse-spec internal-spec
	    ;; :LITERAL is like :BRACKET but internal material is not subjected to further parsing apart from escaped char parsing
	    'otlp::parse-for-escaped-chars-parser))
	  ;;((eq parsertype :parser) parse-spec)
	  ((eq parsertype :parser*)
	   (apply
	    (second parse-spec)
	    (if (> (length parse-spec) 2)
		(subseq parse-spec 2))))
	  ((eq parsertype :under)
	   (pair->under-parser parse-spec internal-spec))
	  ;; silently ignore :TEXTBLOCK until DOCUMENT-COMPONENT? is cleaned up and separate *PARSE*-DOCUMENT-PARSERS call is removed
	  ((eq parsertype :textblock)
	   nil)
	  (t
	   (error (format nil "Unsupported PARSERTYPE value ~S" parsertype))))))

(defun pair-parse-spec (pair)
  (first pair))

(defun pair-parsertype (pair)
  (let ((parse-spec (pair-parse-spec pair)))
    (if (consp parse-spec)
	(first parse-spec)
	parse-spec)))

(defun pair-internal-spec (pair)
  (second pair))

(defun parse-otl (in &key indexterms (input-spec :otl))
  "Parse otl markup and return a otl document item. IN is a path, string, or input stream corresponding to otl markup. %INDEXTERMS% is set to the value of INDEXTERMS."
  (let ((%indexterms% nil))
    (declare (special %indexterms%))
    ;; PARSE-RESET sets %indexterms%
    (parse-reset input-spec indexterms)
    ;; parse everything as a string
    (let ((instring?
	   (cond ((stringp in)
		  in)
		 ((streamp in)
		  (otlb::slurp-stream-into-string in))
		 ((pathnamep in)
		  (dfile:file-to-string in))
		 (t (error "IN must be a string, input stream, or pathname")))))
      (unless instring?
	(error "IN may correspond to a nonexistent file."))
      (parse-string instring?))))

(defparameter *parse-uris* nil
  "Convenience accumulator for URIs encountered while parsing.")
(defun parse-reset (input-spec indexterms)
  (declare (special %indexterms%))
  (refresh-*parse* input-spec)
  ;; set index terms
  (setf %indexterms% indexterms)
  ;; as a convenience, accumulate URIs
  (setf *parse-uris* nil))

;; FIXME: how is parse-document distinct from parse-string?

(defun parse-string (string &key author date-created date-last-modified paper-size-nickname title)
  "Parse otl markup represented by string STRING. Return the corresponding parse tree."
  (declare (special %output-lang%))
  ;; lex line-by-line (see lexer.lisp)
  (let ((lex-vector (lex-it string)))
    ;; post-process lex vector (see LEXER-POST.LISP)
    (let ((post-processed-lex-vector (post-process-lex-vector lex-vector)))
      (multiple-value-bind (parse-result context-result) 
	  (parse-vector (document? :author author :date-created date-created :date-last-modified date-last-modified :paper-size-nickname paper-size-nickname :title title) post-processed-lex-vector)
	(declare (ignore context-result)) 
	(let ((current-result (parser-combinators:current-result parse-result))) 
	  (if current-result
	      (parser-combinators:tree-of current-result)))))))

(defun refresh-*parse* (&optional input-spec)
  "Refresh *PARSE* based on INPUT-SPEC (a keyword)."
  (cond ;; ((or (stringp input-spec) (pathnamep input-spec))
	;;  (refresh-parfile input-spec))
	((keywordp input-spec)
	 (let ((parse-tree? (second 
			     (assoc input-spec *parse-trees*))))
	   (unless parse-tree?
	     (error "Unrecognized INPUT-SPEC type.")) 
	   (setf *parse* parse-tree?)))
	(t
	 (error "Unrecognized INPUT-SPEC type."))))

(defun update-*parse-trees* (output-type parse-tree)
  (if (assoc output-type *parse-trees*)
      (setf (cdr (assoc output-type *parse-trees*)) 
	    (list parse-tree))
      (push (list output-type parse-tree) *parse-trees*)))

(update-*parse-trees* :otl *parse-otl*)
