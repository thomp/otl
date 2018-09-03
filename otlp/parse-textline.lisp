(in-package :otlp)
;;
;; parse-textline.lisp: top-level text block parsing
;;
(defun *textline-parsers* ()
  ;; no lists, tables, ...
  (append
   (list
    ;; a textline may contain items (structured content)
    (cons?)
    (asis-inline?)
    (chem-expr?)
    (citations?)
    (footnote?)
    (glossterm?)
    ;;(image-otl?)  <- this should be reintroduced when otl-specific version is generated...
    (indexterm?)
    (linktarget?)
    ;;(linkpointer?-otl)  <- this should be reintroduced when otl-specific version is generated...
    (math-expr?)
    ;;(uri?-otl) <- this should be reintroduced when otl-specific version is generated...
    ;; component left untouched...
    ;; example: if LaTeX output, leave stuff that looks like latex commands alone
    ;;(inline-asis?)
    )
   (*parse*-textline-parsers)
   (list 
    ;; *PARSE*-TEXTLINE-PARSERS may contain other parsers which grab URIs -- let them do their thing first and, as a convenience, catch a plain-vanilla URI here...
    ;; (uri?-otl)
    ;; for smooth integration with other parsers, the SUBST? parser must be called last (only after it is clear that the region doesn't contain a URI or other matter which must be parsed in a distinct manner)
    (subst?))))

(defun add-asis-pair-to-replacement (r)
  "R is either a string or a list (see cl-ppcre)."
  (let ((asis-pair (otlb:partree-item-list :asis)))
    (cond ((stringp r)
	   (concatenate 'string (first asis-pair) r (second asis-pair)))
	  ;; handle (string index string)
	  ((consp r)
	   (list
	    (concatenate 'string (first asis-pair) (first r))
	    (second r)
	    (concatenate 'string (third r) (second asis-pair))))
	  (t (error (format nil "Can't handle R: ~A" r))))))

(defun start-of-textline? (&optional parsers)
  "Return an item."
  ;; note: calling fn, TEXTLINE-STRING-PARSER, already has grabbed any tabs at the start
  (pc:mdo
    ;; silently discard other whitespace at the start of the line
    (pc:many? (whitespace-except-newline?))
    ;; succeeded by a non-whitespace character or a match against one of PARSERS
    (pc:<- x1 
	   (if parsers
	       ;; single match with one of PARSERS or a string of text (beginning with a non-whitespace character and continuing to another parser match, to the end, or to the first newline)
	       (pc:choices
		(choices-list parsers)
		;; is this an issue? see textline parser -- the material being parsed here may be a mix of items and text -- the first entity in the sequence being parsed (start of textline) may be a URI or another already-parsed item -->> need to look for something which is either not whitespace OR a non-empty item (difficulty: whether an item ends up being whitespace is dependent on the decision of the renderer -->> need to arbitrarily decide what constitutes a non-empty item) -->> define NON-WHITESPACEY? and NON-WHITESPACEY-P to handle this

		;; match text to #\Newline or the end of the file but don't consume the #\Newline
		;; - match one char at a time
		;; - first char shouldn't be whitespace
		(non-whitespace?) ; (non-whitespace-or-cons?) ; formerly (non-whitespace?)
		)
	       (non-whitespace?)))
    (pc:result
     (progn
       ;; X1 could be a single char or could be a parse tree from a parser in PARSERS -> PARSERS must contain only parsers which return items (no object sequences...)
       x1))))

;; PARSER: default parser doesn't bundle initial chars into a :TEXTLINE object
(defun parse-textline-obj-seq (x)
  "X is an object sequence representing the unparsed or partially parsed (the sequence may be composed of characters and/or :ASIS items) content of a textline (see TEXTLINE?). Such a sequence should not yet have been parsed as a textline."
  (let* ((parse-result
	  ;; PARSE-STRING parses other sequences as well...
	  (pc:parse-string (textline-obj-seq-parser) x))
	 (current-parse-result (parser-combinators:current-result parse-result)))
    ;; anticipate possibility of parsing failure
    (if current-parse-result
	(parser-combinators:tree-of current-parse-result))))

;; a modified version of TEXTLINE-STRING-PARSER
(defun textline-obj-seq-parser (&key parsers stop-at)
  "Parse text content of a textline (see TEXTLINE?). Return an object sequence."
  (let ((parsers (or parsers (*textline-parsers*))))
    (pc:mdo
      ;; a textline may begin with tab characters
      (tabs?)
      ;; there might be other whitespace at the start of the line - this should be silently discarded
      ;; succeeded by a non-whitespace character or a match against one of PARSERS
      ;; X1: an item
      (pc:<- x1 (start-of-textline? parsers))
      ;; once matched against a parser or a non-whitespace first char, look for other stuff
      ;; X2: object-seq
      (pc:<- x2
	  (pc:many?
	   (if parsers
	       (pc:choices
		;; match escaped characters (as already-parsed otl items)
		(choices-list parsers)

		;; if STOP-AT (a character) is true, stop at STOP-AT or #\Newline

		;; match text to #\Newline or the end of the file but don't consume the #\Newline
		;; - match one char at a time
		;; - first char shouldn't be whitespace 
		(if stop-at
		    (not-chars? (list stop-at #\Newline))
		    (not-char? #\Newline)))
	       (if stop-at
		   (not-chars? (list stop-at #\Newline))
		   (not-char? #\Newline)))))
      (pc:result 
       (progn
	 ;; return an object sequence
	 (cons x1 x2))))))

(defun textline-string-parser (&key parsers)
  (textline-obj-seq-parser :parsers parsers))

(defun textline? (tablevel &optional (type :NONEMPTYLINE))
  "A textline corresponds to a lexed object representing a single line which is not under content, a list item, a page break, or an empty line. A textline may contain other structured content (e.g., bracketed content, etc.). A textline is not represented as an item but as an object sequence."
  ;; see lexer.lisp OTL-LEXER definition to see what types of lines are caught before a nonemptyline is caught
  ;; X: a member of the vector of the form generated by LEX-AND-PARSE; X looks like (:NONEMPTYLINE (#\T #\h #\i #\s #\  #\s #\h #\o #\u #\l #\d #\  #\b #\e #\  #\a #\  #\p #\a #\r #\a #\g #\r #\a #\p #\h #\. #\Newline))
  (pc:mdo (pc:<- x 
		 (lline? type tablevel))
	  (pc:result 
	   (progn
	     (textline-result 
	      (if (not (and (consp (first x))
			    (eq :list (first (first x)))))
		  (second x)
		  ;; if TYPE is :LIST, line is treated as a regular line
		  ;; -->> add :CUT content back in... 
		  (append
		   (otlb::string-to-charlist 
		    (sixth (first x)))
		   (second x))) 
	      tablevel)))))

(defun textline-result (obj-seq &optional tablevel)
  (let ((parse-tree (parse-textline-obj-seq obj-seq)))
    ;; return an item
    (list (list :TEXTLINE nil :tablevel tablevel) 
	  parse-tree)))
