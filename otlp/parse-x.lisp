(in-package :otlp)
;;;
;;; parse-x.lisp: various document component parsers
;;;


;; An admonition is a line composed of whitespace succeeded by text content with the form foo:[some admonition text...] where foo is one of 'hint', 'note', 'warning', or 'tip'.
;; note that the lexer identifies admonitions (lexer.lisp)
(defun admonition? (&optional tablevel)
  ;; upon match, return ((:admonition nil admonition-type) object-sequence) where
  ;; ADMONITION-TYPE is a list of characters
  (pc:mdo (pc:<- x (lline? :admonition tablevel))
	  (pc:result
	   (progn
	     (let* ((raw-lexed (second x))
		    (parsed-lexed 
		     (parser-combinators:tree-of 
		      (parser-combinators:current-result 
		       (pc:parse-string 
			(pc:mdo 
			  (pc:many? (whitespace-except-newline?))
			  ;; FIXME: this should look specifically for type-seq values of 'hint', 'tip', 'warning', or 'note'
			  ;; (pc:<- type (pc:many1?
			  ;; 		  (pc:choices
			  ;; 		   (cons?)
			  ;; 		   (not-chars? (list #\:)))))
			  (pc:<- type (pc:choices
				       "hint"
				       "note"
				       "warning"
				       "tip"))

			  ":["		; ":"
			  (pc:many? (whitespace-except-newline?))
			  (pc:<- admonition-content (pc:many? 
						     (pc:choices
						      (cons?)
						      (not-char? 
						       #\] ; #\Newline
						       ))))
			  (pc:result 
			   (progn
			     (list 
			      type	; TYPE: a list of characters
			      (parse-textline-obj-seq admonition-content)))))
			raw-lexed)))))
	       (list (list :admonition nil :type (first parsed-lexed)) (second parsed-lexed)))))))

(defun asis-inline? ()
  ;; asis can either be 
  ;; 1. a delimited series of lines ('literal?' parser grabs these)
  ;; -> we parse for this [ ? ]
  ;; 2. a series of characters within a single line of text delimited by the 'asis' bracketing tags specified in *PARTREE* with :ASIS
  ;; -> here we parse for possibility #2
  ;; see BRACKETED-PARSER04
  (let ((asis-pair (otlb::partree-item-list :asis)))
    (pc:mdo (first asis-pair)		; ASIS-START
	    (pc:<- x (pc:find-before? 
		      (pc:choices
		       (cons?)
		       (character?)) 
		      (pc:choices
		       (second asis-pair) ; ASIS-END
		       (pc:char? #\Newline)
		       (pc:end?))))
	    (pc:find? (second asis-pair)) ; ASIS-END
	    (pc:result (list '(:LIT) x)))))

;; (in-package :pc)
;; (progn
;;   (tree-of (current-result (parse-string (otlp::citations?)
;; 					    "cite:[yo] cite:[ho] cite:[yep] 123"))))

(defun citations? ()
  ;; For "cite:[yo] cite:[ho] 123", X will be equal to
  ;; ((((:CITE NIL :CID (#\y #\o))) (#\ )) (((:CITE NIL :CID (#\h #\o))) (#\ )))
  (pc:mdo (pc:<- x (pc:atleast?
		    (pc:seq-list?
		     (cite?)
		     (pc:many? (whitespace?)))
		    1))
	  (pc:result (progn
		       (list
			;; item-spec
			(list :citations
			      nil
			      :citations
			      (mapcar #'(lambda (cite-at-car)
					  (first cite-at-car))
				      x)
			      ;; The parser grabs whitespace after each citation. This exposes whitespace after the last citation as it may be significant for a renderer.
			      :trailing-whitespace (second (car (last x)))))))))

(defun cite? ()  
  ;; -> encounter markup specifying a reference; format of markup: CITE:[cid] 
  ;; CID: the unique identifier for a citation, represented as an object sequence
  ;; - in docbook, this corresponds to <citation>cid</citation>
  ;; - in LaTeX, ...
  ;; - not implemented yet for HTML
  
  ;; -> generate corresponding item in parse tree
  ;; ----format of ITEM object: ((:CITE nil cid))
  (pc:mdo "cite:["
       (pc:<- x
	   (pc:atleast?
	    (pc:choices
	     (cons?)
	     (not-one-of-chars? '(#\] #\Newline))) 1))
       #\]
       (pc:result (list (list :CITE nil :cid x)))))

(defun comment-line? ()
  ;; parse for an inline comment (comments are identified during lexing)
  (pc:mdo (pc:<- x (lline? :comment))
       (pc:result (list
		'(:COM)
		x))))

(defun footnote-parsers ()
  ;; footnotes cannot contain: footnotes, ids/linktargets, images, lists, tables, ...
  (append
   (list
    ;; a footnote may contain items (structured content)
    (cons?)
    (asis-inline?)
    (chem-expr?)
    (citations?)
    (glossterm?)
    (indexterm?)
    (math-expr?)
    )
   (*parse*-textline-parsers)
   (list 
    ;; *PARSE*-TEXTLINE-PARSERS may contain other parsers which grab URIs -- let them do their thing first and, as a convenience, catch a plain-vanilla URI here...
    ;; (uri?-otl)
    ;; for smooth integration with other parsers, the SUBST? parser must be called last (only after it is clear that the region doesn't contain a URI or other matter which must be parsed in a distinct manner)
    (subst?)))
  )

(defun footnote? ()
  (pc:mdo "footnote:["
	  (pc:<- x
		 ;; deep recursion up front can slow things down -> save parsing of internal content for a match

		 ;; !! be smart about parsers here -->> doesn't make sense to have a footnote parser within a footnote -->> specify parsers explicitly, eliminating those which are nonsensical -->> would be nice to be able to do something like (parsers :url :asis ...)

		 (textline-obj-seq-parser :parsers (footnote-parsers)
					  :stop-at #\]))
	  #\]
	  (pc:result 
	   (progn
	     (list 
	      ;; :footnote id before-p
	      '(:FOOTNOTE)
	      ;; FIXME: call should be like parse-textline-string but don't look for nested footnotes...
	      (parse-textline-obj-seq x))))))

;; lexer catches line with glossary
(defun glossary? ()
  (pc:mdo (lline? :glossary)
       (pc:result '((:glossary)))))

(defun def-definition? ()
  (pc:mdo (pc:<- x (pc:choices (lline? :nonemptyline 1)
		      (lline? :list 1)))
       (pc:result
	(progn
	  (parse-textline-obj-seq (second x))))))

(defun def-term? ()
  (pc:mdo (pc:<- x (lline? :defterm 0))
       ;; grab all characters up to :-
       (pc:result
	(progn
	  (let ((raw-lexed (second x)))
	    (let ((current-result
		   (parser-combinators:current-result 
		    (pc:parse-string 
		     (pc:mdo 
		       (pc:<- raw-term (pc:atleast? (character?) 1))
		       ":-"
		       (pc:result
			(progn
			  (parse-textline-obj-seq raw-term))))
		     raw-lexed)))) 
	      (if current-result
		  (parser-combinators:tree-of current-result))))))))

(defun definition?-otl ()
  (pc:mdo (pc:<- term (def-term?))
       (pc:<- definition (def-definition?))
       (pc:result 
	(progn 
	  (list (list :DEFINITION nil :term term :definition definition))))))

(defun escaped-line? ()
  (pc:mdo
    (pc:<- lline (lline? :escape))
    ;;(pc:<- llines (find-before? (item) (lline? :escape)))
    ;;(lline? :escape)
    (pc:result 
     (progn
       (list '(:ESC)
	     (llines-to-charlist (list lline)))))))

;; 'literal' (formerly 'asis/NoProcess') text:
;;    - lines between ## are lexed as 'literal' lines
(defun literal? ()
  (pc:mdo
    (lline? :literal 0)
    (pc:<- llines (pc:find-before? (pc:item) (lline? :literal 0)))
    (lline? :literal 0)
    (pc:result 
     (list '(:LIT)
	   (llines-to-charlist llines)))))

;; look for expression of form exprtype:[[..stuff...]] with parser returning ((type-keyword nil) char-seq) where char-seq corresponds to ...stuff...
(defun otl-expr? (expression-type type-keyword)
  "Return an item."
  (pc:mdo (concatenate 'string expression-type ":[[") 
	  (pc:<- foo (pc:find-before? (character?)
				      (pc:choices "]]" (pc:char? #\Newline) (pc:end?))))
	  (pc:find? "]]")
	  (pc:result
	   (progn
	     (list (list type-keyword nil)
		   foo)))))

;; pagebreaks are identified during lexing
(defun pagebreak? ()
  (pc:mdo (lline? :pagebreak)
	  (pc:result '((:pagebreak)))))

#|

preformatted: text which is the equivalent of the HTML 'pre' tag or the LaTeX verbatim environment.
	
If a line begins with ### followed by only whitespace, the tags specified by the "preequivalent" line in the ~/.otl/otl file will be used (by default, a pair of HTML 'pre' tags or a \begin{verbatim} ... \end{verbatim} environment). The first tag will be used at the ###. The next tag will be used at the following ### (note that this ### must also be at the start of a line and succeeded only by whitespace on that line). 

In smart mode with an HTML or TeX document, otl will try to identify and deal with any "problem characters" between the ### strings.

|#

(defun preformatted? ()
  (pc:mdo 
    (lline? :preformatted 0)
    (pc:<- llines (pc:find-before? (pc:item) (lline? :preformatted 0)))
    (lline? :preformatted 0)
    (pc:result 
     (progn
       ;; ! second position should contain object sequence if we want to keep things consistent...
       (list '(:preformatted)
	     (llines-to-charlist llines))))))

(defun *parse*-document-textblock-parser ()
  (let ((parse-pairs (*parse*-pairs :document)))
    (loop
       for pair in parse-pairs
       if (eq (pair-parsertype pair) :textblock)
       do (return (second (pair-parse-spec pair))))))

(defun textblock?-otl (tablevel &optional (type :nonemptyline))
  "TABLEVEL is an integer. TYPE specifies lline type."
  ;; A top-level textblock begins with a tablevel=0 textline (if the first character of a line is the escape character but it isn't part of an escaped set of lines, then the parser may be staring at a lexed line (lline) of type :ESCAPE which, at this point, should be treated as a regular textline
  ;;   - and may be followed with either a tablevel=1 list or a tablevel=1 textblock
  (pc:mdo
    (pc:<- x (pc:choices 
	      (textline? tablevel type)
	      ;; an isolated ctable line shouldn't parse as CTABLE -> treat as regular line
	      ;;   - note that this means that TESTBLOCK? must succeed parsing for :CTABLE lines (is this the best place for this? or should the textblock-otl? container call this?)
	      (textline? tablevel :ctable)))	  

    (pc:<- y 
	;; FIXME: is it possible for multiple lists of textblocks of the same tablevel to succeed the first textline?
	(pc:many?
	 (pc:choices1

	  ;; consider a succeeding textline at the same tab level part of the current textblock
	  (textline? tablevel type) 

	  (pc:mdo
	    ;; indented text block might have a blank space in front of it... 
	    (pc:atmost? (emptyline?) 1)
	    (pc:<- x (textblock?-otl (1+ tablevel) type))
	    (pc:result x))
	  ;; both same tablevel and tablevel +1 are aesthetically intuitive as 'top-level' lists
	  (pc:choice
	   (list? :tablevel tablevel)
	   (list? :tablevel (1+ tablevel)))
	  ;; ??
	  (list? :tablevel (+ 2 tablevel))
	  (catchall? tablevel)
	  (catchall? (1+ tablevel)))))
    (pc:result 
     (progn
       (list 
	(list :TEXTBLOCK nil :tablevel tablevel)
	;; X: a single item
	;; Y: an object sequence
	(cons x y))))))

;;; the TOC marker is identified during lexing
(defun toc? ()
  "Match the table of contents marker indicating location to insert table of contents."
  (pc:mdo (lline? :toc)
	  (pc:result '((:toc)))))

;;;
;;; URIs
;;;

;;; URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
;;; - rfc3986 doesn't exclude double-quote

;; parse a URI
(defun uri? ()
  (pc:mdo (pc:<- scheme (parser-combinators:choices 
			 "http" "https"
			 "data" 
			 "file"
			 "ftp"
			 "imap"
			 "mailto"
			 "nfs"
			 "nntp"
			 "pop"
			 "urn"))
	  #\: 
	  (pc:<- x (pc:atleast? 
		    (pc:choices
		     (cons?)
		     ;; exclude certain chars from URIs here:
		     ;;   " char: want to handle <a href="http://www.freshmeat.net">www.freshmeat.net</a>...) 
		     ;;   ] char: see uris/uris02.txt 
		     (not-chars? (cons #\] (cons #\[ (cons #\" *whitespace-chars*))))) 1))
	  ;; by default, handle URI at the apparent end of a sentence by excluding #\. as a possible last character for a URI
	  (pc:<- lastchar
		 (not-chars? (cons #\. (cons #\] (cons #\[ (cons #\" *whitespace-chars*)))))
		 )
	  ;; #\[
	  ;; (pc:<- z (pc:atmost?
	  ;; 	    (pc:mdo (pc:<- caption (pc:atleast?
	  ;; 				    (pc:choices
	  ;; 				     (cons?)
	  ;; 				     (not-char? #\])) 
	  ;; 				    1)) 
	  ;; 		    (pc:result caption))
	  ;; 	    1))
	  ;; #\]
	  (pc:result
	   (let ((item-specifier-stuff (append x (list lastchar))))
	     (when scheme
	       (push (list scheme item-specifier-stuff) *parse-uris*))
	     (list 
	      (list :URI nil
		    :scheme scheme 
		    :item-specifier-stuff item-specifier-stuff)
	      nil)))))

;; parse a URI in otl markup
(defun uri?-otl ()
  (pc:mdo (pc:<- scheme (parser-combinators:choices 
			 "http" "https"
			 "data" 
			 "file"
			 "ftp"
			 "imap"
			 "mailto"
			 "nfs"
			 "nntp"
			 "pop"
			 "urn"))
	  #\:
	  ;; X: we exclude certain chars from URIs here:
	  ;;   - exclude " char (want to handle <a href="http://www.freshmeat.net">www.freshmeat.net</a>...) 
	  (pc:<- x (pc:atleast? 
		    (pc:choices
		     (cons?)
		     (not-chars? (cons #\[ (cons #\" *whitespace-chars*)))) 1))
	  #\[
	  (pc:<- z (pc:atmost?
		    (pc:mdo (pc:<- caption (pc:atleast?
					    (pc:choices
					     (cons?)
					     (not-char? #\])) 
					    1)) 
			    (pc:result caption))
		    1))
	  #\]
	  (pc:result
	   (progn
	     (when scheme
	       (push (list scheme x) *parse-uris*))
	     (list 
	      (list :URI nil :scheme scheme :item-specifier-stuff x)
	      (parse-textline-obj-seq (first z)))))))
