(in-package :otlp)
;;;
;;; parse tables
;;;
;;;  - see table.lisp for description of otl representation of table
;;;  - a ctable uses a character, or set of characters, to define boundaries between cells.
;;;
(defun ctable-cell-separator-string ()
  (first (*parse*-ctable-spec)))

(defun ctable-header-string ()
    (second (*parse*-ctable-spec)))

(defun ctable-lex-rules ()
  (if (*parse*-ctable-spec)
      (let ((raw-sep-string (ctable-cell-separator-string)))
	(list
	 (cons (ctable-row-regex raw-sep-string) :CTABLE)
	 (cons (ctable-title-regex raw-sep-string) :CTABLETITLE)))))

(defun ctable-lex-sep-string (raw-sep-string)
  ;; provide a string suitable for treating the cell separator string as a literal rather than as a regex -> escape regex metacharacters in the sep string
  (cl-ppcre:regex-replace-all
   ;; escape vertical pipe if in sep string
   ;; FIXME: also escape \ ^ . $ [ ] { } + * ?
   "\\|" 
   raw-sep-string 
   "\\\\|"))

(defun ctable-row-regex (raw-sep-string)
  "Return a string representing a regex."
    ;; if the separating string is conceptualized as a fixed string (not a regex), need to escape special regex chars
    ;; - e.g., if RAW-SEP-STRING is "|", we actually want to use "\\|"
    (let ((sep-string
	   (ctable-lex-sep-string raw-sep-string)))
      ;; map parameter file specification for a ctable item (list with the form (cell-separator-string header-string) )
      ;; to lex rule for recognizing list item 
      ;; example: for a list item mapping, map 
      ;; ("-" "<li>" "</li>" "<ul style="list-style-type: disc">" "</ul>" :u) to
      ;; "\\t*- .*(\\n|$)"
      (concatenate 'string
		   ;; 1. escape all backslash characters for transition to planet lisp: (cl-ppcre:regex-replace "\\\\" (first parfile-spec) "\\\\\\\\") 
		   ;; 2. since READ is used to read file, characters are escaped in the file itself at this point (and 1 is unneeded)
		   
		   ;; a row is [maybe some non-whitespace stuff] [non-newline whitespace] [cell separator string] [[maybe [ [non-newline whitespace] [maybe some non-whitespace stuff] ]] [a newline]
		   ".*?"
		   "\\s"
		   sep-string 
		   ;; sep-string can be followed by non-newline whitespace...
		   "(\\s.*?)??"
		   ;; sep-string can be followed directly by newline (e.g., two-column table w/empty second cell)
		   "(\\n|$)")))

(defun ctable-title-regex (raw-sep-string)
  ;; if the separating string is conceptualized as a fixed string (not a regex), need to escape special regex chars
  ;; - e.g., if RAW-SEP-STRING is "|", we actually want to use "\\|"
  (let ((sep-string
	 (ctable-lex-sep-string raw-sep-string)))
    ;; map parameter file specification for a ctable item (list with the form (cell-separator-string header-string) )
    ;; to lex rule for recognizing list item 
    ;; example: for a list item mapping, map 
    ;; ("-" "<li>" "</li>" "<ul style="list-style-type: disc">" "</ul>" :u) to
    ;; "\\t*- .*(\\n|$)"
    (concatenate 'string
		 ;; 1. escape all backslash characters for transition to planet lisp: (cl-ppcre:regex-replace "\\\\" (first parfile-spec) "\\\\\\\\") 
		 ;; 2. since READ is used to read file, characters are escaped in the file itself at this point (and 1 is unneeded)
		 
		 ;; a ctable title line is [cell separator string] [cell separator string] [a bunch of stuff]
		 sep-string
		 sep-string
		 ".*?(\\n|$)"
		 )))

;; parsing yields an item with item-spec (:TABLE item-id :key1 val1 ... :keyN valN) where keys include :TITLE :CAPTIONTEXT :CTABLE-SPEC
(defun ctable? ()
  "Match a ctable."
  ;; A ctable is composed of at least MIN-CTABLE-LENGTH consecutive ctable rows
  (let ((min-ctable-length 2)
	;; FIXME: more efficient to pass spec to other parsers rather than having them 're-grab' it?
	(ctable-spec (*parse*-ctable-spec)))
    (cond 
      (ctable-spec
       (pc:mdo 
	 (pc:<- title (pc:atmost? (ctable-title?) 1))
	 (pc:<- captiontext (pc:atmost? (ctable-title?) 1))
	 (pc:<- x (pc:atmost? (ctable-header?) 1))
	 (pc:<- y (pc:atleast? (ctable-row?)
			       min-ctable-length))
	 (pc:result
	  (progn
	    (list (list :table nil
			;; FIXME: once this is set in stone, get rid of (second (first ... -->> revise CTABLE-TITLE? accordingly
			:captiontext (second (first captiontext))
			:ctable-spec ctable-spec
			;; HEADER-ROWS should be nil or a single TCOLLBLS item
			:header-rows (first x)
			:title (second (first title)))
		  y)))))
      (t (warn "CTABLE parser not specified")))))

(defun ctable-internal-cell? (cell-separator-char &optional internal-parser)
  ;; INTERNAL-PARSER isn't used to for actual parse test - it's used on internal content once test succeeds
  (assert (characterp cell-separator-char))
  ;; - if CELL-SEPARATOR-CHAR is NIL, unintended matches occur against all sorts of content -> kludge: specify a default value
  ;; (let ((cell-separator-char (if cell-separator-char
  ;; 				 cell-separator-char
  ;; 				 ≃	; unicode 2243 (or something else obscure...)
  ;; 				 ))) 

  ;; A ctable internal cell is series of characters terminated by CELL-SEPARATOR-CHAR
  (pc:mdo
    ;; grab phrase up to CELL-SEPARATOR-CHAR non-greedily
    (pc:<- x (pc:many? 
	   (pc:choices
	    (cons?)
	    (not-char? cell-separator-char))))
    cell-separator-char
    (pc:result
     (progn
       (if x
	   (progn
	     (list '(:tcell nil)
		   (if internal-parser
		       ;; parse internal text block; return text block if no parse match
		       (parse-charlist x internal-parser)
		       x))))))))

(defun ctable-internal-header-cell? (cell-separator-char header-char)
  ;; like CTABLE-INTERNAL-CELL? but specific for a cell in the last row of a series of 'header rows'
  (assert (characterp cell-separator-char))
  (assert (characterp header-char))

  ;; A ctable internal header cell is series of header-char, succeeded by whitespace, and terminated by CELL-SEPARATOR-CHAR
  (pc:mdo
    ;; grab phrase up to CELL-SEPARATOR-CHAR non-greedily
    (pc:<- x (pc:atleast? (pc:char? header-char) 1))
    (pc:atleast? (pc:whitespace?) 1)
    cell-separator-char
    (pc:result
     (progn
       (if x 
	   :flurp			; return value ignored
	   )))))

(defun ctable-terminal-cell? (&optional internal-parser) 
  ;; INTERNAL-PARSER isn't used for actual parse test - it's used on internal content once test succeeds
  (pc:mdo
    ;; A ctable terminal cell is everything left on the line...
    (pc:<- x (pc:many? 
	   (pc:choices
	    (cons?)
	    (character?))))
    (pc:result
     (progn
       (if x
	   (progn
	     (list '(:tcell nil)
		   (if internal-parser
		       ;; parse internal text block; return text block if no parse match
		       (parse-charlist x internal-parser)
		       x))))))))

(defun ctable-terminal-header-cell? (header-char) 
  ;; like CTABLE-TERMINAL-CELL? but for last row in a series of 'header rows'

  ;; A ctable terminal header cell is a series of header-char, possibly bracketed by whitespace (in original document, terminal header cell should terminate with #\Newline -- no need to match #\Newline here since #\Newline is matched elsewhere)
  (pc:mdo
    (pc:many? (pc:whitespace?))
    (pc:<- x (pc:atleast? (pc:char? header-char) 1))
    (pc:many? (pc:whitespace?))
    (pc:result 
     (if x
	 ;; value ignored
	 :glurp))))

(defun ctable-title? ()
  (pc:mdo (pc:<- x (lline? :ctabletitle nil))
	  (pc:result
	   (progn
	     (let ((raw-lexed (second x)))
	       (let ((parsed-lexed
		      (pc:tree-of
		       (pc:current-result
			(pc:parse-string
			 (let ((cell-sep-string
				(ctable-cell-separator-string)))
			   (pc:mdo (pc:times? cell-sep-string 2)
				   (pc:many? (pc:whitespace?))
				   ;; use MAYBE? to avoid choking if what appears to be an empty title is encountered (see foo00.txt test file)
				   ;; TEXTLINE-STRING-PARSER should return an object sequence
				   ;; ATMOST? returns a list
				   (pc:<- title (pc:atmost? (textline-string-parser) 1))
				   (pc:many? (pc:whitespace?))
				   ;; return the object sequence
				   (pc:result (first title))))
			 raw-lexed)))))
		 (list '(:TTITLE) parsed-lexed)))))))

(defun ctable-header-or-row? ()
    (pc:choices1 
     (ctable-header?)
     (ctable-row?)))

(defun ctable-header? ()
  ;; A ctable header is a series of ctable rows (lines lexed as :CTABLE lines) where the last row (series of cell separators flanked by whitespace) is composed of cells containing only whitespace or content which matches the header string
  (pc:mdo (pc:<- x (pc:atleast? (ctable-row?) 1))
       (ctable-header-under-row?)
       (pc:result 
	(progn
	  (list 
	   '(:tcollbls nil) 
	   x)))))

(defun ctable-header-under-row? ()
  ;; see CTABLE-HEADER-ROW?:
  ;; - the 'header-under-row' is a line which contains only whitespace, matches to the header string and matches to the cell separator regex string
  (let ((cell-sep-string (ctable-cell-separator-string)) 
	(header-string (ctable-header-string)))
    (let ((header-char (elt header-string 0)) 
	  (cell-sep-char (elt cell-sep-string 0)))
      ;; like CTABLE-ROW? except we 
      ;; 1. limit it to cells w/a pre-defined header-string
      ;; 2. don't return a parse tree
      (lline? :ctable nil
	      ;; this type of row is composed of a series of at least two cells, each containing only whitespace and the header-string
	      #'(lambda (char-seq)
		  ;; CHAR-SEQ is a string
		  (let ((result (pc:current-result 
				 (pc:parse-string 
				  (pc:mdo
				    (pc:<- x (pc:atleast? (ctable-internal-header-cell? 
						     cell-sep-char 
						     header-char) 
						    1))
				    (pc:<- y (ctable-terminal-header-cell? header-char))
				    (pc:result 
				     (progn
				       t)))
				  char-seq))))
		    result))))))

(defun ctable-row? ()
  ;; A normal ctable row begins with a line which
  ;; - was lexed as a ctable row (has cell separator regex string preceded and succeeded by whitespace)
  ;; - is not followed by a line which contains only whitespace, matches to the header string and matches to the cell separator regex string
  (let* ((cell-sep-string (ctable-cell-separator-string))
	 ;; only a single character is used to separate cells
	 (cell-sep-char (elt cell-sep-string 0))) 
    ;; a row is composed of a set of cells
    (pc:mdo (pc:<- x (lline? :ctable nil))
	 (pc:result 
	  (progn
	    (let* ((raw-lexed (second x))
		   (parsed-lexed 
		    (parser-combinators:tree-of 
		     (parser-combinators:current-result 
		      (pc:parse-string 
		       (pc:mdo 
			 (pc:<- cells 
			     (pc:atleast? 
			      (ctable-internal-cell? cell-sep-char (textline-string-parser))
			      1))
			 (pc:<- lastcell
			     (ctable-terminal-cell? (textline-string-parser)))
			 (pc:result 
			  (progn
			    (append cells (list lastcell)))))
		       raw-lexed)))))
	      (list '(:trow nil) parsed-lexed)))))))

(defun *parse*-ctable-spec ()
  (first (rest (first (*parse*-pairs :table)))))
