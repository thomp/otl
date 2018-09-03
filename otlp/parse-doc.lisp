(in-package :otlp)
;;;
;;; parse top-level component
;;;
(defun backmatter-otl () 
  (list 
   '((:FOOTNOTES nil nil))
   ;; consider throwing in a glossary at the end
   (if explicit-glossary-p 
       '((:GLOSSARY nil nil)))))

(defun document? (&key author date-created date-last-modified (paper-size-nickname otlb::*paper-size-nickname-default*) title)
  "Return an item. See PARSE-DOCUMENT for documentation of arguments."
  (let ((item-spec-kv-pairs nil)
	(paper-size? (clps::nickname->paper-size paper-size-nickname))) 
    (setf item-spec-kv-pairs
	  (cons :author (cons author item-spec-kv-pairs))) 
    (setf item-spec-kv-pairs
	  (cons :date-created (cons date-created item-spec-kv-pairs)))
    (setf item-spec-kv-pairs
	  (cons :date-last-modified (cons date-last-modified item-spec-kv-pairs))) 
    (setf item-spec-kv-pairs
	  (cons :page-size (cons paper-size? item-spec-kv-pairs)))
    (setf item-spec-kv-pairs
	  (cons :title (cons title item-spec-kv-pairs)))
    (pc:mdo (pc:<- x (pc:atleast? (document-component?) 1))
	    (pc:result
	     (progn
	       (list (cons :document (cons nil item-spec-kv-pairs))
		     (append '(((:DOCTOP)))
			     x		; parse-tree
			     ;; add backmatter (appendices glossary endnotes bibliography-or-references index afterword colophon) here automatically if flags are set
			     (backmatter-otl)
			     '(((:DOCBOTTOM))))))))))

(defun document-component? ()
  "Look for a single document component. Return an item."
  ;; each component of an otl document is a line or a set of lines
  (choices-list
   (append (list
	    (escaped-line?)
	    (literal?)
	    (comment-line?)
	    (preformatted?)
	    (toc?))
	   (otlb::remove-nils (*parse*-document-parsers))
	   (list 
	    ;; (ctable?)
	    ;; (admonition?)
	    ;; (glossary?)
	    ;; (pagebreak?)
	    ;; ;; tablevel +1 is aesthetically intuitive as a 'top-level' list
	    ;;(list? :tablevel 0)
	    ;; (list? :tablevel 1)
	    

	    ;; FIXME: see parse.lisp *PARSE-OTL* definition -- textblock?-otl doesn't seem to work there so we test it here...
	    ;; stuff above must precede text block
	    (textblock?-otl 0 :nonemptyline) 
	    

	    
	    ;; catch anything which fell through the cracks
	    (catchall?)


     ;; the only other thing that should get matched is a line of only whitespace or trailing whitespace after another object
     ;; match any blank lines floating around - grab any whitespace up to a newline
     ;; - whitespace-except-tab-to-newline ; avoid tabs? 
     (emptyline?)))			; match an empty line
   ))


;; FIXME: indiscriminate use of this is asking for problems -- any time something is caught here, figure out why it didn't get caught by one of the other parsers
(defun catchall? (&optional tablevel)  
  ;; - this is similar to TEXTBLOCK-OTL? -> TEXTLINE? but indiscriminate -->> probably would be better to modify and use a generalized form of TEXTBLOCK-OTL?/TEXTLINE?
  (pc:mdo
    (pc:<- x
	   ;; catch any lines except empty lines
	   ;; Y: the item from the lexing which matched 
	   (pc:mdo (pc:<- y (lline?-core nil
					 tablevel
					 #'(lambda (type-spec)
					     (not (eq type-spec :emptyline)))))
		   (pc:result 
		    ;; if TYPE is :LIST, line is treated as a regular line
		    (textline-result (second y) nil)))) 
    (pc:result 
     (list 
      (list :TEXTBLOCK nil :tablevel tablevel)
      ;; X: a single item
      (list x)))))

(defun parse-vector (parser vector)
  "VECTOR is a sequence of llines. Return a PARSER-COMBINATORS PARSE-RESULT object."
  ;; handle escaped characters and escaped lines
  ;; NEWVECTOR: vector of lists; second item in each list is an object sequence containing only characters and/or asis items
  (let ((newvector
	 (map 'vector
	      #'(lambda (line)
		  (let ((lline-type (elt line 0)))
		    ;; don't process escaped lines further except to remove escape character
		    (cond ((eq lline-type :ESCAPE)
			   (list lline-type
				 (subseq (elt line 1) 1)))
			  (t
			   (list lline-type
				 (parse-for-escaped-chars (elt line 1)))))))
	      vector)))

    ;; then parse for remainder of outl items
    (parser-combinators:parse-string parser newvector)))

;; parse-document.lex.01 - parse after lexing line-by-line
(defun parse-document (string &key (author "") (date-created 0) (date-last-modified 0) (paper-size-nickname otlb::*paper-size-nickname-default*) (title ""))
  "Parse a complete document, represented by string STRING. Return the complete parse tree. See also OTL:PARSE-AND-RENDER-FILE. DATE-CREATED and DATE-LAST-MODIFIED are integers representing Common Lisp universal times. PAPER-SIZE-NICKNAME is a CLPS keyword."
  (declare (special %output-lang%)
	   (integer date-created date-last-modified)
	   (keyword paper-size-nickname)
	   (string author string title))
  (parse-string string :author author :date-created date-created :date-last-modified date-last-modified :paper-size-nickname paper-size-nickname :title title))

(defun parse-file (infile &key (author "") (date-created 0) (date-last-modified 0) (encoding :utf-8) (paper-size-nickname otlb::*paper-size-nickname-default*) (title ""))
  "See also REFRESH-*PARSE*. Note that it is expected that files are UTF-8 encoded. If a file is not UTF-8 encoded, encoding should specify the encoding."
  ;; sanity checks
  (cond ((not (dfile:file-or-directory-exists infile))
	 (error "File doesn't exist."))
	((not *parse*)
	 (warn "*PARSE* does not appear to have been set. Consider using PARSE-RESET."))
	(t   
	 (let ((instring (dfile:file-to-string infile :external-format encoding)))
	   ;; return parse tree
	   (parse-document instring 
			   :author author 
			   :date-created date-created
			   :date-last-modified date-last-modified
			   :paper-size-nickname paper-size-nickname :title title)))))

(defun parse-for-escaped-chars (string)
  "Parse string STRING, looking for escaped characters. Return an object sequence containing only characters and single-character :ESC items."
  (multiple-value-bind (parse-result context-result) 
      (parser-combinators:parse-string (parse-for-escaped-chars-parser) string)
    (declare (ignore context-result)) 
    (let ((current-result (parser-combinators:current-result parse-result)))
      (let ((parse-tree 
	     (if current-result
		 (parser-combinators:tree-of current-result))))
	parse-tree))))

(defun parse-for-escaped-chars-parser ()
  (pc:many?
   (pc:choices (escaped-char?01)
	       (pc:item))))
