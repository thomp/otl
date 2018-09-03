(in-package :otlp)
;;;
;;; lex-list.lisp: lex list items
;;;
(defun list-lex-rules ()
  (mapcar #'(lambda (parse-pair) 
	      ;; LIST-SYMBOL-REGEX matches against the symbol at the start of a line in a list
	      (let ((list-symbol-regex (second (pair-parse-spec parse-pair))))
		(cons (list-symbol-regex->lex-regex list-symbol-regex)
		      ;; in first stage of lexing, we only identify potential list items (considering each item in isolation); :LIST? is used to identify such a potential list item
		      (cons :list?
			    ;; FIXME: is it necessary to include the symbol regex here?
			    (cons list-symbol-regex
				  (pair-internal-spec parse-pair))))))
	  (*parse*-pairs :list)))

(defun list-symbol-regex->lex-regex (list-symbol-regex)
  ;; map parameter file specification for a list item a la ("-" "<li>" "</li>" "<ul style="list-style-type: disc">" "</ul>" :u)
  ;; to lex rule for recognizing list item a la "\\t*- .*(\\n|$)"

  ;; a line is a list item if (1) it is one of multiple consecutive lines of this form (not analyzed at lexing level) and (2) it has the form
  ;; [ zero or more tabs ] [ list item string ] [maybe [ single space ] [ . (anything except newline/end-of-line) ] ]  [ end-of-line or newline ]
  ;; - note: important to look for whitespace or newline ([maybe [ single space ] ...) - otherwise match line beginning with 3.141597

  
  (concatenate 'string 
	       ;; STRIP-LIST-ITEM-DESIGNATOR relies on this
	       "\\t*" 
	       ;; 1. escape all backslash characters for transition to planet lisp: (cl-ppcre:regex-replace "\\\\" (first parfile-spec) "\\\\\\\\") 
	       ;; 2. since READ is used to read file, characters are escaped in the file itself at this point (and 1 is unneeded)
	       list-symbol-regex
	       ;; don't enforce trailing space -- so that one can compose lists where items are empty " .*\\s*?(\\n|$)"
	       ;;"( \\S*?\\s*?)?(\\n|$)"
	       "( .*?)?(\\n|$)"
	       ))