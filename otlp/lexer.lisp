(in-package :otlp)
;;
;; lexer.lisp: lex document line-by-line
;;
(defun escaped-line-lex-rule () 
  (let ((escape-char? 
	 (second (pair-parse-spec (first (*parse*-pairs :escape-char))))))
    (if escape-char?
	(cons (concatenate 'string
			   (string escape-char?)
			   ".*?(\\n|$)")
	      :escape))))

;; use this instead of LEX-STRING
(defgeneric lex-it (x))

(defmethod lex-it ((x string))
  (with-input-from-string (is x) 
    (lex-it is)))

(defmethod lex-it ((x stream))
  (lex-otl (otl-lexer x) :vector-p t))

(defun lex-otl (lexer &key outs vector-p)
  "INS and OUTS are input and output streams, respectively."
  (let ((out-vector (if vector-p (make-array 5 :fill-pointer 0))))
    (do* ((token (multiple-value-list (graylex:stream-read-token lexer))
		 (multiple-value-list (graylex:stream-read-token lexer)))
	  (class (car token) (car token))
	  ;(image (cadr token) (cadr token))
	  )
	 ((null class))
      (cond (outs
	     (write token :stream outs))
	    (vector-p
	     (vector-push-extend token out-vector))))
    (if vector-p out-vector)))

;; see docs for notes on rule set
(defun lex-ruleset ()
  (otlb::remove-nils
   (let ((comment-rule
	  (let ((comment-line-string 
		 (second (pair-parse-spec (first (*parse*-pairs :comment))))))
	    (if comment-line-string
		(cons (concatenate 'string
				   comment-line-string 
				   ".*?(\\n|$)")
		      :comment))))
	 (ctable-rules (ctable-lex-rules))
	 (other-lex-rules 
	  (append
	   (*parse*-line-lex-rules)
	   (*parse*-linesgroup-lex-rules)))
	 (last-rules
	  (list
	   (cons "[\\t\\r\\f ]*(\\n|$)" :emptyline)
	   (cons "<<<+\\s*?(\\n|$)" :pagebreak) ; FIXME: this should be specified in the partree rather than hardwired
	   
	   ;; match DocBook admonitions
	   ;; - a line with an admonition is recognized by the lexer if it consists of whitespace succeeded by the admonition 'cue' ('hint:', 'tip:', ...) followed by the admonition text
	   ;;(cons "\\s*?(hint:|note:|tip:|warning:).*(\\n|$)" :admonition)

	   ;; - a line with an admonition is recognized by the lexer if it consists of whitespace succeeded by the admonition 'cue' followed by a left square bracket followed by the admonition text followed by a right square bracket
	   (cons "\\s*?(hint:|note:|tip:|warning:)\\[.*\\](\\n|$)" :admonition)

	   ;; this should succeed any other non-empty line (under-line, pagebreak, ...)
	   (cons ".*[^\\s].*(\\n|$)" :nonemptyline)
	   ;;(cons ".*?" :catchall)
	   )))
     (let ((other-rules
	    (append
	     (list
	      ;; \\s*? - allow trailing whitespace on lines
	      ;; (\\n|$) - match end of each line -- including the last line
	      (cons "###\\w*?\\s*?(\\n|$)" :preformatted)
	      (cons "##\\w?\\s*?(\\n|$)" :literal)
	      ;; catch these next
	      (cons "\\[\\[glossary\\]\\]\\w?\\s*?(\\n|$)" :glossary)
	      (cons "\\[\\[TOC\\]\\]\\w?\\s*?(\\n|$)" :toc)
	      (cons ".*?:-\\s*?(\\n|$)" :defterm))
	     (list-lex-rules))))
       (append
	;; catch X before Y because [ why? ]
	(list (escaped-line-lex-rule) comment-rule)
	;; (under-lex-rules)
	ctable-rules
	other-rules
	other-lex-rules
	last-rules)))))

(defun otl-lexer (input-stream) 
  (make-instance 'graylex:lexer-input-stream :stream input-stream :rules (lex-ruleset)))

(defun pprint-lex-ruleset ()
  (pprint (lex-ruleset)))

;;
;; lexer-related util functions
;;
(defun llines-to-charlist (llines)
  ;; ignore lline types and just merge all character data into a single list
  (let ((out-seq nil)
	(llines-max-index (1- (length llines))))
    ;; start with last lline and work up since using push with a list
    (do ((llines-index llines-max-index (1- llines-index)))
	((< llines-index 0))
      (let ((lline (nth llines-index llines)))
	(let* ((string-content (second lline))
	       (str-content-len (length string-content)))
	  (do (				;(x 0 (1+ x)
	       (x (1- str-content-len) (1- x))
	       )
	      ((< x 0)
					;(>= x str-content-len)
	       )
	    (push (elt string-content x) out-seq)))))
    out-seq))

(defun llines-to-char-vector (llines)
  ;; ignore lline types and just merge all character data into a single list
  (let ((out-vector (make-array 4 :fill-pointer 0)))
    (dolist (lline llines)
      (let* ((string-content (second lline))
	     (str-content-len (length string-content)))
	(do ((x 0 (1+ x)
	      ;(1- str-content-len) (1- x)
		))
	    (;(< x 0)
	     (>= x str-content-len))
	  ;; VECTOR-PUSH pushes onto end of vector... (fill pointer mark)
	  (vector-push (elt string-content x) out-vector))))
    out-vector))

  ;; if :TABS is set, use that value, otherwise count Tab chars
(defun lline-tablevel (lline)
  "Always return an integer. An empty line has a tablevel of 0."
  (declare (sequence lline))
  ;; FIXME: code is dangerous - no guarantee :TABS is present; no guarantee first member of lline is a sequence; ...
  (cond ((consp (first lline))
	 (let ((tabs-index? (position :tabs (first lline))))
	   (if tabs-index?
	       (progn
		 (assert (> (length (first lline)) tabs-index?))
		 (nth (1+ tabs-index?) (first lline)))
	       (tablevel (second lline)))))
	((keywordp (first lline))
	 (tablevel (second lline)))
	(t
	 (error "Why did we end up here?"))
	))

(defun lline-type (lline)
  (let ((type-spec (first lline)))
    (if (consp type-spec)
	(first type-spec)
      type-spec)))

(defun lline-type-supplemental (lline)
  (let ((type-spec (first lline)))
    (if (consp type-spec)
	(second type-spec)
      nil)))

(defun lline-text (lline)
  (second lline))

(defun *parse*-line-lex-rules ()
  (mapcar #'(lambda (pair)
	      (let ((parse-spec (pair-parse-spec pair))
		    (internal-spec (pair-internal-spec pair)))
		(let ((parse-type (first parse-spec)))
		  (cond 
		    ((eq parse-type :exact)
		     (cons 
		      (concatenate 'string 
				   (second parse-spec)
				   "(\\n|$)") 
		      (first internal-spec)))
		    (t (error "Unsupported LEXREGEX PARSE-SPEC in *PARSE*"))))))
	  (*parse*-pairs :line)))

(defun *parse*-linesgroup-lex-rules ()
  (mapcar #'(lambda (pair)
	      (let ((parse-spec (pair-parse-spec pair))
		    (internal-spec (pair-internal-spec pair)))
		(let ((parse-type (first parse-spec)))
		  (cond 
		    ((eq parse-type :beginswith)
		     (cons 
		      (concatenate 'string 
				   (second parse-spec)
				   ".*?(\\n|$)") 
		      (first internal-spec)))
		    (t (error "Unsupported LINESGROUP PARSE-SPEC in *PARSE*")) 
		    ))))
	  (*parse*-linesgroup-pairs)))

(defun *parse*-linesgroup-pairs ()
  (*parse*-pairs :linesgroup))
