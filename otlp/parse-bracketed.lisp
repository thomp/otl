(in-package :otlp)
;;
;; parsers for bracketed objects
;;
(defun bracketed? (&rest parsers)
  "A parser in parser must return, as the second object, a sequence of characters. Corresponding parse tree has form ((:BRACKETED bstart bend) ...)"
  (let ((bparsers (if parsers parsers (default-bracketed-parsers))))
    (pc:mdo (pc:<- r01
	     (choices-list bparsers))
	 (pc:result 
	  (progn r01)))))

(defun bracketed-parser-nongreedy (bstart bend &key internal-parser-symbol item-spec)
  "BSTART is a string. BEND is a string. Everything between BSTART and BEND is parsed externally. Return result with the form ((:BRACKETED bstart-sequence) internal-text-sequence). If INTERNAL-PARSER is non-nil, parse internal text block."
  ;; look immediately for an instance of bstart
  (pc:mdo bstart
	  ;; grab everything between bstart and bend non-greedily
	  ;; definition: a bracketed phrase is a sequence of characters which succeeds bstart, precedes bend, and does not include a #\Newline
	  ;; note: FIND-BEFORE? doesn't parse through BEND
	  (pc:<- x (pc:find-before?
		    (pc:choices
		     (cons?)
		     (character?))
		    (pc:choices
		     bend
		     (pc:char? #\Newline)
		     (pc:end?))))
	  ;; 1. find-before*:        (pc:<- x (find-before* (character?) bend))
	  ;; 2. set two contexts at bstart and bend - any easy way to grab stuff in between?
	  (pc:find? bend)
	  ;; return (:bracketed bstart-sequence ...stuff-from-parsing-between-bstart-and-bend)
	  (pc:result
	   (progn
	     (list (or item-spec (list :bracketed nil :bstart bstart :bend bend))
		   (if internal-parser-symbol
		       ;; parse internal text block; return text block if no parse match
		       ;; FIXME: why not PARSE-TEXTLINE-OBJ-SEQ ?
		       (parse-obj-seq x internal-parser-symbol)
		       x))))))

(defun bracketed-parser-greedy (bstart bend &optional internal-parser-symbol)
  "BSTART is a string. BEND is a string. Everything between BSTART and BEND is parsed externally. Return result with the form ((:BRACKETED bstart-sequence) internal-text-sequence). If INTERNAL-PARSER is non-nil, parse internal text block."
  ;; look immediately for an instance of bstart
  ;; grab everything between bstart and bend (greedily)
  ;; definition: a bracketed phrase is a sequence of characters which succeeds bstart, precedes bend, and does not include a #\Newline
  (pc:mdo (pc:<- x 
	   (pc:bracket? bstart
		     (pc:atleast?
		      (pc:choices
		       (cons?)
		       (character?))
		      1)
		     bend))
       ;; return (:bracketed bstart-sequence ...stuff-from-parsing-between-bstart-and-bend)
       (pc:result
	(progn
	  (list (list :bracketed nil bstart bend) 
		(if internal-parser-symbol
		    ;; parse internal text block; return text block if no parse match
		    (parse-obj-seq x internal-parser-symbol)
		    x))))))

(defun bracketed-parsers (bracket-pairs)
  "Return list of parsers. BRACKET-PAIRS is a list of conses."
  ;; nonsensical -- but also a nonissue -- to match with internal parser: if match is against **joe**, then it will be impossible to ever match **foo** inside joe...
  (let ((default-internal-text-parser-symbol
	 ;; this must be the symbol - otherwise it's an infinite recursion (...could use lazy eval framework...)
	 'textline-string-parser))
    (mapcar #'(lambda (bpair)
		(let ((bstart (car bpair))
		      (bend (cdr bpair)))
		  (bracketed-parser-nongreedy 
		   bstart bend 
		   :internal-parser-symbol default-internal-text-parser-symbol)))
	    bracket-pairs)))

(defun bracketed-parsers-excluding? (bracket-pair)
  (let ((bracket-pairs (remove bracket-pair (bracket-pairs) :test #'equalp)))
    (choices-list (bracketed-parsers bracket-pairs))))
    
(defun bracket-pairs ()
  "Return a list of conses. Each cons holds bracketing start and end values used for parsing."
  (error "no longer used?")
  (let ((accum nil)
	(bracketed-tree (otlb:partree-item-list :bracket)))
    (map nil 
	 ;; each sublist should be of the form (bstart bend mupstart mupend)
	 #'(lambda (bstart-bend-mupstart-mupend)
	     ;; simple symmetric bracketing (bstart=bend)
	     ;;(push (cons (first bstart-mupstart-mupend) (first bstart-mupstart-mupend)) accum)
	     ;; support asymmetric bracketing (bstart not necessarily equal to bend)
	     (push (cons (first bstart-bend-mupstart-mupend) 
			 (second bstart-bend-mupstart-mupend)) 
		   accum))
	 bracketed-tree)
    accum))

;; tree where each member has form (bstart (bend1 markup1s markup1e) (bend2 markup2s markup2e) ...)
(defun bracket-pairs-complex-tree ()
  "Return as conses bracketing start and end values used for parsing."
  (error "no longer used?")
  (let ((accum nil)
	(bracketed-tree (otlb:partree-item-list :bracket)))
    (map nil 
	 #'(lambda (bstart-list)
	     (map nil #'(lambda (bend-list)
			  (push (cons (first bstart-list) (first bend-list)) accum))
		  (rest bstart-list)))
	 bracketed-tree)
    accum))

(defun default-bracketed-parsers ()
  (bracketed-parsers (bracket-pairs)))

;; PARSE-SPEC and INTERNAL-SPEC as described in *PARSE* documentation
;; INTERNAL-PARSER-SYMBOL: must be a symbol for a parser - otherwise infinite recursion (...could use lazy eval framework...)
(defun pair->bracketed-parser (parse-spec internal-spec &optional (internal-parser-symbol 'textline-string-parser))
   (let ((bstart (second parse-spec))
	 (bend (third parse-spec)))
     (bracketed-parser-nongreedy 
      bstart bend
      :internal-parser-symbol internal-parser-symbol
      :item-spec (if (keywordp internal-spec)
		     (list internal-spec)
		     internal-spec))))

(defun *parse*-textline-bracket-pairs ()
  (loop 
     for pair in (*parse*-pairs :textline)
     if (eq (pair-parsertype pair) :bracket)
     collect pair))
