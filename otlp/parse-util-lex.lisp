(in-package :otlp)
;;;
;;; utility parsers
;;;

;;
;; misc
;;

;; rough equivalent of (apply #'choices parser-list)
(defun choices-list (parser-list)
  (when parser-list
    (let ((parser1 (parser-combinators::ensure-parser (first parser-list)))
	  (parser2 (parser-combinators::ensure-parser
		    (choices-list (rest parser-list)))))
      #'(lambda (inp) (parser-combinators::execute-choice inp parser1 parser2)))))

(pc:def-cached-parser cons?
  (pc:sat #'consp))

;; PARSER-COMBINATORS> (tree-of (current-result (parse-string (otlp::items-or-chars-except-char? #\a) "ABCDabcd")))
;; (#\A #\B #\C #\D)
(defun items-or-chars-except-char? (char)
  (pc:atleast?
   (pc:choices
    (cons?)
    (not-char? char)) 1))

(defun parse-charlist (charlist parser)
  ;; PARSER can be a symbol or a parser
  (assert parser)
  (if charlist 
      (let* ((internal-text-block (otlb::charlist-to-string charlist))
	     (parse-result
	      (progn
		(if (symbolp parser)
		    (pc:parse-string (funcall parser) internal-text-block)
		    (pc:parse-string parser internal-text-block))))
	     (current-result
	      (parser-combinators:current-result parse-result)))
	(if current-result
	    (parser-combinators:tree-of current-result)
	    charlist))))

(defun parse-llines (llines parser)
  (let ((parse-result (parse-vector parser llines)))
    (let ((current-result (parser-combinators:current-result parse-result)))
      (if current-result
	  (parser-combinators:tree-of current-result)))))

(defun parse-obj-seq (obj-seq parser)
  ;; PARSER can be a symbol or a parser
  ;; OBJ-SEQ is composed of characters or ASIS items
  (assert parser)
  (if obj-seq
      (let* ((parse-result
	      (progn
		(if (symbolp parser)
		    (parser-combinators:parse-string (funcall parser) obj-seq)
		    (parser-combinators:parse-string parser obj-seq))))
	     (current-result
	      (parser-combinators:current-result parse-result)))
	(if current-result
	    (parser-combinators:tree-of current-result)
	    obj-seq))))

;;
;; numeric
;;
(pc:def-cached-parser numeral?
  (pc:sat #'numeral-p))

(defun numeral-p (char)
  (member
   char
   (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(defun positive-integer? ()
   (pc:atleast? (numeral?) 1))

;; FIXME: this should call lline?-core
(pc:def-cached-arg-parser lline? (line-type &optional tablevel content-test type-spec-test)
  "Parse a representation, post-lexing, of a line. Test whether the line type matches LINE-TYPE (a keyword). If TABLEVEL is true, test whether that tab level of the line matches TABLEVEL, an integer representing the number of tab characters at the start of the line).

CONTENT-TEST is an arbitrary test on the content (second component of the line). This is a function which is handed a single argument, a sequence of characters, and returns NIL or T.

TYPE-SPEC-TEST is a funcallable object or NIL. If a funcallable object, TYPE-SPEC-TEST is called with the first member of the line."
  ;; lexed line is represented by (linetype linecontent) where LINETYPE is a keyword and LINECONTENT is an object sequence composed of characters and/or asis items
  (pc:sat #'(lambda (x)
	      (let ((obj-seq (second x)))
		(and
		 (eq (lline-type x) line-type)
		 (if tablevel
		     (if (eq line-type :list)
			 (or (eq (car (last (first x))) tablevel)  ; this appears to always evaluate to NIL these days (tab count appears to no longer be embedded as an integer in the first member of X)
			     (tablevel-p obj-seq tablevel))
			 (tablevel-p obj-seq tablevel))
		     t)
		 (if content-test
		     (funcall content-test obj-seq)
		     t)
		 (if type-spec-test 
		     (funcall type-spec-test (first x))
		     t))))))

(pc:def-cached-arg-parser lline?-core (&optional content-test tablevel type-spec-test)
  "Parse a representation, post-lexing, of a line. If TABLEVEL is true, test whether that tab level of the line matches TABLEVEL, an integer representing the number of tab characters at the start of the line).

CONTENT-TEST is an arbitrary test on the content (second component of the line). This is a function which is handed a single argument, a sequence of characters, and returns NIL or T."
  ;; lexed line is represented by (linetype linecontent) where LINETYPE is a keyword and LINECONTENT is an object sequence composed of characters and/or asis items
  (pc:sat #'(lambda (x)
	      (let ((obj-seq (second x)))
		(and
		 (if tablevel
		     (progn 
		      ;; a lexed line can specify tablevel in one of two ways:
		      ;; 1. list items currently specify last value as an integer representing # of tabs preceding list designator
		      ;; 2. other items retain tabs in object sequence
		       
		       ;; FIXME: need util fns for this sort of thing --> too bug-prone otherwise...
		       (if (and (consp (first x)) (eq (first (first x)) :list))
			   (eq (car (last (first x))) tablevel)
			   (tablevel-p obj-seq tablevel)))
		     t)
		 (if content-test
		     (funcall content-test obj-seq)
		     t)
		 (if type-spec-test 
		     (funcall type-spec-test (first x))
		     t))))))

(defun lline-of? (some-string line-type &optional tablevel)
  (declare (keyword line-type))
  (lline?
   line-type
   tablevel
   #'(lambda (char-seq)
       (progn
	 (otlb::char-seq-of-p
	  ;; FIXME: kludge: CHAR-SEQ may have #\Newline at end due to lexing
	  (otlb::truncate-seq-at
	   char-seq
	   #'(lambda (x) (eq x #\Newline)))
	  (progn
	    (otlb::string-to-charlist some-string)))))))

(defun tablevel (obj-seq)
  "Return an integer corresponding to the number of tab characters at the start of sequence OBJ-SEQ."
  (declare (sequence obj-seq))
  (if (and obj-seq (not (= (length obj-seq) 0)))
      ;; non-zero-length object sequence
      (let ((count 0) 
	    (index 0))
	(do ((x (elt obj-seq index) (elt obj-seq index)))
	    ((or (>= count (length obj-seq)) 
		 (not (characterp x))
		 (not (char= x #\Tab)))
	     count)
	  (if (char= x #\Tab)
	      (incf count))
	  (incf index)))
      0))

(defun tablevel-p (obj-seq tablevel)
  ;; does object sequence OBJ-SEQ begin with neither more nor less than TABLEVEL (an integer) tabs?
  (declare (fixnum tablevel))
  (or
   ;; handle NIL obj-seq
   (and (not obj-seq)
	(= 0 (length obj-seq)))
   ;; non-NIL obj-seq
   (and
    ;; there is no guarantee that the sequence is even as long as TABLEVEL...
    (>= (length obj-seq) tablevel)
    (equal
     (subseq obj-seq 0 tablevel)
     (make-list tablevel :initial-element #\Tab))
    ;; OBJ-SEQ may contain other items apart from characters
    (and (characterp (elt obj-seq tablevel))
	 (not
	  (eq #\Tab
	      (elt obj-seq tablevel)))))))

;;;
;;; specialty
;;;
(defmacro at-least-n-of-x-maybe-with-y-interspersed (n x y)
  "Match at least N of X (a parser) where Y may be interspersed between instances of X."
  `(pc:seq-list?
     ,@(at-least-n-of-x-helper n x y)))

;; (otlp::at-least-n-of-x-helper 3 #\a #\b)
(defun at-least-n-of-x-helper (n x y &optional (count 1))
  (cons
   (pc:atleast? x 1)
   (if (= count n)
       (list (pc:many? y))
       (cons (pc:many? y) 
	     (at-least-n-of-x-helper n x y (1+ count))))))
