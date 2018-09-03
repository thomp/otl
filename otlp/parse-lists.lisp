(in-package :otlp)
;;;
;;; parse-lists.lisp: parse lists
;;;

;; see textblock?-otl (parse-x.lisp) -- note that part of the definition of a list is embedded in this definition: a list must (1) have a tablevel greater than that of the immediately preceding content or (2) be preceded by a blank line

;;
;; we expect the lexer to generate, for a true list item, a sequence with the form (<type-spec> <char-seq>) where
;; <char-seq> is the original, unadulterated character sequence of the line and
;; <type-spec> has the form
;; ((:LIST <??> <order-type> <symbol-type> :CUT "1." :REST " digestion" :TABS 1) (#\Tab #\1 #\. #\  #\d #\i #\g #\e #\s #\t #\i #\o #\n #\Newline))

;; example:
;; ((:LIST "	1. digestion" :CUT "1." :REST " digestion" :TABS 1) (#\Tab #\1 #\. #\  #\d #\i #\g #\e #\s #\t #\i #\o #\n #\Newline))
;;

(defun list? (&key (tablevel 0))
  "Match a list where all members of the list are at a single tablevel TABLEVEL, an integer."
  ;; a list is either ordered or unordered
  (pc:choices1 
   (list-of-type? :ordered tablevel) 
   (list-of-type? :unordered tablevel)))

(defun list-item? (order-type tablevel &optional symbol-type)
  "ORDER-TYPE is either :ORDERED or :UNORDERED. See LIST?."
  (declare (fixnum tablevel)
	   (keyword order-type))
  ;; A list item begins with a line which
  ;; - has the correct tablevel 
  ;; - has a list designator succeeded by some non-whitespace content
  ;; The line may be succeeded by
  ;; - a textblock with a greater tab level
  ;; - a list with a greater tab level

  ;; X example:
  ;; ((:LIST "[0-9]+\\." :ORDERED :NUMERIC) NIL)
  (pc:mdo (pc:<- x (lline? :list
			   tablevel
			   nil
			   #'(lambda (type-spec)
			       (and 
				(eq (third type-spec) order-type)
				(if symbol-type
				    (eq (fourth type-spec) symbol-type)
				    t)))))
	  (pc:<- y (pc:many? (pc:choices 
			      (admonition? (1+ tablevel))
			      ;; FIXME: not generic
			      (textblock?-otl (1+ tablevel))
			      (list? :tablevel (1+ tablevel))
			      ;; catch orphan list lines (see LIST-OF-TYPE? MIN-LIST-LENGTH)
			      (catchall? (1+ tablevel)))))
	  (pc:result
	   (progn
	     ;; same principal as parse-under-lex: only single line accep	  
	     (let* ((rest (eighth (first x)))
		    (parsed-lexed
		     ;; an object sequence containing a :TEXTLINE object
		     (list (list '(:TEXTLINE)
				 (parse-textline-obj-seq 
				  rest ; (second x)
				  )))))
	       ;; a list item
	       (list 
		;; the list item item-spec
		(list :LISTITEM
		      nil 
		      :tablevel tablevel 
		      ;; regex shouldn't be needed 
		      :order-type (third (first x)) ; order-type
		      :symbol-type (fourth (first x)) ; symbol-type
		      ;; specify what was cut
		      :cut (sixth (first x))
		      :rest rest)
		(if y
		    (append parsed-lexed y)
		    parsed-lexed)))))))

(defun list-item-designator?-hardwired (type)
  (cond ((eq type :ordered)
	 (pc:mdo (positive-integer?) 
		 (pc:char? #\.)))
	((eq type :unordered)
	 (pc:char? #\-))
	(t (error "Unsupported LIST ITEM type."))))

(defun list-item-designator?-from-param-file (type)
  (cond ((eq type :ordered)
	 (pc:mdo (positive-integer?) 
	      (pc:char? #\.)))
	((eq type :unordered)
	  (pc:char? #\-))
	(t (error "Unsupported LIST ITEM type."))))

(defun list-of-type? (order-type tablevel)
  "See LIST?."
  ;; A list is composed of at least MIN-LIST-LENGTH consecutive list items of the same type and tablevel
  (let ((min-list-length 2))
    (pc:mdo (pc:<- x (pc:atleast?
		      ;; FIXME: all list items should have the same SYMBOL-TYPE 
		      (list-item? order-type tablevel) min-list-length))
	    (pc:result 
	     (progn
	       ;; FIXME: is there a good reason to lose SYMBOL-TYPE information here?
	       (list (list :list nil order-type)
		     x))))))
