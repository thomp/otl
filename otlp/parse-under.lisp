(in-package :otlp)
;;;
;;; parse under
;;;

;; PARSE-SPEC: example: (:under "=" 1) 
;; INTERNAL-SPEC: example: (:SECTHEAD nil :SIGN 1)
(defun pair->under-parser (parse-spec internal-spec)
  (let ((under-string (second parse-spec)))
    ;; UNDER-CONTEXT-LEX is an object sequence (#\h #\e #\a #\d #\i #\n #\g)
    (pc:mdo (pc:<- under-content-lex
		   ;; textblock? can consist of many lines - this should be only a **single line**
		   (pc:choices (textline? 0)
			       (lline? :list 0)))
	    ;; Y: an under item -- e.g., (:UNDER (#\= #\= #\= #\= #\Newline))
	    (pc:<- y
		   ;; line composed solely of repeats of the under string
		   (lline-of? under-string :nonemptyline))
	    ;; lexer leaves #\Newline on the ends of the lexed line objects 
	    (pc:many? #\Newline)
	    (pc:result
	     ;; RAW-UNDER-STRING is actual string used in doc
	     ;; UNDER-SUBTYPE: string at first position of a list in the :UNDER component of partree
	     (progn
	       (list	    	     
		(otlb::set-item-spec-id 
		 (internal-spec-as-item-spec internal-spec) 
		 (pop *container-id*)) 
		;; return object seq at second position
	     
		;; a list item doesn't really make sense here -> treat it as a textline
		(if (otlb::itemp under-content-lex :list)
		    (otlb::get-item-obj-seq under-content-lex)
		    (list under-content-lex))))))))

(defun under-strings ()
  (mapcar #'(lambda (x) (first x)) (otlb::under-tree)))

(defun under-lex-rules ()
  (mapcar #'(lambda (under-string)
	      ;; consider under string "----"
	      ;; shouldn't match:
	      ;; ----|-----|----
	      ;; should match (including last line of document):
	      ;; ----
	      ;; --------
	      ;; should handle trailing whitespace: \\s*
	      (cons (concatenate 'string under-string "{4,}\\s*?(\\n|$)") :under))
	  (under-strings)))
