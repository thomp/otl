(in-package :otlp)
;;;
;;; parse glossary entry (term/definition)
;;;

;; markup:
;; gloss:[someterm|singular\definition of someterm]


;; how to know if glossary should be included in document? approaches (not necessarily mutually exclusive):
;; 1. set flag if glossary terms are detected -> if flag set, automatically generate glossary at appropriate point when rendering
;; 2. allow user to specify glossary position in otl markup

(defvar explicit-glossary-p nil
  "If a true value, add a glossary component to the end of the parse tree.")

(defun glossterm? ()
  ;; item-spec: (:gloss nil term definition singular)
  ;; SINGULAR: singular term (optional)
  "Return an item."
  (pc:mdo "gloss:[" 
       ;; match someterm|singular\definition
       (pc:<- foo (gloss-internal-parser?))
       #\]
       (pc:result
	(progn 
	  (list (list :GLOSSTERM
		      nil ; ID 
		      :term (parse-textline-obj-seq (first foo))	; term - an object sequence
		      :singular (parse-textline-obj-seq (second foo))	; singular of term (optional)
		      :def (parse-textline-obj-seq (third foo)) 	; definition of term
		      ))))))

(defun gloss-internal-parser? ()
  "Return list of form (term singular definition)"
  ;; TERM is an object list composed of characters and/or :asis objects
  (pc:mdo (pc:<- term (pc:atleast?
		 (pc:choices
		  (cons?)
		  (not-chars? '(#\\ #\|))) 
		 1))
       (pc:<- singular 
	   (pc:atmost?
	    (pc:mdo "|"
		 (pc:<- x (pc:atleast?
			(pc:choices
			 (cons?)
			 (not-chars? '(#\Newline #\\ #\]))) 1))
		 (pc:result x))
	    1))
       (pc:<- definition 
	   (pc:atmost? (pc:mdo "\\"
			 (pc:<- x (pc:atleast?
				(pc:choices
				 (cons?)
				 (not-chars? '(#\Newline #\]))) 1))
			 (pc:result x))
		    1))
       (pc:result 
	(progn
	  (let ((raw-term term)
		(raw-singular (first singular))
		(raw-definition (first definition)))
	    (list raw-term raw-singular raw-definition))))))
