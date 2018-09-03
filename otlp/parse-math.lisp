(in-package :otlp)
;;;
;;; parse math
;;;

;; markup for a math expression:
;; math:[[<Tex math markup>]]

;; match a math expression
(defun math-expr?-OLD ()
  "Return an item."
  (pc:mdo "math:[[" 
       (pc:<- foo (pc:find-before? (character?)
			     (pc:choices "]]" (pc:char? #\Newline) (pc:end?))))
       (pc:find? "]]")
       (pc:result
	(progn 
	  (list '(:MATH nil)
		foo)))))

(defun math-expr? ()
  "Return an item."
  (otl-expr? "math" :MATH))
