(in-package :otlp)
;;;
;;; parse chemistry expression
;;;

;; markup for a chem expression:
;; chem:[[<Tex chem markup>]]

;; FIXME: this is a common idiom -- see math-expr? -> consider common code
;; match a chem expression
(defun chem-expr?-OLD ()
  "Return an item."
  (pc:mdo "chem:[[" 
	  (pc:<- foo (pc:find-before? (character?)
				   (pc:choices "]]" (pc:char? #\Newline) (pc:end?))))
	  (pc:find? "]]")
	  (pc:result
	   (progn 
	     (list '(:CHEM nil)
		   foo)))))

(defun chem-expr? ()
  (otl-expr? "chem" :CHEM))
