(in-package :otlp)
;;;
;;; parse-latex.lisp: LaTeX-specific parsing
;;;
(defun latex-special-component? ()
  ;; identify meaningful LaTeX structures 
  ;; tex command:  \texcom[things]{stuff}
  ;; tex special char: \square \Box 
  (pc:mdo
    "\\"
    (pc:<- cmd (pc:atleast? (alpha-char?) 1))
    (pc:<- opts (pc:atmost?
	      (pc:mdo
		#\[ 
		(pc:<- x (pc:atleast? (alpha-char?) 1)) 
		#\]
		(pc:result (cons #\[ (append x '(#\])))))
	      1))
    "{"
    (pc:<- x (pc:many? (not-one-of-chars? '(#\}))))
    "}"
    (pc:result (list 
	     '(:ASIS nil nil) 
	     (cons #\\ (append cmd opts (cons #\{ x) '(#\})))))))