(in-package :otlp)
;;
;; parsers for whitespace
;;
(defun emptyline? ()
  "Match lexed representation of a line containing only whitespace."
  (pc:mdo (lline? :emptyline)
	  (pc:result '((:emptyline)))))



(defun tab? () 
  (pc:char? #\Tab))

(defun tabs? ()
  (pc:many? (tab?)))

(defvar *whitespace-chars*
  '(#\Newline #\Space #\Tab #\Return #\Linefeed #\Page))

(defun whitespacep (char)
  (member char *whitespace-chars* :test #'char=))

(pc:def-cached-parser whitespace?
  (pc:sat #'whitespacep))

(defun non-whitespace-p (char)
  (not (whitespacep char)))

(pc:def-cached-parser non-whitespace? 
  (pc:sat #'non-whitespace-p))

(defun non-whitespace-or-cons-p (x)
  (or (consp x)
      (non-whitespace-p x)))

(pc:def-cached-parser non-whitespace-or-cons? 
  (pc:sat #'non-whitespace-or-cons-p))

(defun whitespace-except-newline? ()
  (one-of-chars? 
   ;; #\Newline varies with platform (might be CR, CRLF, or LF)
   '(#\Tab #\Space #\Return #\Page)))

(defun whitespace-to-newline? ()
  "Match/consume whitespace, if any, up to, and including, a newline."
  (pc:mdo (pc:<- x (pc:many? (whitespace-except-newline?)))
	  (pc:result 
	   (progn
	     x))))
