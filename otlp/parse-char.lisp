(in-package :otlp)
;;;
;;; parse characters
;;;
(pc:def-cached-parser alpha-char?
  (pc:sat #'cl:alpha-char-p))

(pc:def-cached-parser character?
  (pc:sat #'characterp))

(pc:def-cached-parser standard-char?
  (pc:sat #'standard-char-p))

(defun chars? ()
  (pc:many? (standard-char?)))

(defun escaped-char?01 ()
  (let ((esc-symbol? 
	 (second (pair-parse-spec (first (*parse*-pairs :escape-char))))))
    (pc:mdo 
      (or esc-symbol? #\SYMBOL_FOR_ESCAPE) ; unicode 'SYMBOL FOR ESCAPE' (␛)
      (pc:<- x (character?))
      (pc:result
       (list '(:ESC) (list x))))))

(pc:def-cached-parser latex-special-character?
  (pc:sat #'latex-special-character-p))

(defun latex-special-character-p (char)
  (member char otlb::latex-special-chars))

(pc:def-cached-arg-parser not-char? (character)
  (pc:sat (parser-combinators::curry #'otlb::not-eql character)))

(pc:def-cached-arg-parser not-chars? (characters)
  (pc:sat #'(lambda (x) (apply 'otlb::not-eql-to-any x characters))))

(defun one-of-chars-p (chars char)
  (member char chars))

(pc:def-cached-arg-parser one-of-chars? (characters)
  (pc:sat (parser-combinators::curry #'one-of-chars-p characters)))

(defun not-one-of-chars-p (chars char)
  (not (one-of-chars-p chars char)))

(pc:def-cached-arg-parser not-one-of-chars? (characters)
  (pc:sat (parser-combinators::curry #'not-one-of-chars-p characters)))

(pc:def-cached-parser xml-special-character?
  (pc:sat #'xml-special-character-p))

(defun xml-special-character-p (char)
  (member char '(#\> #\< #\&)))
