(in-package :otlp)
;;;
;;; parse XML
;;;
(defun xml-tag? ()
  ;; tag types: <joe ...> </joe ...> <joe />
  ;; - tag name is composed of letters, underscores, and/or colons

  ;; definition for a start tag: '<' Name (S  Attribute)* S? '>'
  ;; definition of a Name: (Letter | '_' | ':') (NameChar)*

  (pc:seq-list?
   #\<					; (times? #\< 1)
   (pc:many? #\/)
   (pc:atleast?
    (pc:choices
     (alpha-char?)
     #\_
     #\:)
    1)
   ;; (NameChar)*
   (pc:many? (pc:choices (alpha-char?) (pc:digit?) #\. #\- #\_ #\: 
		   ;; FIXME
		   ;;(CombiningChar?)
		   ;;(Extender?)
		   ))
   (pc:choices
    #\>
    (pc:seq-list?
     (pc:atleast? (pc:whitespace?) 1)
     (pc:atleast? (not-chars? '(#\>)) 1)
     #\>))))


(defun xml-special-component? ()
  ;; identify meaningful XML structures 
  ;;   <foo>
  ;;   &flurp;
  (pc:mdo
    "<"
    (pc:<- tag (pc:atleast? (alpha-char?) 1)) 
    (pc:<- stuff (pc:many? (not-one-of-chars? '(#\>))))
    ">"
    (pc:result (list 
	     '(:ASIS nil nil) 
	     (cons #\< (append tag stuff '(#\> )))))))