;;;
;;; otlp.asd
;;;
(defsystem otlp
  :serial t
  :description "OTLP (OTL-PARSE): parse plain text, generating a parse tree"
  :components ((:file "packages")
	       (:file "index")
	       (:file "lexer")
	       (:file "lexer-post")
	       (:file "lex-list")
	       (:file "parse-util-lex") 
	       (:file "parse-bracketed")

	  
	       (:file "parse-char")
	       (:file "parse-chem")
	       (:file "parse-gloss")
	       (:file "parse-image")
	       (:file "parse-latex")
	       (:file "parse-links")
	       (:file "parse-math")
	       (:file "parse-under")
	       (:file "parse-subst")
	       (:file "parse-table")
	       (:file "parse-whitespace-lex")
	       (:file "parse-x")
	       (:file "parse-xml")

	       (:file "parse-lists")	; parse-x
	       (:file "parse-textline") ; uri 

	       (:file "parse")		;parse-x, parse-textline 
	       ;; requires all parse-foo material 
	       (:file "parse-doc"))
  :depends-on (:graylex :otl-base :paper-sizes :parser-combinators))
