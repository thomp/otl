;;;
;;; otlr.asd
;;;
(defsystem otlr
  :serial t
  :description "OTLR (OTL-RENDER): transform a sequence of otl objects to a string"
  :components ((:file "packages")
	       (:file "images")
	       (:file "styles") 
	       (:file "render-vars")
	       (:file "render2")
	       (:file "render-footnote")
	       (:file "render-gloss")
	       (:file "render-list")
	       (:file "render-toc")
	       (:file "render") 	; requires render-footnote, render-gloss, render-toc
	       (:file "render-x")
	       (:file "render-bracketed")
	       (:file "table"))
  :depends-on (:otl-base))
