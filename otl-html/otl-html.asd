;;;
;;; otl-html.asd 
;;;
(defsystem otl-html
  :serial t
  :description "HTML rendering for OTL."
  :components ((:file "char")
	       (:file "cite")
	       (:file "footnote")
	       (:file "render-html")
	       (:file "table")
	       (:file "vars-html"))
  :depends-on (:otl 
	       :otl-cite
	       :dfile
	       :dxh
	       :paper-sizes
	       :cl-base64		; math expressions
	       ))
