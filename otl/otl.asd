;;;
;;; otl.asd
;;;
(defsystem otl
  :serial t
  :description "Functions integrating parsing and rendering."
  :components ((:file "packages") 
	       (:file "otl"))
  :depends-on (:dfile
	       :otlr
	       :otlp
	       :unix-options		; otl.lisp
	       ))
