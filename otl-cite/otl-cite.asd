;;;
;;; otlp.asd
;;;
(defsystem otl-cite
  :serial t
  :description "Handle citations/references internally"
  :components ((:file "packages")
	       (:file "bibtex"))
  :depends-on (:bibtex))
