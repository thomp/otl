;;;
;;; otl-base.asd
;;;
(defsystem otl-base
  :serial t
  :description "OTL-BASE provides tools for interacting with an otl object sequence."
  :components ((:file "packages")
	       (:file "char-seq")
	       (:file "document")
	       (:file "util")
	       (:file "index")
	       (:file "items")
	       (:file "latex")
	       (:file "object-sequences")
	       (:file "partree")
	       (:file "table")
	       (:file "transform")
	       (:file "vars"))
  :depends-on (:cl-ppcre
	       :dfile))
