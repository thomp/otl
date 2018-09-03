(in-package :otlp)

;; TODO:
;;  # only need to index one term per page so, on a single line, probably safe just to deal with one match


;; FIXME/TODO:

;; issues:
;; - indexterms should only be flagged if in 'regular' content (want to avoid accidentally recognizing an index term in an attribute of an xml element, etc...)
;;     #     example: figure:[this is a foo]... 
;;     #         - if 'foo' is an index term, it may later end up as an id value for the figure
;;     #           -> don't want to fiddle with foo in figure:[...]
;;     #           -> maybe just generalize... don't fiddle with foo in goo:[foo]

;; # make sure the index term isn't in a context we leave alone
;; # 1. foo:[... indexterm ...] is troublesome... (can end up in an attribute)
;; #    regex: look for :[ ... indexterm ... ]
;; #      :\[
;; #      [^[]*? - FIXME: should also eliminate newlines here
;; #      [^\]]*?  - FIXME: should eliminate newlines here
;; #      \]
;; # 2. << ...indexterm... >> is troublesome... (can end up in an attribute)
;; # 3. goo:foo is troublesome if goo is tip, hint, warning, ...

(defun indexterm? ()
  ;; match term to be included in index of document
  (declare (special %indexterms%))
  ;; what types of matches qualify as matching "foo"?
  ;; 1. $indexterm[\s\W]
  ;;     - succeeded by a comma, period, or other punctuation
  ;;     - succeeded by any sort of whitespace (newline, etc.)
  ;; 2. m/ /i
  ;;     - capitalized or lower-case
  ;; 3. plural version of term

  ;; better to ask which matches don't qualify as matching foo?
  ;; - 'foo' followed by any letter except an 's' followed by either punctuation of whitespace
  (pc:mdo (pc:<- x (choices-list %indexterms%))
       (pc:result
	(progn
	  ;; X is a string
	  ;;(format t "indexterm: X: ~S~%" x)
	  (list '(:INDEXTERM nil) (otlb::string-to-charlist x))))))