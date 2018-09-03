(in-package :otlr)

(defvar *gloss-filespec* nil "Filespec for glossary entries.")

(defvar *gloss-stack* nil "Stack holding data for glossary entries. Each sequence element is a list with the form (glossterm glossdef) where both GLOSSTERM and GLOSSDEF are represented as object sequences.")

(defun gloss-filespec-generator* ()
  ;; FIXME:
  "Should return NIL or a symbol corresponding to a fn which accepts ?? arguments and which returns ??."
  (first (get-*render2*-value :gloss-filespec-generator)))

(defun fix-glossterm-term (term)
    ;; FIXME: this kludge (cheap rewrite of RENDER-OBJECT-SEQUENCE which ignores indexterm items) isn't ideal since we lose the INDEXTERM information
  (with-output-to-string (s)
    (let ((char-renderer? (get-*render*-value :CHAR)))
      (dolist (obj term)
	(cond ((otlb:itemp obj) ; ignore non-character content of all items... including :INDEXTERM
	       (otlb::top-level-chars-to-stream (otlb::get-item-obj-seq obj) s))
	      ((characterp obj)
	       ;; CHAR-RENDERER fn is different than other render fns in that it only accepts two args
	       (if char-renderer? (funcall char-renderer? obj s)
		   (write-char obj s)))
	      ;; NIL is a valid item -> ignore
	      ((not obj))
	      (t
	       (error (format nil "Unsupported item type: ~S~%" obj))))))))

;; in some cases (e.g., DocBook, HTML, ...) it is desirable to simply render the term
(defun glossterm-x (object-sequence item-id item-kvs stream)
  (declare (ignore object-sequence item-id))
  ;; TERM and DEF are object sequences
  (let ((term (otlb::item-kvs-val :term item-kvs))
	(singular (otlb::item-kvs-val :singular item-kvs))
	(def (otlb::item-kvs-val :def item-kvs)))
    (push (list (or singular term) def) *gloss-stack*)  
    ;; for HTML, nothing is done in text except to render the term itself...
    (render-obj-seq term stream)))
