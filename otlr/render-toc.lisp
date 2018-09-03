(in-package :otlr)
;;;
;;; render-toc
;;;

#|

How to approach identifying the TOC? Current implementation:
- see render-x.lisp/UNDER-X -> any 'under' item is associated with a 'priority' and is a potential candidate for inclusion in the table of contents


How to approach generating the TOC? Possibilities include:

1. (A) TOC content recognized during pre-rendering pass through content (see RENDER-OBJECT-SEQUENCE and *TRANSFORM*) and (B) request for TOC noted during pre-rendering pass through content
- advantages = doesn't rely on another rendering functionality outside the *RENDER* tree
- disadvantages = ?

2. CURRENT IMPLEMENTATION: TOC content rendered during rendering pass through parse tree and TOC itself rendered post-hoc and inserted into string
- first pass: (1) recognize/catch all items which should be in the TOC and (2) mark location where TOC should be
    notes:
    - entire document tree must be walked in order not to miss anything... 
    - note location/index of character in output sequence when/where TOC should be inserted
- second pass: generate TOC at appropriate location

|#


(defparameter *toc-store* nil
  ;; store as (text . level) pairs where level is 0 for a major subsection (h1), 1 for a minor subsection (h2), 2 for an even more minor subsection (h3), etc.

  ;; items are pushed onto the stack as encountered during rendering (see render-under.lisp)
    "As values which should be registered in table of contents are encountered, ?.")

(defparameter *toc-index* nil "Note the position where the TOC should be inserted.")


;; FIXME: * to distinguish from former TOC-GENERATOR (which should be renamed as GENERATE-TOC?)
(defun toc-generator* ()
  "Should return NIL or a symbol corresponding to a fn which accepts no arguments and which returns a string representing the table of contents in the appropriate markup language."
  (first (get-*render2*-value :toc-generator)))

;; called on first rendering pass; used if markup language lacks complete support for table-of-contents generation
(defun toc-x (object-sequence item-id item-kvs stream)
  (declare (ignore object-sequence item-id item-kvs))
  ;; The TOC can't be rendered on the first pass through the parse tree since information regarding the entire document isn't available yet.
  ;; so we note the location where the TOC should be...   ... and only render later with (render-toc2...)
  (setf *toc-index* (file-position stream)))

(defun insert-toc (instring index)
  "Insert table of contents at position specified by INDEX. Return corresponding string."
  (let ((n-onwards (subseq instring index))
	(up-to-n (subseq instring 0 index)))
    (concatenate 'string up-to-n 
		 (toc-generator)
		 n-onwards)))

(defun toc-generator ()
  "Return the string representation of the table of contents."
  (if (toc-generator*) (funcall (toc-generator*))))
