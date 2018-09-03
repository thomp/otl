(in-package :otlr)
;;;
;;; render-footnote: framework for footnote rendering
;;;
(defvar *footnote-stack* nil "Stack holding footnotes, each represented as an object sequence.")

(defvar *footnote-style* :numeric "Keyword describing footnote style to use (one of :numeric, [...]).")

(defun render-footnote-text (text stream)	 
  (render-obj-seq text stream))


;; if language known to lack footnote support, user must explicitly request footnotes in order for actual footnote content to be rendered (it won't automatically be added to the backmatter)

;; some languages which support automatic footnote generation (e.g., for LaTeX, :FOOTNOTES is mapped to NIL instead of a function)

;; if a document contains footnotes and *RENDER* lacks a specification for rendering footnotes, it is user-friendly to either (1) warn the user, indicating *RENDER* lacks a specification for rendering footnotes (currently, RENDER-ITEM-CORE will signal an error) or (2) warn and then fall back to a generic footnote-rendering funtionality

(defun footnotes-x (object-sequence item-id item-kvs stream)
  "Intended for rendering with a markup language which lacks footnote support."
  (declare (ignore object-sequence item-id item-kvs))
  (let ((fs-length (length *footnote-stack*)))
    (textblock-x 
     (otlb::string-to-charlist "Footnotes")
			       nil		; item-id
			       0 			; tablevel
			       stream)
    (do ((i 0 (+ i 1)))
	((>= i (length *footnote-stack*)))
      (let* ((footnote-pos-in-stack (1- (- fs-length i)))
	     (footnote-number (1+ i))	; number from 1
	     (footnote (nth footnote-pos-in-stack *footnote-stack*)))
	     (cond ((eq *footnote-style* :numeric)
		    ;; enclose in simple container
		    (textblock-x 
		     (append (otlb::string-to-charlist
			      (format nil "~A. " footnote-number))
			     footnote)
		     nil		; item-id
		     0 			; tablevel
		     stream)) 
		   (t (error "Unsupported footnote style (RENDER-FOOTNOTES-TEXT)")))))))