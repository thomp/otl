(in-package :otlr)
;;;
;;; char.lisp
;;;
(defun lit-char-html (char stream)
  (render-char-xml char stream))

(defun lit-chars-html (chars stream)
  "CHARS is a sequence of characters"
  (declare (sequence chars)
	   (stream stream))
  (map nil #'(lambda (char) (lit-char-html char stream))
       chars))

