(in-package :cl-user)

(defpackage :otlp
  (:use :cl)
  (:documentation "Parsing. The program is released under the terms of the Lisp Lesser GNU Public License http://opensource.franz.com/preamble.html, also known as the LLGPL. Copyright: David A. Thompson, 2011-2012")
  (:export parse-file))

;; :PARSER-COMBINATORS is a bit unwieldy
(rename-package :parser-combinators :parser-combinators '(:pc))