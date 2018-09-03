(in-package :cl-user)

(defpackage :otlr
  (:use :cl)
  (:documentation "Rendering. Provides OBJ-SEQ-TO-STRING. The program is released under the terms of the Lisp Lesser GNU Public License http://opensource.franz.com/preamble.html, also known as the LLGPL. Copyright: David A. Thompson, 2011-2012")
  (:export
   *styles-dirpath*
   document-to-string
   obj-seq-to-string
   render-document
   render-to-string))