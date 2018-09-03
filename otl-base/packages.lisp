(in-package :cl-user)

(defpackage :otl-base
  (:use :cl)
  (:nicknames :otlb)
  (:documentation "The code is released under the terms of the Lisp Lesser GNU Public License http://opensource.franz.com/preamble.html, also known as the LLGPL. Copyright: David A. Thompson, 2011-2012")
  (:export
   *default-parfile*
   *partrees*
   aupdate
   aval
   charlist-to-stream
   charlist-to-string
   elt-with-item-key
   elts-with-item-key
   get-item-id
   get-item-key
   get-item-obj-seq
   itemp
   next-obj
   output-language
   partree-item-list
   prev-obj
   print-table-otl
   refresh-partree
   set-item-id
   table-otl))

