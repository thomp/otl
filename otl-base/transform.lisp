(in-package :otlb)
;;;
;;; transform.lisp: transformations of otl parse tree 1 -> otl parse tree 2
;;;
(defvar *transform* nil "Alist like *RENDER*. Values are functions which can transform the parse tree prior to rendering. Accessible to the function are special variables %OBJ-SEQ% and %OBJ-SEQ-POINTER%.")

(defvar *transform-trees* nil)

(defun get-transform-fn (item-key)
  (assoc item-key *transform* :test 'eq))

(defun refresh-*transform* (output-language)
  (assert output-language)
  (let ((transform?
	 (first (aval output-language *transform-trees*))))
    (setf *transform* transform?)))