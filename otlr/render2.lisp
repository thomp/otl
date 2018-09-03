(in-package :otlr)
;;;
;;; render2.lisp: access/manipulate *RENDER2* and *RENDER2-SET*
;;;

;; alist more flexible than structure (other slots can be easily added)
(defvar *render2* nil
  "An alist which describes other customizable functionality (apart from the mapping of item type to function described by *render*) used in generating output.")

(defvar *render2-set* nil
  "An alist of render2 alists. Each key is a partree keyword (see *PARTREES*).")

(defun add-to-*render2-set* (output-spec render2-val)
    (if (assoc output-spec *render2-set*)
	(setf (cdr (assoc output-spec *render2-set*))
	      (list render2-val))
	(push (list output-spec render2-val) *render2-set*)))

(defun get-*render2-set*-value (output-spec)
  (assoc output-spec *render2-set*))

(defun get-*render2*-value (key)
  (otlb:aval key *RENDER2* #'eq))

(defun set-*render2* (&optional output-spec)
  (let ((output-language (or output-spec
			     (otlb::output-language))))
    (let ((render2?
	   (first (otlb:aval output-language *render2-set*))))
      (if render2?
	  (setf *render2* render2?)
	  (error (format nil "*RENDER2-SET* doesn't contain a match for language specified by ~A" output-language))))))
