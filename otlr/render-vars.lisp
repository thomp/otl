(in-package :otlr)
;;;
;;; render-vars.lisp: variables
;;;
(defvar *render* nil 
  "An alist which maps item type to the function which should be used to render a parse tree item of that type. Each key/value pair is composed of a keyword (corresponding to the item type) and a funcallable object (which can be used to render the appropriate component of the output.)")

(defvar *render-set* nil
  "An alist of render trees. Each key is an OUTPUT-SPEC value (e.g., :HTML, :DOCBOOK, :LATEX, ...). Each value is an alist which describes the function which should be used for a given parse tree item based on the item type. Each 'render tree' is an alist where the car is a keyword and the cdr is a symbol pointing at function or the function itself.")

(defun add-to-*render-set* (output-spec render-val)
    (if (assoc output-spec *render-set*)
	(setf (cdr (assoc output-spec *render-set*))
	      (list render-val))
	(push (list output-spec render-val) *render-set*)))

(defun get-*render-set*-value (output-spec)
  (assoc output-spec *render-set*))

(defun get-*render*-value (key)
  (otlb:aval key *RENDER* #'eq))

(defun output-specs ()
  "Return a list of valid OUTPUT-SPEC values given the current value of *RENDER-SET*."
  (mapcar #'(lambda (x) (first x))
	  *render-set*))

(defun set-*render* (&optional output-spec)
  (let ((output-language (or output-spec
			     (otlb::output-language))))
    (let ((render?
	   (first (otlb:aval output-language *render-set*))))
      (if render?
	  (setf *render* render?)
	  (error (format nil "*RENDER-SET* doesn't contain a match for language specified by ~A" output-language))))))


(defvar *render-image* nil "An alist. Each key is a partree keyword. The corresponding value is a list where each member is either a string or NIL. Each string should correspond to a specific image type which is acceptable for use with the output markup type.")
