(in-package :otlr) 

(defun acceptable-image-p (path output-lang)
  "Return a true value if path object PATH is an acceptable image for output language OUTPUT-LANGUAGE."
  (let ((type (pathname-type path))) 
    ;; infer image type from file name suffix (lazy)
    ;; compare image type against list of acceptable types 
    (member type (first (otlb:aval output-lang *render-image* #'eq))
	    :test #'equalp)))