(in-package :otl)

(defparameter *TRANSFORM-html*
  '((:FOOTNOTE . otlr::footnote-transform-html)))

(if (assoc :html otlb::*transform-trees*)
    (setf (cdr (assoc :html otlb::*transform-trees*)) (list *transform-html*))
    (push (list :html *transform-html*) otlb::*transform-trees*))



