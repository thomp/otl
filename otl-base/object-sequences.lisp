(in-package :otlb)
;;;
;;; object-sequences
;;;
(defun current-obj (obj-seq pos)
  "Accessor."
  (nth pos obj-seq))

(defun elt-with-item-key (object-seq item-key)
  ;; get element with specified item-key
  (dolist (element object-seq)
    ;; check if object sequence element is an item
    (if (consp element)
	(if (eq (get-item-key element) item-key)
	    (return element)))))

(defun elts-with-item-key (object-seq item-key)
  ;; get elements with specified item-key
  (let ((accum))
    (dolist (element object-seq)
      (if (consp element)
	  (if (eq (get-item-key element) item-key)
	      (push element accum))))
    (reverse accum)))

(defun merge-obj-seqs (obj-seqs)
  "Return a new object sequence as the concatenation of object sequences in list OBJ-SEQS."
  (apply #'append obj-seqs))

(defun next-obj (obj-seq pos &optional test-until)
  "Accessor. Given current position POS (an integer), return next object in OBJ-SEQ. TEST is a function; if non-nil, look for the first item succeeding POS which yields a true value when subjected to test TEST."
  (if test-until
      (if (>= pos (1- (length obj-seq)))
	  nil
	  (let ((item (next-obj obj-seq pos nil)))
	    (if (funcall test-until item)
		item
		(next-obj obj-seq (1+ pos) test-until)))) 
      ;; simply return next item, if present
      (unless (>= pos (1- (length obj-seq)))
	(nth (1+ pos) obj-seq))))

(defun prev-obj (obj-seq pos)
  "Accessor. POS represents current position in sequence."
  (unless (< pos 1)
    (nth (1- pos) obj-seq)))

(defun top-level-chars-to-stream (obj-seq stream)
  (declare (list obj-seq)
	   (stream stream)) 
  (dolist (x obj-seq)
    (if (characterp x) (write-char x stream))))

;;generate string from top-level character content of an object sequence
(defun top-level-chars-to-string (obj-seq)
  "Return a string."
  (declare (list obj-seq))
  (with-output-to-string (s)
    (top-level-chars-to-stream obj-seq s)))

