(in-package :otlb)
;;;
;;; character sequence support
;;;


;;;;;;;;;;;;;;;;;;;;;;;;
;;; character lists

(defun charlist-to-stream (l s)
  "Send send characters in list of characters L to stream S."
  (dolist (c l)
    (write-char c s)))

(defun charlist-to-string (l)
  "Given list L containing characters, return corresponding string."
  (with-output-to-string (s)
    (charlist-to-stream l s)))

(defgeneric to-charlist (x))

(defmethod to-charlist ((x string))
  (string-to-charlist x))

(defmethod to-charlist ((x null))
  nil)

;; DCU/SHARED-STRING
(defun string-to-charlist (some-string)
  (declare (string some-string))
  (map 'list (lambda (x) x) some-string))



;;;;;;;;;;;;;;;;;;;;;;;;
;;; character sequences

(defun any-of-chars-in-seq-p (chars seq)
  (declare (list chars)
	   (sequence seq))
  (find-if
   #'(lambda (char)
       (declare (character char))
       (find char seq :test #'char=))
   chars))

(defun any-of-chars-in-list-p (chars s)
  (declare (list chars s))
  (find-if
   #'(lambda (char)
       (declare (character char))
       (find char s :test #'char=))
   chars))

(defun any-of-chars-in-string-p (chars str)
  (declare (string str)
	   (list chars))
  (find-if
   #'(lambda (char)
       (declare (character char))
       (find char str :test #'char=))
   chars))

;; not in DCU/shared-string.lisp
(defun char-seq-of-p (char-seq subseq)
  "Return a true value if CHAR-SEQ is composed solely of repeats of the elements in sequence SUBSEQ."
  (seq-of-p char-seq subseq))

(defun char-seq-to-stream (char-seq s)
  (let ((char-seq-length (length char-seq)))
    (do ((x 0 (1+ x)))
	((>= x char-seq-length))
      (write-char (elt char-seq x) s))))

(defun char-seq-to-string (char-seq)
  (with-output-to-string (s)
    (char-seq-to-stream char-seq s)))