(in-package :otlb)
;;;
;;; misc utility variables and functions
;;;
(defparameter latex-special-chars '(#\\ #\& #\% #\$))

;; ID? is a string (see doc/internal-representation/item-spec.txt) which might correspond to a character string representing an ID value
;; Return a sequence of characters or, if impossible to generate a satisfactory ID value, NIL.
(defun acceptable-id (id? &optional default-id)
  "Note: if non-nil, DEFAULT-ID is blindly accepted as an acceptable ID value."
  ;; replace an unacceptable ID value with an acceptable value for an ID attribute for a HTML or DocBook element or for a LaTeX \ref{...} value
  ;; unacceptable HTML or DocBook id attribute values:
  ;; - values containing only whitespace
  ;; - nonunique values [not evaluated here]
  ;; ...
  ;; unacceptable LaTeX id attribute values: ?
  ;; -> avoid all LaTeX special chars
  ;; (let ((id?-string (otl::char-list-to-string id?)))
  (if (or (not id?) 
	  ;; avoid LaTeX special characters
	  (any-of-chars-in-string-p latex-special-chars id?))
      default-id 
      ;; ignore complex content (title can contain complex content, making title->id relationship problematic) 
      ;;(obj-seq->item-id-string id?)
      id?))

(defun otl-dir ()
    (make-pathname
     :directory (append (pathname-directory (user-homedir-pathname)) '(".otl"))))

(defun output-language ()
  ;; keyword - e.g., :HTML, :LATEX, :DOCBOOK, ...
  (first (partree-item-list :out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; char utils (dcu/shared-char.lisp)
;;
(defun collect-chars (obj-seq)
  (declare (list obj-seq))
  (loop for obj in obj-seq
     when (characterp obj)
     collect obj))

(defun whitespacep (char)
  (member char '(#\Newline #\Space #\Tab #\Return #\Linefeed #\Page)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; file utils (dfile)
;;
(defun file-to-stream (pathname stream)
  "PATHNAME is a string or pathname. If PATHNAME points to a file which exists and is readable, send content of file to stream STREAM. If the file does not exist, return nil. Signal an error if file contains unreadable data. Note(s):
- 'readable' currently means the file only contains objects corresponding to lisp characters. Return value undefined."
  (if (dfile:file-or-directory-exists pathname)
      (let ((file-s (open pathname :direction :input)))
	(do ((x (read-char file-s nil :eof) (read-char file-s nil :eof)))
	    ((eq x :eof))
	  (write-char x stream))
	(close file-s))))

(defun file-to-string (pathname)
  "PATHNAME is a string or pathname. If PATHNAME points to a file which exists and is readable, return a string corresponding to the content of the file. If the file does not exist, return nil. Signal an error if file contains unreadable data. Note that 'readable' currently means the file only contains objects corresponding to lisp characters."
  (if (dfile:file-or-directory-exists pathname)
      (with-output-to-string (string-s nil :element-type 'character)
	(file-to-stream pathname string-s))))

(defmacro insert-system-file-as-string (name system &key type)
  (file-to-string (asdf:system-relative-pathname system name :type type)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; function utils (dcu/shared-function.lisp)
;;
(defun funcallablep (x)
  (member (type-of x) '(function symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; list utils (dcu/shared-list.lisp)
;;
(defun aupdate (key value some-alist &key (test #'eq))
  "Add new key/value pair if key not already present in alist SOME-ALIST. If key already present, update corresponding value. Don't modify the original list. Return a copy of the updated alist."
  (if (assoc key some-alist :test test)
      (let ((sa-copy (copy-alist some-alist)))
	(setf (cdr (assoc key sa-copy :test test)) value) 
	sa-copy)
      (progn (push (cons key value) some-alist)
	     some-alist)))

(defun aval (key some-alist &optional (test #'equal))
  "Return the value associated with key KEY in alist SOME-ALIST. If the key KEY isn't present, return NIL. Use EQUAL for the test."
  (let ((acons (assoc key some-alist :test test)))
    (if acons (cdr acons))))

(defun avals (key some-alist &optional (test #'equal))
  (let ((vals nil))
    (map nil
	 #'(lambda (pair)
	     (if (funcall test (car pair) key)
		 (push (cdr pair) vals)))
	 some-alist)
    vals))

(defun avals-deep (keys some-alist &optional (test #'equal))
  ;; recursive approach best?
  (if keys
      (let ((key (pop keys)))
	(avals-deep keys (avals key some-alist test) test))
      some-alist))

(defun remove-nils (some-list)
  "Remove nil values from list SOME-LIST. Return the corresponding list."
  (remove-if-not #'(lambda (x) x) some-list))

(defun map-from-sets-to (x lista listb &optional (test #'eql))
  (loop
     for xa in lista
     for xb in listb
     if (member x xb :test test)
     do (return xa)))

(defun truncate-list-at (l test)
  "See TRUNCATE-SEQUENCE-AT."
  (loop for x in l
     while (not (funcall test x))
     collect x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; stream utils
;;

;; iolib's slurp-stream-into-string
(defun slurp-stream-into-string (stream)
  (with-output-to-string (s)
    (loop :for c := (read-char stream nil nil)
       :while c :do (write-char c s))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; string utils (dcu/shared-string)
;;
(defun cut-regex-match-from-front (string regex)
  (cl-ppcre:regex-replace-all 
   (concatenate 'string "^" regex)
   string
   ""))

(defun empty-string-p (str)
  (do ((n 0 (1+ n)))
      ((eq n (length str))
       t)
    (if (not (whitespacep (elt str n)))
	(return nil))))

(defun noes (x &optional (no-nils-p t))
  "Return T if X is NIL or if it is an empty string (as defined by EMPTY-STRING-P). If NO-NILS-P is true, return T if X is the string 'NIL'"
  (or (not x)
      (empty-string-p x)
      (if no-nils-p 
	  (if (and (stringp x) (string= x "NIL")) t))))

;; note: not in DAT-CL-UTILS -
;; - expects lists to be lists of characters
(defun tostring (x)
  (cond ((stringp x) x)
	((consp x) (charlist-to-string x))
	(t (write-to-string x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; shared-comparison.lisp
;;
(defun not-eql (x y)
  (not (eql x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; shared-sequence..lisp
;;
(defun seq= (seq1 seq2 &optional (test #'eql))
  (let ((s1len (length seq1))
	(s2len (length seq2)))
    (when (= s1len s2len)
      (do ((i 0 (1+ i)))
	  ((= i s1len)
	   t)
	(if (not (funcall test (elt seq1 i) (elt seq2 i)))
	    (return nil))))))

;; FIXME: what is desired behavior for (dcu::seq-of-p "abab" '(#\a #\b)) ? (currently returns NIL)
(defun seq-of-p (seq subseq)
  "Return a true value if sequence sequence SEQ is composed solely of repeats of the elements in sequence SUBSEQ."
  (let ((seq-length (length seq))
	(subseq-length (length subseq)))
    (do ((x 0 (+ x subseq-length)))
	((> (+ x subseq-length) seq-length)
	 nil)
      (if (not (equalp (subseq seq x (+ x subseq-length))
		       subseq))
	  (return nil))
      (if (= (+ subseq-length x) seq-length)
	  (return t)))))

(defun truncate-seq-at (seq test)
  "Return subsequence of SEQ truncated at first position where calling TEST (some funcallable object which accepts a single argument) returns a true value."
  (cond ((consp seq)
	 (truncate-list-at seq test))
	(t
	 (error "Code me..."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; time utils
;;
(defun iso8601-date-string (universal-time &key day mn yr)
  "Return a string, representing a date, with the form 2010-06-07. UNIVERSAL-TIME, an integer, supersedes current time if UNIVERSAL-TIME is non-negative. DAY, MN, YR supersede those values specified by UNIVERSAL-TIME or those associated with the current time."
  (declare (integer universal-time))
  (multiple-value-bind (second minute hour date month year)
      (if (>= universal-time 0) 
	  (decode-universal-time universal-time)
	  (get-decoded-time))
    (declare (ignore second minute hour))
    (let ((d (or day date))
	  (mo (or mn month))
	  (y (or yr year)))
      (format nil "~4,'0D-~2,'0,D-~2,'0,D" y mo d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; uncategorized
;;
(defun not-eql-to-any (x &rest ys)
  (let ((flag t))
    (map nil #'(lambda (y)
		 (if (eql x y)
		     (setf flag nil)))
	 ys)
    flag))
