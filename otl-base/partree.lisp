(in-package :otlb)
;;;
;;; partree (parameter tree): a substitution table for parsing
;;;
(defparameter *default-parfile-dir*
  (make-pathname :directory '(:absolute "home" "foo" ".otl"))
  "Pathname corresponding to the default directory to search for parameter files.")

(defparameter *default-parfile*
  (make-pathname :defaults *default-parfile-dir* :name "otl2-html"))

(defvar *partree* nil "The current partree. An alist specifying, for a given output-language, some of the user-defined parameters used in parsing and rendering. See otl-html/vars.lisp for detailed documentation of each item type.")

(defvar *partrees* nil "A collection of partrees where each partree is associated with a specific output type (a keyword). Current implementation: hold partrees in a list where each partree is associated with a keyword label (i.e., a list where each member has the form (output-type partree) and where OUTPUT-TYPE is a keyword such as :HTML or :TEXT.")

(defun partree-item-list (key)
  (aval key *partree*))

#|
If we choose to read the parameter file in this manner, then we have to accept lisp-isms up-front... backslash characters need to be escaped appropriately along with double-quote characters, etc...
|#
(defun read-parfile (path)
  (let ((params nil))
    (with-open-file (s path :direction :input)
      (do ((sexp (read s nil :eof) (read s nil :eof)))
	  ((eq sexp :eof))
	(push sexp params)))
    params))

(defun refresh-parfile (&optional file)
  (setf *partree* 
	(read-parfile (if file file *default-parfile*))))

(defun refresh-partree (&optional output-spec)
  "Refresh *partree* based on OUTPUT-SPEC (a keyword or a path or a pathspec)"
  (cond ((or (stringp output-spec) (pathnamep output-spec))
	 (refresh-parfile output-spec))
	((keywordp output-spec)
	 (let ((partree? (second (assoc output-spec *partrees*))))
	   (assert partree?)
	   (setf *partree* partree?)))
	(t
	 (error "Unrecognized OUTPUT-SPEC type."))))

(defun set-*partree* (&optional x)
  (setf *partree* 
	(cond ((consp x)
	       x)
	      ((pathnamep x)
	       (read-parfile x))
	      (t
	       (read-parfile *default-parfile*)))))

;; SET-PARTREE-ITEM returns a modified copy of PARTREE but does not modify the original PARTREE
(defun set-partree-item (partree key &rest values)
  (aupdate key values partree))

(defun string-to-output-spec (output-spec-string)
  (declare (string output-spec-string))
  (if (char= (elt output-spec-string 0) #\:)
      (read-from-string output-spec-string)
      output-spec-string))

(defun string-to-paper-size-nickname (page-size-string)
  (declare (string page-size-string))
  (if (not (char= (elt page-size-string 0) #\:))
      (setf page-size-string (concatenate 'string ":" page-size-string)))
  (read-from-string page-size-string))

(defun under-tree ()
  (otlb:partree-item-list :under))

(defun update-*partrees* (output-type partree)
  (if (assoc output-type otlb:*partrees*)
      (setf (cdr (assoc output-type otlb:*partrees*)) (list partree))
      (push (list output-type partree) otlb:*partrees*)))
