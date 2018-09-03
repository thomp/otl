(in-package :otlb)
;;
;; index.lisp: tools for generating an index
;;


;; file named 'indexterms' 
;; - in the directory of the source file being processed
;; - file contains a series of words or phrases to be indexed where each word or phrase is on a separate line
(defun read-indexterms (dir)
  "Returns NIL if the indexterms file does not exist."
  ;; expect a file 'indexterms' in directory DIR (the directory component of a path object)
  ;; return a set containing index terms
  (let ((indexfile "indexterms"))
    (let ((indexfile-path (merge-pathnames indexfile (make-pathname :directory dir))))
      (when (dfile:file-exists-p indexfile-path)
	(dfile:lines-in-file indexfile-path)))))
