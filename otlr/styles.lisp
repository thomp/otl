(in-package :otlr)
;;
;; styles.lisp: support for associating a 'style' with the rendered document
;;

(defvar *default-style* t
  "If T, use embedded default. If a string, use the style file with that name.")

;; expect stylesheets to be found in *STYLES-DIRPATH*
(defvar *styles-dirpath* 
  nil	   ; (make-pathname :directory '(:absolute "my" "style" "dir"))
  "A pathname corresponding to a directory containing style files.")

;; support CSS stylesheets by default
(defvar *styles-suffix* 
  "css"
  "A string which corresponds to a pathname type.")

(defun default-stylepath ()
  "Return default stylepath (by definition, file with name specified by *DEFAULT-STYLE* (if a string), with suffix *STYLES-SUFFIX*, in the default styles directory *STYLES-DIRPATH*)."
  (if (stringp *default-style*) 
      (stylepath *default-style*)))

(defun default-stylepaths ()
  "Return all pathnames associated with style files in default styles directory."  
  (dfile:filepaths-in-dirpath (styles-dirpath) :type *styles-suffix*))

(defun pprint-styles (stream)
  ;; write to stream STREAM unless STREAM is NIL; if STREAM is NIL, return a string
  (if stream
      (pprint-styles-core stream)
      (with-output-to-string (s) (pprint-styles-core s))))

(defun pprint-styles-core (stream)
  ;; - identify files in *styles-dir* with style suffix
  ;; - the style name is just the file name sans suffix 
  (map nil #'(lambda (x) 
	       (write-string (pathname-name x) stream)
	       (write-char #\Newline stream))
       (default-stylepaths)))

(defun style-string (style)
  (dfile:file-to-string (stylepath style)))

(defun stylepath (style)
  (merge-pathnames
   (make-pathname :name style :type *styles-suffix*)
   (styles-dirpath)))

(defun styles-dirpath ()
  "Return a pathname corresponding to the directory containing style files. The default directory, ~/.otl, is used if *STYLES-DIRPATH* is NIL."
  (the pathname
    (or *styles-dirpath*
	(otlb::otl-dir))))
