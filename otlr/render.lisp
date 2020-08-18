(in-package :otlr)
;;;
;;; render
;;;
(define-condition unsupported-item-type (error)
  ((item-key :initarg :item-key :initform nil :reader item-keyread)
   (item :initarg :item :initform nil :reader itemread)
   (key-fn :initarg :key-fn :initform nil :reader key-fnread))
  (:documentation "This should be signalled if a request is made to render an item but support for that item type is not present.")
  )

(defparameter *pre-render-hooks* nil "List of funcallable objects, each of which has the capacity to not accept any arguments. Each member of the list should be called before the start of rendering.")

(defun document-to-stream (document output-spec stream &key glossfile glossp ignore-errors-p out-dir output-subtype style)
  "Return a string. IGNORE-ERRORS-P is either T or NIL. DOCUMENT should represent a complete otl document. OUTPUT-SPEC is one of :HTML, :LATEX, :TEXT, or a path or pathspec pointing to a parameter file."
  (declare (optimize (debug 3) (safety 3)) 
	   (boolean ignore-errors-p))
  ;; %OUTPUT-SUBTYPE% is only used with render functions
  (let ((%output-subtype%
	 (if output-subtype
	     output-subtype
	     ;; define default output subtype for LaTeX
	     (if (eq output-spec :latex)
		 :document))))
    (declare (special %output-subtype%))
    (if ignore-errors-p
	(handler-case
	    (render-document document :glossfile glossfile :glossp glossp :out-dir out-dir :output-spec output-spec :stream stream :style style) 
	  (error (e) 
	    (render-document 
	     (list
	      (list :document)
	      (otlb::string-to-charlist
	       ;; TRIVIAL-BACKTRACE or equivalent might be used here to supply more detail
	       (format nil "ERROR while rendering:~%~S" e))) 
	     :output-spec output-spec
	     :stream stream)))
	
	(render-document document :glossfile glossfile :glossp glossp :out-dir out-dir :output-spec output-spec :stream stream :style style))))

;; FIXME: use DOCUMENT-TO-STREAM -- it does everything this does except define the stream
(defun document-to-string (document output-spec &key glossfile glossp ignore-errors-p out-dir output-subtype style)
  "Return a string. IGNORE-ERRORS-P is either T or NIL. DOCUMENT should represent a complete otl document. OUTPUT-SPEC is one of :HTML, :LATEX, :TEXT, or a path or pathspec pointing to a parameter file."
  (declare (optimize (debug 3) (safety 3)) 
	   (boolean ignore-errors-p))
  ;; %OUTPUT-SUBTYPE% is only used with render functions
  (let ((%output-subtype%
	 (if output-subtype
	     output-subtype
	     ;; define default output subtype for LaTeX
	     (if (eq output-spec :latex)
		 :document))))
    (declare (special %output-subtype%))
    (if ignore-errors-p
	(handler-case
	    (with-output-to-string (s)
	      (render-document document :glossfile glossfile :glossp glossp :out-dir out-dir :output-spec output-spec :stream s :style style)) 
	  (error (e) 
	    (with-output-to-string (s)
	      (render-document 
	       (list
		(list :document)
		(otlb::string-to-charlist
		 ;; TRIVIAL-BACKTRACE or equivalent might be used here to supply more detail
		 (format nil "ERROR while rendering:~%~S" e))) 
	       :output-spec output-spec
	       :stream s))))
	(with-output-to-string (s)
	  (render-document document :glossfile glossfile :glossp glossp :out-dir out-dir :output-spec output-spec :stream s :style style)))))

(defun execute-pre-render-hooks ()
  (dolist (hook *pre-render-hooks*)
    (funcall hook)))

;; FIXME: are two different versions needed?
;; 1. called from SECTHEAD-OPENDOCUMENT
;;    -> %containing-obj-seq% and %containing-obj-seq-pointer% as externally established special variable
;; 
(defun obj-seq-to-string (obj-seq &optional custom)
  (with-output-to-string (s)
    (obj-seq-to-stream obj-seq s custom)))

(defun obj-seq-to-stream (obj-seq stream &optional custom)
  "OBJ-SEQ is an object sequence."
  ;; CUSTOM has the form (item-key fn) -- CUSTOM is used to specify a function which is called when an object with a specific ITEM-KEY is encountered 
  ;; -> can be used to ignore or transform an item in a specific context
  ;; - function FN should accept two arguments, an object sequence, and a stream
  
  ;; see comment for OBJ-SEQ-TO-STRING
  ;;(declare (special %containing-obj-seq% %containing-obj-seq-pointer%)) 
  (let ((%custom% custom)
	;(%containing-obj-seq% nil)
	;(%containing-obj-seq-pointer% nil)
	)
    (declare (special %custom%))
    (render-obj-seq obj-seq stream :custom custom)))

(defun post-render ()
  ;; see RENDER-DOCUMENT
  ;; it may be desirable to perform certain actions post-rendering, 
  ;; for example, for glossary generation for a LaTeX document, it may be desirable to generate a file containing \newglossentry entries
  (let ((post-render? (get-*render*-value :POST))) 
    (if post-render?
	(funcall post-render?))))

(defun render-document (document &key glossfile glossp out-dir output-spec stream style)
  "DOCUMENT is a document item."
  (render-item-standalone document
			  :glossfile glossfile
			  :glossp glossp
			  :out-dir out-dir
			  :output-spec output-spec
			  :stream stream
			  :style style))

(defun render-item-standalone (item &key glossfile glossp out-dir output-spec stream style)
  "IGNORE-ERRORS-P is either T or NIL. OBJECT-SEQUENCE is a document or other object sequence. OUT-DIR is a path corresponding to a directory (presumably the location where, if relevant, the output file is to be written). OUTPUT-SPEC specifies either a default parameter definition (e.g., :HTML, :LATEX, or :TEXT) or a parameter file (a path or pathspec)."
  (assert output-spec)
  (let ((%custom% nil)	; must be set prior to any RENDER-OBJ-SEQ call
	(%containing-obj-seq% nil)
	(%containing-obj-seq-pointer% nil)
	(%glossp% glossp)
	(%out-dir% out-dir)
	(%output-lang% (first (otlb:partree-item-list :out)))
	;; %~%: home directory ~ for the user
	(%~%
	 (user-homedir-pathname) ;(osicat:environment-variable "HOME")
	  ))
    (declare (special %containing-obj-seq% %containing-obj-seq-pointer% %custom% %glossp% %out-dir% %output-lang% %output-subtype% %~%))
    ;; reset various vars
    (render-reset :glossfile glossfile
		  :output-spec output-spec)
    ;; standalone rendering of item means 'current' object sequence is that sequence composed only of the current object
    ;(render-item item :stream stream) ; :style style
    (render-obj-seq (list item) stream :style style)
    (post-render)))

;; FIXME: is STREAM always a specific type of stream?
(defun render-obj-seq-standalone (object-sequence &key glossfile glossp out-dir output-spec output-subtype stream style)
  "IGNORE-ERRORS-P is either T or NIL. OBJECT-SEQUENCE is a document or other object sequence. OUT-DIR is a path corresponding to a directory (presumably the location where, if relevant, the output file is to be written). OUTPUT-SPEC specifies either a default parameter definition (e.g., :HTML, :LATEX, or :TEXT) or a parameter file (a path or pathspec)."
  (assert output-spec)
  (let ((%custom% nil)	; must be set prior to any RENDER-OBJ-SEQ call
	(%containing-obj-seq% nil)
	(%containing-obj-seq-pointer% nil)
	(%glossp% glossp)
	(%out-dir% out-dir)
	(%output-lang% (first (otlb:partree-item-list :out)))
	(%output-subtype% output-subtype)
	;; %~%: home directory ~ for the user
	(%~%
	 (user-homedir-pathname) ;(osicat:environment-variable "HOME")
	  )
	)
    (declare (special %containing-obj-seq% %containing-obj-seq-pointer% %custom% %glossp% %out-dir% %output-lang% %output-subtype% %~%))
    ;; reset various vars
    (render-reset :glossfile glossfile
		  :output-spec output-spec)
    (render-obj-seq object-sequence stream :style style)
    (post-render)))

(defun render-file-to-stream (input-file &optional stream)
  "Render INPUT-FILE to output stream STREAM."
  (render-document (dfile:file-to-string input-file)
		   :stream stream))

(defun render-item (item &key stream)
  "When an item is encountered during rendering, this should be called to ensure the item is rendered appropriately. Render item ITEM to stream STREAM. See also: RENDER-OBJECT-SEQUENCE and doc/internal-representation.txt."
  ;; %CUSTOM%: see OBJ-SEQ-TO-STRING
  (declare (special %custom% %containing-obj-seq% %containing-obj-seq-pointer% %obj-seq-pointer% %style%)
	   (otlb::item item))
  ;;(assert (itemp item))
  (when item
    (let ((item-specifier (otlb::get-item-spec item))
	  (object-sequence (otlb::get-item-obj-seq item)))
      ;(log:debug item-specifier object-sequence)
      (let ((item-spec-length (length item-specifier)))
	(let ((item-key (first item-specifier))
	      (item-id (when (> item-spec-length 1) (second item-specifier)))
	      (item-kvs (when (> item-spec-length 2) (subseq item-specifier 2))))
	  (cond ((eq (first %custom%) item-key)
		 ;;(format t "!** ~S~%" (with-output-to-string (s) (funcall (second %custom%) object-sequence s)))
		 (funcall (second %custom%) object-sequence stream))
		(t
		 (let ((key-fn (assoc item-key *render* :test 'eq)))
		   ;(log:debug key-fn)
		   (unless (consp key-fn)
		     (error (format nil "Unsupported item type (RENDER-ITEM):~%    item-key: ~S~%    item: ~S~%    key-fn: ~S~%" item-key item key-fn)))
		   ;; R-FN?:  if specified, a symbol pointing to a fn
		   ;; - fn should be able to access and alter encompassing object sequence
		   (let ((r-fn? (if key-fn (cdr key-fn))))
		     (cond (key-fn
			    ;; if key is specified but fn isn't specified, ignore item
			    (if r-fn?
				(funcall r-fn? object-sequence item-id item-kvs stream)))))))))))))

(defun render-obj-seq (object-sequence stream &key custom style)
  "Refer to doc/internal-representation.txt for a description of OBJECT-SEQUENCE."
  ;; %CONTAINING-OBJ-SEQ%: the object sequence which holds the current item which holds OBJECT-SEQUENCE
  ;; - facilitates linktarget setting id for an item
  ;; %CONTAINING-OBJ-SEQ-POINTER%: the current position of the current item in %CONTAINING-OBJ-SEQ%
  ;; %OBJ-SEQ%: the current object sequence OBJECT-SEQUENCE
  ;; %OBJ-SEQ-POINTER%: the current position in OBJECT-SEQUENCE

  ;; %custom%: must be defined prior to call to render-obj-seq
  (declare (special %containing-obj-seq% %containing-obj-seq-pointer% %custom%))
  ;; - OTLR::%OUTPUT-LANG% should be defined as a special variable
  ;; - establish context so an item can be aware of what is preceding and succeeding it
  (let ((%obj-seq% object-sequence)
	(%obj-seq-pointer%)
	(%style% style))
    (declare (special %obj-seq% %obj-seq-pointer% %style%))
    ;; if outer call had %custom% set, inherit that value
    (let ((%custom% (or custom %custom%)))
      (declare (special %custom%))
      ;;
      ;; before rendering object...
      ;; - identify special cases here (items which are modified by a successive item in the sequence)
      (transform-%obj-seq%)
      ;;
      ;; render items in sequence
      (setf %obj-seq-pointer% 0)
      ;; remove objects which are ignored anyways...
      ;; - makes downstream processing a little easier (see otl-html/render-footnote.lisp)
      (setf %obj-seq% (remove nil %obj-seq%)) ; (setf object-sequence (remove nil object-sequence))
      (let ((char-renderer? (get-*render*-value :CHAR)))
	(dolist (x %obj-seq%)
	  (cond ((characterp x)
		 ;; CHAR-RENDERER fn is different than other render fns in that it only accepts two args
		 (if char-renderer?
		     (funcall char-renderer? x stream)
		     (write-char x stream)))
		((consp x) 
		 (let ((%containing-obj-seq% %obj-seq%)
		       (%containing-obj-seq-pointer% %obj-seq-pointer%))
		   (declare (special %containing-obj-seq% %containing-obj-seq-pointer%))
		   (render-item x :stream stream)))
		;; NIL is a valid item -> ignore
		((not x))
		(t
		 (error (format nil "Unsupported item type (RENDER-OBJECT-SEQ): ~S~%" x)))) 
	  (incf %obj-seq-pointer%))))))

(defun render-reset (&key glossfile output-spec)
  (declare (special %out-dir%))
  (assert output-spec)
  (setf *footnote-stack* nil)
  (setf *gloss-stack* nil)
  ;; unless GLOSSFILE is explicitly specified use the default value
  (if glossfile
      (setf *gloss-filespec* glossfile)
      (if (gloss-filespec-generator*)
	  (setf *gloss-filespec*
		(funcall (gloss-filespec-generator*)))))
  (setf *toc-store* nil)
  (setf *toc-index* nil)
  (otlb::refresh-*transform* output-spec)
  (set-*render* output-spec)
  (set-*render2* output-spec)
  ;; (let ((render-vars?
  ;; 	 (first (otlb:aval
  ;; 		 (first (otlb:partree-item-list :out))
  ;; 		 *render-vars-trees*))))
  ;;   (if render-vars?
  ;; 	(setf *render-vars* render-vars?)
  ;; 	;; (error (format nil "*RENDER-VARS-TREES* doesn't contain a match for language specified by OUTPUT-SPEC ~A" output-spec))
  ;; 	))
  (execute-pre-render-hooks))

(defun render-to-file (parse-tree f overwrite-p output-spec)
  "F is a pathname object or a string."
  (if (and (dfile:file-or-directory-exists f) (not overwrite-p))
      (cerror "Overwrite ~S ?" (format nil "File ~S exists" f) f))
  (dfile:string-to-file (document-to-string parse-tree output-spec) f))

(defun set-*render* (&optional output-spec)
  "Set *RENDER* based on OUTPUT-SPEC or the specified output language returned by OTLB::OUTPUT-LANGUAGE."
  (let ((output-language (or output-spec
			     (otlb::output-language))))
    (let ((render?
	   (first (otlb:aval output-language *render-set*))))
      (if render?
	  (setf *render* render?)
	  (error (format nil "*RENDER-SET* doesn't contain a match for language specified by ~A" output-language))))))

(defun transform-%obj-seq% ()
  (declare (special %containing-obj-seq% %obj-seq% %obj-seq-pointer%))
  (setf %obj-seq-pointer% 0)
  (dolist (x %obj-seq%)
    ;; - X is a member of the object sequence %OBJ-SEQ%
    ;; - %CONTAINING-OBJ-SEQ% is the object sequence which holds, potentially, many items, including the item with object sequence %OBJ-SEQ% 

    ;; ignore characters
    (unless (characterp x)
      (if (consp x)
	  (transform-obj-seq-cons x)
	  (if x (error (format nil "Unsupported item type (RENDER-OBJECT-SEQUENCE): ~S~%" x)))))
    (incf %obj-seq-pointer%)))

(defun transform-obj-seq-cons (x)
  (declare (special %containing-obj-seq%))
  (let ((item-key (otlb:get-item-key x)))
    ;; modify id of container item when linktarget is encountered
    ;; if linktarget is at first position in document or subcomponent of document, the item to which linktarget points isn't clear (nonsensical) 
    (if (and %containing-obj-seq% 
	     (eq item-key :linktarget)
	     (otlb:itemp %containing-obj-seq%)) 
	(otlb:set-item-id %containing-obj-seq% (otlb:get-item-id x)))
    ;; a pre-rendering hook fn specifiable in *TRANSFORM* for each item type 
    ;; - fn handed object sequence and pointer and (potentially) modifies object sequence
    ;; - note that fn can be used for other purposes (e.g., for developing toc content)
    (let ((transform-fn? (otlb::get-transform-fn item-key)))
      (if transform-fn?
	  (let ((t-fn? (if transform-fn? (cdr transform-fn?))))
	    (if t-fn?
		(funcall t-fn?)))))))
