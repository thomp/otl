(in-package :otl-base)
;;;
;;; latex.lisp: utilities to process LaTeX
;;;

;; imagemagick convert for pdf->png
(defun convert-pdf->png (pdffile pngfile &optional (density "600"))
  (convert-pdf->png-sbcl pdffile pngfile density))

;; DENSITY: string representation of DPI
(defun convert-pdf->png-sbcl (pdffile pngfile &optional (density "600"))
  (sb-ext:run-program "convert"
		      (list "-density"
			    (concatenate 'string density "x" density)
			    (if (typep pdffile 'pathname)
				(namestring pdffile)
				pdffile)
			    "-quality"
			    "90"
			    (if (typep pngfile 'pathname)
				(namestring pngfile)
				pdffile))
		      :input t
		      :output *standard-output*
		      :search t))


(defun convert-pdf->svg (pdffile svgfile)
  (convert-pdf->svg-sbcl pdffile svgfile))

(defun convert-pdf->svg-sbcl (pdffile svgfile)
  (sb-ext:run-program "pdf2svg"
		      (list (if (typep pdffile 'pathname)
				(namestring pdffile)
				pdffile)
			    (if (typep svgfile 'pathname)
				(namestring svgfile)
				pdffile))
		      :input t
		      :output *standard-output*
		      :search t))

(defun pdfcrop (infile outfile)
  "Run pdflatex on file INFILE and write cropped version to file OUTFILE."
  (pdfcrop-sbcl infile outfile))

(defun pdflatex (file)
  "Run pdflatex on file FILE."
  (pdflatex-sbcl file))

;; (defun pdflatex-trivial-shell (file-tex)
;;   ;; generate the corresponding pdf
;;   (trivial-shell:shell-command 
;;    (concatenate 'string "pdflatex --halt-on-error " 
;; 		(if (typep file-tex 'pathname)
;; 		    (namestring file-tex)
;; 		    file-tex)
;; 		;; pdflatex automatically writes to foo.pdf
;; 		;;" > " (if (typep outfile-pdf 'pathname) (namestring outfile-pdf) outfile-pdf)
;; 		)))

(defun pdfcrop-sbcl (infile outfile)
  (sb-ext:run-program "pdfcrop" 
		      (list "--clip" 
			    (if (typep infile 'pathname)
				(namestring infile)
				infile)
			    (if (typep outfile 'pathname)
				(namestring outfile)
				outfile))
		      :input t
		      :output *standard-output*
		      :search t))

(defun pdflatex-sbcl (file-tex)
  ;; generate the corresponding pdf
  (let ((file-tex-path (if (typep file-tex 'pathname)
			   file-tex
			   (pathname file-tex))))
    (sb-ext:run-program "pdflatex" 
			(list "-halt-on-error" 
			      ;; put output file in the 'right' place
			      "-output-directory"
			      ;; this STRING-TRIM-RIGHT is a kludge for a pdftex bug: if handed /tmp/foo/ here, then pdftex writes /tmp/foo//goo.pdf instead of /tmp/foo/goo.pdf
			      (string-right-trim
			       "/" ;'(#\/)
			       (namestring (make-pathname :directory (pathname-directory file-tex-path))))
			      (if (typep file-tex 'pathname)
				  (namestring file-tex)
				  file-tex)
			      ;; pdflatex automatically writes to foo.pdf
			      ;;" > " (if (typep outfile-pdf 'pathname) (namestring outfile-pdf) outfile-pdf)
			      )
			:input t
			:output *standard-output*
			:search t)))
