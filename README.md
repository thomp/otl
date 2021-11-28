# otl

*Generate markup from plain text*

---

Generate HTML, OpenDocument, LaTeX, or other markup from a plain-text input document. Supported document components include 

- comments
- footnotes
- a glossary
- images and figures 
- links
- lists
- mathematical expressions
- references
- sections
- tables
- a table of contents

Source documents can include Unicode characters such as ℃ or ←.


## Getting started

otl depends on

- https://github.com/Ramarren/cl-parser-combinators[cl-parser-combinators]
- https://github.com/e-user/graylex[graylex]
- https://gitlab.com/dtho/dfile[dfile]
- https://bitbucket.org/thomp1/paper-sizes[paper-sizes]

otl-html depends on

- cl-base64
- https://github.com/thomp/dxg[dxg]
- https://github.com/thomp/dxh[dxh]


## Use

Generate an otl representation of a document:
	 
    (ql:quickload :otl)
	(otlp::parse-reset :otl nil)
    (otlp:parse-file infile)


Generate an HTML file corresponding to a text file:

    (ql:quickload :otl-html)
    (otl:parse-and-render-file "/path/to/mydoc.txt" :html :overwrite-p t)


## Default markup

Examples of some of the default markup for otl are shown below.

	**this is in bold**

	//
	// this text will be ignored by otl
	//

	;;italicized content;;

	``this should be in monospace``


	The formula for water is H,,2,,O or H₂O.

	We represent "x cubed" as x^^3^^ or x³.


	|| Title of table
	|| Additional caption text
	a   | b   | c
	A   | B   | C
	--- | --- | ---
	foo | goo | loo
	asd | per | for



## Building an executable

Ensure support is included for output in the desired language(s). For example, for support for LaTeX and HTML, invoke SBCL from the shell:

    sbcl --eval "(progn (ql:quickload :otl) (ql:quickload :otl-html) (ql:quickload :otl-latex))"
	
Then generate the executable:

	(sb-ext:save-lisp-and-die "/path/to/otl" :executable t :toplevel 'otl::invoking-otl-from-shell)


The executable can be invoked with a variety of options. An example using the `otl` executable to build a section of a LaTeX document:

    /path/to/otl --ignore-errors --output-spec :latex --subtype :section ./0preface.txt ./1intro.txt ./2procedure.txt


## Parsing

`OTLP::*PARSE*` holds the current parsing specification. `OTLP::*PARSE*` is defined using `OTLP::*PARSE-TREES*`. `OTLP::*PARSE-TREES*`, in turn, is populated by invoking `UPDATE-*PARSE-TREES*`. 

The default otl syntax is largely specified by `*PARSE-OTL*`.


## Rendering

If necessary, load the appropriate system for the desired output format.

    > (ql:quickload :otl-latex)

`PARSE-AND-RENDER-FILE` is a convenience wrapper for parsing and then rendering.

    > (otl:parse-and-render-file "/path/to/mydoc.txt" :html :overwrite-p t)

`PARSE-DOCUMENT` and `PARSE-FILE` are used to parse a string or the content of a file, respectively.

	> (otlp::parse-document "Happy to you... ;;you;; __you__ **you**")
	((:DOCUMENT NIL :TITLE "" :PAGE-SIZE
	  #S(CL-PAPER-SIZES::PAPER-SIZE
		 :NICKNAME :LETTER
		 :WIDTH (215.9 . :MM)
		 :LENGTH (279.4 . :MM)
		 :ORGANIZATION :ASME
		 :SPECIFICATION NIL)
	  :DATE-LAST-MODIFIED 0 :DATE-CREATED 0 :AUTHOR "")
	 (((:DOCTOP))
	  ((:TEXTBLOCK NIL :TABLEVEL 0)
	   (((:TEXTLINE NIL :TABLEVEL 0)
		 (#\H #\a #\p #\p #\y #\  #\t #\o #\  #\y #\o #\u #\. #\. #\. #\
		  ((:ITALIC) (#\y #\o #\u)) #\  ((:UNDERLINE) (#\y #\o #\u)) #\
		  ((:BOLD) (#\y #\o #\u))))))
	  ((:FOOTNOTES NIL NIL)) NIL ((:DOCBOTTOM))))

## Invoking the shell command

otl --query <query text>
otl [options] file1 ... fileN


For the first form of the command, <query text> may be one of

input
        output a list of valid 'input-spec' values
output
        output a list of valid 'output-spec' values
styles
        output a list of available styles, each as a style name on a separate line


For the second form of the command, file1 through fileN are processed as input files. [options] may include any of

--gloss
        include glossary

--glossfile gfile
        write glossary data to gfile and include glossary term markup

--help
        output this message

--no-overwrite
        don't overwrite preexisting output file

--outfile FILE
        specify output filename (note that behavior is guaranteed only in the case where a single file is parsed); if --stitch is true, this option is ignored; if --stitch isn't true and if --outfile isn't specified, then the output file(s), by default, are written to the same directory as the input files and have the same file names as the input files, but with the suffix corresponding to the output-spec provided

--output-spec output-spec
        specify a 'output-spec' value (':text', ':latex', ':html' (the default), or a path corresponding to a parameter file)

--papersize PAPER-SIZE
        PAPER-SIZE is a CLPS paper size nickname sans colon ('A4' or 'letter')

--sexp
        print the sexp which would be otherwise executed and then halt

--stitch
        process, as the equivalent of a single file, the concatenatation of file1 through fileN

--style stylename
        request that style specified by stylename be used
