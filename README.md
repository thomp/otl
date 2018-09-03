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


## Getting started

otl depends on
- https://github.com/Ramarren/cl-parser-combinators[cl-parser-combinators]
- https://github.com/e-user/graylex[graylex]
[dfile]
[paper-sizes]


otl-html depends on
- cl-base64
- https://github.com/thomp/dxg[dxg]
- https://github.com/thomp/dxh[dxh]


## Use

Generate an otl representation of a document:
	 
    (ql:quickload :otl)
	(otlp::parse-reset :otl nil)	
    (otlp:parse-file infile)


Generating an HTML file corresponding to a text file:

    (ql:quickload :otl-html)
    (otl:parse-and-render-file "/path/to/mydoc.txt" :html :overwrite-p t)


## Building an executable

Run (from the shell, not the REPL):

    sbcl --eval "(progn (ql:quickload :otl) (ql:quickload :otl-html) )"
	
Then

	(sb-ext:save-lisp-and-die "/path/to/otl/build/otl" :executable t :toplevel 'otl::invoking-otl-from-shell)


The executable can be invoked with a variety of options. An example using the `otl` executable to build a section of a LaTeX document:

    /path/to/otl/build/otl --ignore-errors --output-spec :latex --subtype :section --glossfile glossentries.tex  lab02/30prelab.txt lab02/31introduction.txt lab02/32procedure.txt


## Parsing

`OTLP::*PARSE*` holds the parsing specification. `OTLP::*PARSE*` is defined using `OTLP::*PARSE-TREES*`. `OTLP::*PARSE-TREES*`, in turn, is populated by invoking `UPDATE-*PARSE-TREES*`. 

The default otl syntax is largely specified by `*PARSE-OTL*`.
